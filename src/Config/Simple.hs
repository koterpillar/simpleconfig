{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Config.Simple
  ( ConfigBool
  , ConfigLast
  , ConfigSet
  , Partial
  , Complete
  , LensFor(..)
  , configLensPartial
  , configLens
  , fromPartialConfig
  ) where

import Control.Lens

import Data.Monoid (Any(..), Last(..))
import Data.Set (Set)

import GHC.Generics
import qualified GHC.Generics as G
import qualified GHC.Generics.Lens as G

import Control.Applicative

data CPartial

data CComplete

data CLensFor k c

newtype LensFor s a =
  LensFor (Lens' s a)

type family ConfigLast a k where
  ConfigLast a CPartial = Last a
  ConfigLast a CComplete = a
  ConfigLast a (CLensFor CPartial root) = LensFor root (Maybe a)
  ConfigLast a (CLensFor CComplete root) = LensFor root a

type family ConfigBool k where
  ConfigBool CPartial = Any
  ConfigBool CComplete = Bool
  ConfigBool (CLensFor CPartial root) = LensFor root Bool
  ConfigBool (CLensFor CComplete root) = LensFor root Bool

type family ConfigSet a k where
  ConfigSet a CPartial = Set a
  ConfigSet a CComplete = Set a
  ConfigSet a (CLensFor CPartial root) = LensFor root (Set a)
  ConfigSet a (CLensFor CComplete root) = LensFor root (Set a)

type Partial config = config CPartial

type Complete config = config CComplete

type LensConfig k config = config (CLensFor k (config k))

type PartialLens config = LensConfig CPartial config

type CompleteLens config = LensConfig CComplete config

fromPartialConfig ::
     ( Generic (Partial config)
     , Generic (Complete config)
     , GFromPartialConfig (Rep (Partial config)) (Rep (Complete config))
     )
  => Partial config
  -> Maybe (Complete config)
fromPartialConfig = fmap G.to . gFromPartialConfig . G.from

class GFromPartialConfig (repPartial :: * -> *) (repComplete :: * -> *) where
  gFromPartialConfig :: repPartial x -> Maybe (repComplete x)

instance GFromPartialConfig fp fc =>
         GFromPartialConfig (D1 m fp) (D1 m fc) where
  gFromPartialConfig (M1 x) = M1 <$> gFromPartialConfig x

instance GFromPartialConfig fp fc =>
         GFromPartialConfig (C1 m fp) (C1 m fc) where
  gFromPartialConfig (M1 x) = M1 <$> gFromPartialConfig x

instance GFromPartialConfig fp fc =>
         GFromPartialConfig (S1 m fp) (S1 m fc) where
  gFromPartialConfig (M1 x) = M1 <$> gFromPartialConfig x

instance (GFromPartialConfig ap ac, GFromPartialConfig bp bc) =>
         GFromPartialConfig (ap :*: bp) (ac :*: bc) where
  gFromPartialConfig (a :*: b) =
    liftA2 (:*:) (gFromPartialConfig a) (gFromPartialConfig b)

instance GFromPartialConfig (Rec0 Any) (Rec0 Bool) where
  gFromPartialConfig (K1 a) = Just $ K1 $ getAny a

instance GFromPartialConfig (Rec0 (Set a)) (Rec0 (Set a)) where
  gFromPartialConfig (K1 a) = Just $ K1 a

instance GFromPartialConfig (Rec0 (Last a)) (Rec0 a) where
  gFromPartialConfig (K1 a) = K1 <$> getLast a

configLens' ::
     forall config k proxy.
     ( Generic (config k)
     , Generic (LensConfig k config)
     , GLensFor k (config k) (Rep (config k)) (Rep (LensConfig k config))
     )
  => proxy k
  -> LensConfig k config
configLens' pk = G.to $ gToLensFor pk rootLens
  where
    rootLens ::
         forall x. Generic (config k)
      => Lens' (config k) (Rep (config k) x)
    rootLens = G.generic

configLensPartial ::
     forall config proxy.
     ( Generic (config CPartial)
     , Generic (LensConfig CPartial config)
     , GLensFor CPartial (config CPartial) (Rep (config CPartial)) (Rep (LensConfig CPartial config))
     )
  => LensConfig CPartial config
configLensPartial = configLens' (Nothing :: Maybe CPartial)

configLens ::
     forall config proxy.
     ( Generic (config CComplete)
     , Generic (LensConfig CComplete config)
     , GLensFor CComplete (config CComplete) (Rep (config CComplete)) (Rep (LensConfig CComplete config))
     )
  => LensConfig CComplete config
configLens = configLens' (Nothing :: Maybe CComplete)

class GLensFor k root rep repLens where
  gToLensFor :: proxy k -> Lens' root (rep x) -> repLens x

instance GLensFor k root r rl => GLensFor k root (D1 m r) (D1 m rl) where
  gToLensFor pk rootLens = M1 $ gToLensFor pk (rootLens . G._M1)

instance GLensFor k root r rl => GLensFor k root (C1 m r) (C1 m rl) where
  gToLensFor pk rootLens = M1 $ gToLensFor pk (rootLens . G._M1)

instance (GLensFor k root ra ral, GLensFor k root rb rbl) =>
         GLensFor k root (ra :*: rb) (ral :*: rbl) where
  gToLensFor pk rootLens =
    gToLensFor pk (rootLens . _1) :*: gToLensFor pk (rootLens . _2)

instance GLensFor k root r rl => GLensFor k root (S1 m r) (S1 m rl) where
  gToLensFor pk rootLens = M1 $ gToLensFor pk (rootLens . G._M1)

instance GLensLeaf k x y =>
         GLensFor k root (Rec0 x) (Rec0 (LensFor root y)) where
  gToLensFor pk rootLens = K1 $ LensFor $ rootLens . G._K1 . gLensLeaf pk

class GLensLeaf k x y where
  gLensLeaf :: proxy k -> Lens' x y

instance GLensLeaf CPartial (Last a) (Maybe a) where
  gLensLeaf _ = iso getLast Last

instance GLensLeaf CPartial Any Bool where
  gLensLeaf _ = iso getAny Any

instance GLensLeaf CPartial (Set a) (Set a) where
  gLensLeaf _ = id

instance GLensLeaf CComplete a a where
  gLensLeaf _ = id
