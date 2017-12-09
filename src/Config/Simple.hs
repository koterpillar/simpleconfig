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
  , configLens
  , configLensPartial
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
         GFromPartialConfig (M1 i m fp) (M1 i m fc) where
  gFromPartialConfig (M1 x) = M1 <$> gFromPartialConfig x

instance (GFromPartialConfig ap ac, GFromPartialConfig bp bc) =>
         GFromPartialConfig (ap :*: bp) (ac :*: bc) where
  gFromPartialConfig (a :*: b) =
    liftA2 (:*:) (gFromPartialConfig a) (gFromPartialConfig b)

class GFromPartialConfigMember partial complete where
  gFromPartialConfigMember :: partial -> Maybe complete

instance GFromPartialConfigMember partial complete =>
         GFromPartialConfig (Rec0 partial) (Rec0 complete) where
  gFromPartialConfig (K1 a) = K1 <$> gFromPartialConfigMember a

instance GFromPartialConfigMember Any Bool where
  gFromPartialConfigMember = Just . getAny

instance GFromPartialConfigMember (Set a) (Set a) where
  gFromPartialConfigMember = Just

instance GFromPartialConfigMember (Last a) a where
  gFromPartialConfigMember = getLast

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

instance GLensFor k root r rl => GLensFor k root (M1 i m r) (M1 i m rl) where
  gToLensFor pk rootLens = M1 $ gToLensFor pk (rootLens . G._M1)

instance (GLensFor k root ra ral, GLensFor k root rb rbl) =>
         GLensFor k root (ra :*: rb) (ral :*: rbl) where
  gToLensFor pk rootLens =
    gToLensFor pk (rootLens . _1) :*: gToLensFor pk (rootLens . _2)

instance GLensForMember k x y =>
         GLensFor k root (Rec0 x) (Rec0 (LensFor root y)) where
  gToLensFor pk rootLens = K1 $ LensFor $ rootLens . G._K1 . gLensLeaf pk

class GLensForMember k x y where
  gLensLeaf :: proxy k -> Lens' x y

instance GLensForMember CPartial (Last a) (Maybe a) where
  gLensLeaf _ = iso getLast Last

instance GLensForMember CPartial Any Bool where
  gLensLeaf _ = iso getAny Any

instance GLensForMember CPartial (Set a) (Set a) where
  gLensLeaf _ = id

instance GLensForMember CComplete a a where
  gLensLeaf _ = id
