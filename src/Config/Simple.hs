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
  , ConfigItem
  , ConfigSet
  , Partial
  , Complete
  , LensFor(..)
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

data CLensFor c

newtype LensFor s a =
  LensFor (Lens' s a)

type family ConfigItem a k where
  ConfigItem a CPartial = Last a
  ConfigItem a CComplete = a
  ConfigItem a (CLensFor root) = LensFor root (Maybe a)

type family ConfigBool k where
  ConfigBool CPartial = Any
  ConfigBool CComplete = Bool
  ConfigBool (CLensFor root) = LensFor root Bool

type family ConfigSet a k where
  ConfigSet a CPartial = Set a
  ConfigSet a CComplete = Set a
  ConfigSet a (CLensFor root) = LensFor root (Set a)

type Partial config = config CPartial

type Complete config = config CComplete

type PartialLens config = config (CLensFor (Partial config))

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

configLens ::
     forall config.
     ( Generic (Partial config)
     , Generic (PartialLens config)
     , GLensFor (Partial config) (Rep (Partial config)) (Rep (PartialLens config))
     )
  => PartialLens config
configLens = G.to $ gToLensFor rootLens
  where
    rootLens ::
         forall x. Generic (Partial config)
      => Lens' (Partial config) (Rep (Partial config) x)
    rootLens = G.generic

class GLensFor root rep repLens where
  gToLensFor :: Lens' root (rep x) -> repLens x

instance GLensFor root r rl => GLensFor root (D1 m r) (D1 m rl) where
  gToLensFor rootLens = M1 $ gToLensFor (rootLens . G._M1)

instance GLensFor root r rl => GLensFor root (C1 m r) (C1 m rl) where
  gToLensFor rootLens = M1 $ gToLensFor (rootLens . G._M1)

instance (GLensFor root ra ral, GLensFor root rb rbl) => GLensFor root (ra :*: rb) (ral :*: rbl) where
  gToLensFor rootLens = gToLensFor (rootLens . _1) :*: gToLensFor (rootLens . _2)

instance GLensFor root r rl => GLensFor root (S1 m r) (S1 m rl) where
  gToLensFor rootLens = M1 $ gToLensFor (rootLens . G._M1)

instance GLensFor root (Rec0 (Last x)) (Rec0 (LensFor root (Maybe x))) where
  gToLensFor rootLens = K1 $ LensFor $ rootLens . G._K1 . iso getLast Last

instance GLensFor root (Rec0 (Set x)) (Rec0 (LensFor root (Set x))) where
  gToLensFor rootLens = K1 $ LensFor $ rootLens . G._K1

instance GLensFor root (Rec0 Any) (Rec0 (LensFor root Bool)) where
  gToLensFor rootLens = K1 $ LensFor $ rootLens . G._K1 . iso getAny Any
