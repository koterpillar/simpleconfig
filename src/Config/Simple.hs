{-|
Module: Config.Simple
Description: Simple configuration data types

Functions for declaring a configuration data type.
-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , fromPartialConfig
  ) where

import Control.Lens

import Data.Either.Validation
import Data.Monoid (Any(..), Last(..))
import Data.Set (Set)
import Data.String (IsString(..))

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
  ConfigLast a (CLensFor k root) = LensFor root (ConfigLast a k)

type family ConfigBool k where
  ConfigBool CPartial = Any
  ConfigBool CComplete = Bool
  ConfigBool (CLensFor k root) = LensFor root (ConfigBool k)

type family ConfigSet a k where
  ConfigSet a CPartial = Set a
  ConfigSet a CComplete = Set a
  ConfigSet a (CLensFor k root) = LensFor root (ConfigSet a k)

type Partial config = config CPartial

type Complete config = config CComplete

type LensConfig k config = config (CLensFor k (config k))

fromPartialConfig ::
     ( Generic (Partial config)
     , Generic (Complete config)
     , GFromPartialConfig (Rep (Partial config)) (Rep (Complete config))
     , IsString text
     )
  => Partial config
  -> Validation [text] (Complete config)
fromPartialConfig = fmap G.to . gFromPartialConfig . G.from

class GFromPartialConfig (repPartial :: * -> *) (repComplete :: * -> *) where
  gFromPartialConfig :: IsString text => repPartial x -> Validation [text] (repComplete x)

instance GFromPartialConfig fp fc =>
         GFromPartialConfig (D1 m fp) (D1 m fc) where
  gFromPartialConfig (M1 x) = M1 <$> gFromPartialConfig x

instance GFromPartialConfig fp fc =>
         GFromPartialConfig (C1 m fp) (C1 m fc) where
  gFromPartialConfig (M1 x) = M1 <$> gFromPartialConfig x

instance (GFromPartialConfig ap ac, GFromPartialConfig bp bc) =>
         GFromPartialConfig (ap :*: bp) (ac :*: bc) where
  gFromPartialConfig (a :*: b) =
    liftA2 (:*:) (gFromPartialConfig a) (gFromPartialConfig b)

instance (GFromPartialConfigMaybe fp fc, Selector m) =>
         GFromPartialConfig (S1 m fp) (S1 m fc) where
  gFromPartialConfig v@(M1 x) =
    fmap M1 $
    case gFromPartialConfigMaybe x of
      Just r -> Success r
      Nothing -> Failure [fromString $ selName v]

class GFromPartialConfigMaybe (repPartial :: * -> *) (repComplete :: * -> *) where
  gFromPartialConfigMaybe :: repPartial x -> Maybe (repComplete x)

class GFromPartialConfigMember partial complete where
  gFromPartialConfigMember :: partial -> Maybe complete

instance GFromPartialConfigMember partial complete =>
         GFromPartialConfigMaybe (Rec0 partial) (Rec0 complete) where
  gFromPartialConfigMaybe (K1 a) = K1 <$> gFromPartialConfigMember a

instance GFromPartialConfigMember Any Bool where
  gFromPartialConfigMember = Just . getAny

instance GFromPartialConfigMember (Set a) (Set a) where
  gFromPartialConfigMember = Just

instance GFromPartialConfigMember (Last a) a where
  gFromPartialConfigMember = getLast

configLens ::
     forall config k.
     ( Generic (config k)
     , Generic (LensConfig k config)
     , GLensFor (config k) (Rep (config k)) (Rep (LensConfig k config))
     )
  => LensConfig k config
configLens = G.to $ gToLensFor rootLens
  where
    rootLens ::
         forall x. Generic (config k)
      => Lens' (config k) (Rep (config k) x)
    rootLens = G.generic

class GLensFor root rep repLens where
  gToLensFor :: Lens' root (rep x) -> repLens x

instance GLensFor root r rl => GLensFor root (M1 i m r) (M1 i m rl) where
  gToLensFor rootLens = M1 $ gToLensFor (rootLens . G._M1)

instance (GLensFor root ra ral, GLensFor root rb rbl) =>
         GLensFor root (ra :*: rb) (ral :*: rbl) where
  gToLensFor rootLens =
    gToLensFor (rootLens . _1) :*: gToLensFor (rootLens . _2)

instance GLensFor root (Rec0 x) (Rec0 (LensFor root x)) where
  gToLensFor rootLens = K1 $ LensFor $ rootLens . G._K1
