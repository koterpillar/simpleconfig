{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Config.Simple
  ( ConfigItem
  , ConfigBool
  , ConfigSet
  , Partial
  , Complete
  , fromPartialConfig
  ) where

import Data.Monoid (Any(..), Last(..))
import Data.Set (Set)

import GHC.Generics

import Control.Applicative

data CPartial

data CComplete

type family ConfigItem a k where
  ConfigItem a CPartial = Last a
  ConfigItem a CComplete = a

type family ConfigBool k where
  ConfigBool CPartial = Any
  ConfigBool CComplete = Bool

type family ConfigSet a k where
  ConfigSet a CPartial = Set a
  ConfigSet a CComplete = Set a

type Partial config = config CPartial

type Complete config = config CComplete

fromPartialConfig ::
     ( Generic (Partial config)
     , Generic (Complete config)
     , FromPartialConfig (Rep (Partial config)) (Rep (Complete config))
     )
  => Partial config
  -> Maybe (Complete config)
fromPartialConfig = fmap to . fromPartialConfigRep . from

class FromPartialConfig (repPartial :: * -> *) (repComplete :: * -> *) where
  fromPartialConfigRep :: repPartial x -> Maybe (repComplete x)

instance FromPartialConfig fp fc => FromPartialConfig (D1 m fp) (D1 m fc) where
  fromPartialConfigRep (M1 x) = M1 <$> fromPartialConfigRep x

instance FromPartialConfig fp fc => FromPartialConfig (C1 m fp) (C1 m fc) where
  fromPartialConfigRep (M1 x) = M1 <$> fromPartialConfigRep x

instance FromPartialConfig fp fc => FromPartialConfig (S1 m fp) (S1 m fc) where
  fromPartialConfigRep (M1 x) = M1 <$> fromPartialConfigRep x

instance (FromPartialConfig ap ac, FromPartialConfig bp bc) =>
         FromPartialConfig (ap :*: bp) (ac :*: bc) where
  fromPartialConfigRep (a :*: b) =
    liftA2 (:*:) (fromPartialConfigRep a) (fromPartialConfigRep b)

instance FromPartialConfig (Rec0 Any) (Rec0 Bool) where
  fromPartialConfigRep (K1 a) = Just $ K1 $ getAny a

instance FromPartialConfig (Rec0 (Set a)) (Rec0 (Set a)) where
  fromPartialConfigRep (K1 a) = Just $ K1 a

instance FromPartialConfig (Rec0 (Last a)) (Rec0 a) where
  fromPartialConfigRep (K1 a) = K1 <$> getLast a
