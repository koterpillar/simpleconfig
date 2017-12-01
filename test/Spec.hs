{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Control.Lens

import Data.Text

import Generics.Deriving.Monoid

import GHC.Generics

import Config.Simple

data ConfigF k = Config
  { _address :: ConfigItem Text k
  , _dryRun :: ConfigBool k
  , _widgets :: ConfigSet Text k
  } deriving (Generic)

type PartialConfig = Partial ConfigF

deriving instance Eq PartialConfig

deriving instance Show PartialConfig

instance Monoid PartialConfig where
  mempty = memptydefault
  mappend = mappenddefault

type Config = Complete ConfigF

Config (LensFor address) (LensFor dryRun) (LensFor widgets) = configLens

main :: IO ()
main = do
  let partial = mempty & address <>~ pure "Silverpond"
  pure ()
