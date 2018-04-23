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
import Control.Monad

import Data.Foldable
import Data.Either.Validation
import Data.Monoid (Last(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Generics.Deriving.Monoid

import GHC.Generics

import Config.Simple

data ConfigF k = Config
  { _address :: ConfigLast Text k
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

deriving instance Eq Config

deriving instance Show Config

Config (LensFor address) (LensFor dryRun) (LensFor widgets) = configLens

main :: IO ()
main = do
  let config' =
        mempty & (address <>~ pure "Silverpond") & (dryRun <>~ Any True) &
        (widgets <>~ Set.singleton "blah") &
        (address <>~ pure "SEEK")
  print config'
  let (Success config) = fromPartialConfig config'
  Text.putStrLn $ "Address = " <> config ^. address
  when (config ^. dryRun) $ Text.putStrLn "Dry run"
  for_ (config ^. widgets) $ \widget -> Text.putStrLn $ "Widget = " <> widget
  let incompleteConfig' = mempty & (dryRun <>~ Any True)
  let (Failure fields) = fromPartialConfig incompleteConfig'
  for_ fields Text.putStrLn
