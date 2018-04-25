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

import Data.Either.Validation
import Data.Foldable
import Data.Maybe
import Data.Monoid (Last(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Generics.Deriving.Monoid

import GHC.Generics

import Test.Hspec

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

Config (AccumFor address') (AccumFor dryRun') (AccumFor widgets') =
  configLensPartial

isSuccess :: Validation e a -> Bool
isSuccess (Success _) = True
isSuccess (Failure _) = False

main :: IO ()
main =
  hspec $
  describe "fromPartialConfig" $ do
    context "with a complete config" $ do
      let config' =
            mempty & address' "Silverpond" & dryRun' True & widgets' "blah" &
            address' "SEEK"
      let result = fromPartialConfig config'
      it "should succeed" $ result `shouldSatisfy` isSuccess
      let (Success config) = fromPartialConfig config'
      it "should override Last members with later assignments" $
        config ^. address `shouldBe` "SEEK"
      it "should turn on Any members" $ config ^. dryRun `shouldBe` True
      it "should record set members" $
        config ^. widgets `shouldBe` Set.fromList ["blah"]
    context "with incomplete config" $ do
      let config' = mempty & dryRun' True
      let result = fromPartialConfig config'
      it "should return a failure" $ result `shouldNotSatisfy` isSuccess
      let (Failure fields) = fromPartialConfig config'
      it "should return missing fields" $ fields `shouldBe` ["_address"]
