module Mobility.NearestDrivers (spec) where

import qualified "beckn-transport" App.Types as BecknTransport
import Beckn.Types.Id
import Beckn.Types.Storage.Vehicle
import Beckn.Utils.Dhall (readDhallConfig)
import EulerHS.Prelude
import qualified "beckn-transport" Storage.Queries.Person as Q (getNearestDrivers)
import Test.Hspec
import "beckn-transport" Types.API.Location (LatLong (..))
import Utils

spec :: Spec
spec = do
  (appEnv :: BecknTransport.AppEnv) <- runIO $ readDhallConfig "../dhall-configs/dev/beckn-transport.dhall"
  describe "getNearestDrivers function" $ do
    it "Test ordering" $ testOrder appEnv
    it "Test radius filtration" $ testInRadius appEnv
    it "Test outside radius filtration" $ testNotInRadius appEnv

testOrder :: BecknTransport.AppEnv -> IO ()
testOrder appEnv = do
  res <-
    runTransportFlow "Test ordering" appEnv $
      Q.getNearestDrivers pickupPoint 5000 org1 SUV <&> map (getId . fst)
  res `shouldBe` ["002093df-4f7c-440f-ba-closest_driver", "001093df-4f7c-440f-b-furthest_driver"]

testInRadius :: BecknTransport.AppEnv -> IO ()
testInRadius appEnv = do
  res <-
    runTransportFlow "Test readius filtration" appEnv $
      Q.getNearestDrivers pickupPoint 800 org1 SUV <&> map (getId . fst)
  res `shouldBe` ["002093df-4f7c-440f-ba-closest_driver"]

testNotInRadius :: BecknTransport.AppEnv -> IO ()
testNotInRadius appEnv = do
  res <-
    runTransportFlow "Test outside radius filtration" appEnv $
      Q.getNearestDrivers pickupPoint 0 org1 SUV <&> map (getId . fst)
  res `shouldBe` []

pickupPoint :: LatLong
pickupPoint = LatLong 12.994927 77.596386

org1 :: forall k. Id k
org1 = Id "7f7896dd-787e-4a0b-8675-e9e6fe93bb8f"
