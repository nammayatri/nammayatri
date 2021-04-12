module Mobility.NearestDrivers (spec) where

import Beckn.Types.Id
import Beckn.Types.Storage.Vehicle
import EulerHS.Prelude
import qualified "beckn-transport" Storage.Queries.Person as Q (getNearestDrivers)
import Test.Hspec
import "beckn-transport" Types.API.Location (LatLong (..))
import Utils

spec :: Spec
spec =
  describe "getNearestDrivers function" $ do
    it "Test ordering" testOrder
    it "Test radius filtration" testInRadius
    it "Test outside radius filtration" testNotInRadius

testOrder :: IO ()
testOrder = do
  rez <- runTransportFlow $ Q.getNearestDrivers pickupPoint 5000 org1 SUV <&> map (getId . fst)
  rez `shouldBe` ["002093df-4f7c-440f-ba-closest_driver", "001093df-4f7c-440f-b-furthest_driver"]

testInRadius :: IO ()
testInRadius = do
  rez <- runTransportFlow $ Q.getNearestDrivers pickupPoint 800 org1 SUV <&> map (getId . fst)
  rez `shouldBe` ["002093df-4f7c-440f-ba-closest_driver"]

testNotInRadius :: IO ()
testNotInRadius = do
  rez <- runTransportFlow $ Q.getNearestDrivers pickupPoint 0 org1 SUV <&> map (getId . fst)
  rez `shouldBe` []

pickupPoint :: LatLong
pickupPoint = LatLong 12.994927 77.596386

org1 :: forall k. Id k
org1 = Id "7f7896dd-787e-4a0b-8675-e9e6fe93bb8f"