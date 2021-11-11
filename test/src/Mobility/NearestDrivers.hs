module Mobility.NearestDrivers (spec) where

import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import EulerHS.Prelude
import qualified "beckn-transport" Storage.Queries.Person as Q
import Test.Hspec
import Types.Storage.Vehicle
import Utils

spec :: Spec
spec = do
  describe "getNearestDrivers function" $ do
    it "Test ordering" testOrder
    it "Test radius filtration" testInRadius
    it "Test outside radius filtration" testNotInRadius

testOrder :: IO ()
testOrder = do
  res <-
    runTransporterFlow "Test ordering" $
      Q.getNearestDrivers pickupPoint 5000 org1 (Just SUV) <&> getIds
  res `shouldBe` ["002093df-4f7c-440f-ba-closest_driver", "001093df-4f7c-440f-b-furthest_driver"]

testInRadius :: IO ()
testInRadius = do
  res <-
    runTransporterFlow "Test readius filtration" $
      Q.getNearestDrivers pickupPoint 800 org1 (Just SUV) <&> getIds
  res `shouldBe` ["002093df-4f7c-440f-ba-closest_driver"]

testNotInRadius :: IO ()
testNotInRadius = do
  res <-
    runTransporterFlow "Test outside radius filtration" $
      Q.getNearestDrivers pickupPoint 0 org1 (Just SUV) <&> getIds
  res `shouldBe` []

getIds :: [Q.DriverPoolResult] -> [Text]
getIds = map (getId . (.driverId))

pickupPoint :: LatLong
pickupPoint = LatLong 12.994927 77.596386

org1 :: forall k. Id k
org1 = Id "7f7896dd-787e-4a0b-8675-e9e6fe93bb8f"
