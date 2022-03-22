module Mobility.NearestDrivers (spec) where

import qualified "beckn-transport" App.Types as BecknTransport
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Flow (FlowR)
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import Domain.Types.Vehicle
import EulerHS.Prelude
import qualified "beckn-transport" Storage.Queries.DriverInformation as Q
import qualified "beckn-transport" Storage.Queries.Person as Q
import Test.Hspec
import Utils

spec :: Spec
spec = do
  describe "getNearestDrivers function"
    . beforeAll_ (runTransporterFlow "Turn on drivers" $ setDriversActive True)
    . afterAll_ (runTransporterFlow "Turn off drivers." $ setDriversActive False)
    $ do
      it "Test ordering" testOrder
      it "Test radius filtration" testInRadius
      it "Test outside radius filtration" testNotInRadius

testOrder :: IO ()
testOrder = do
  res <-
    runTransporterFlow "Test ordering" $
      Q.getNearestDrivers pickupPoint 5000 org1 (Just SUV) <&> getIds
  res `shouldBe` [closestDriver, furthestDriver]

testInRadius :: IO ()
testInRadius = do
  res <-
    runTransporterFlow "Test readius filtration" $
      Q.getNearestDrivers pickupPoint 800 org1 (Just SUV) <&> getIds
  res `shouldBe` [closestDriver]

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

furthestDriver :: Text
furthestDriver = "001093df-4f7c-440f-b-furthest_driver"

closestDriver :: Text
closestDriver = "002093df-4f7c-440f-ba-closest_driver"

otherDriver :: Text
otherDriver = "003093df-4f7c-440f-bada-other_driver"

setDriversActive :: Bool -> FlowR BecknTransport.AppEnv ()
setDriversActive isActive = Esq.runTransaction $ do
  Q.updateActivity (Id furthestDriver) isActive
  Q.updateActivity (Id closestDriver) isActive
  Q.updateActivity (Id otherDriver) isActive
