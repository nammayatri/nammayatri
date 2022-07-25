module Mobility.ARDU.NearestDrivers (spec) where

import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Flow (FlowR)
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import qualified "driver-offer-bpp" Environment as ARDUEnv
import EulerHS.Prelude
import qualified "driver-offer-bpp" Storage.Queries.DriverInformation as Q
import qualified "driver-offer-bpp" Storage.Queries.Person as Q
import Test.Hspec
import Utils

spec :: Spec
spec = do
  describe "getNearestDrivers function"
    . beforeAll_ (runARDUFlow "Turn on drivers" $ setDriversActive True)
    . afterAll_ (runARDUFlow "Turn off drivers." $ setDriversActive False)
    $ do
      it "Test ordering" testOrder
      it "Test radius filtration" testInRadius
      it "Test outside radius filtration" testNotInRadius

testOrder :: IO ()
testOrder = do
  res <-
    runARDUFlow "Test ordering" $
      Q.getNearestDrivers pickupPoint 5000 org1 <&> getIds
  res `shouldBe` [closestDriver, furthestDriver]

testInRadius :: IO ()
testInRadius = do
  res <-
    runARDUFlow "Test radius filtration" $
      Q.getNearestDrivers pickupPoint 800 org1 <&> getIds
  res `shouldBe` [closestDriver]

testNotInRadius :: IO ()
testNotInRadius = do
  res <-
    runARDUFlow "Test outside radius filtration" $
      Q.getNearestDrivers pickupPoint 10 org1 <&> getIds
  res `shouldBe` []

getIds :: [Q.DriverPoolResult] -> [Text]
getIds = map (getId . (.driverId))

pickupPoint :: LatLong
pickupPoint = LatLong 12.994927 77.596386

org1 :: forall k. Id k
org1 = Id "nearest-drivers-testing-organization"

furthestDriver :: Text -- distance is 600
furthestDriver = "ND-furthest_driver-00000000000000000"

closestDriver :: Text -- distance is 1200
closestDriver = "ND-closest-driver-000000000000000000"

--not used yet
--otherDriver :: Text
--otherDriver = "ND-other-driver-00000000000000000000"

-- distance to next drivers is more than 5000 (about 6900)
suvDriver :: Text
suvDriver = "ND-suv-driver-0000000000000000000000"

sedanDriver :: Text
sedanDriver = "ND-sedan-driver-00000000000000000000"

hatchbackDriver :: Text
hatchbackDriver = "ND-hatchback-driver-0000000000000000"

setDriversActive :: Bool -> FlowR ARDUEnv.AppEnv ()
setDriversActive isActive = Esq.runTransaction $ do
  let drivers = [furthestDriver, closestDriver, suvDriver, sedanDriver, hatchbackDriver]
  forM_ drivers (\driver -> Q.updateActivity (Id driver) isActive)
