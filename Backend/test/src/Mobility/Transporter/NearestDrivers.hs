{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Mobility.Transporter.NearestDrivers (spec) where

import qualified "static-offer-driver-app" Domain.Types.FarePolicy.FareProduct as SFP
import "static-offer-driver-app" Domain.Types.Vehicle
import qualified "static-offer-driver-app" Environment as BecknTransport
import EulerHS.Prelude
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Flow (FlowR)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified "static-offer-driver-app" Storage.Queries.DriverInformation as Q
import qualified "static-offer-driver-app" Storage.Queries.DriverLocation as QL
import qualified "static-offer-driver-app" Storage.Queries.Person as Q
import Test.Hspec
import Utils

spec :: Spec
spec = do
  describe "getNearestDrivers function"
    . beforeAll_
      ( runTransporterFlow "Turn on drivers" $ do
          setDriversActive True
          setDriverWithOldLocation
      )
    . afterAll_ (runTransporterFlow "Turn off drivers." $ setDriversActive False)
    $ do
      it "Test ordering" testOrder
      it "Test radius filtration" testInRadius
      it "Test outside radius filtration" testNotInRadius
      it "Test downgrading driver with SUV ride request" testDowngradingDriverWithSUV
      it "Test downgrading driver with sedan ride request" testDowngradingDriverWithSedan
      it "Test downgrading driver with hatchback ride request" testDowngradingDriverWithHatchback
      it "Test isRental" testIsRental
      it "Test notRental" testNotRental

hour :: Seconds
hour = 3600

testOrder :: IO ()
testOrder = do
  res <-
    runTransporterFlow "Test ordering" $
      Q.getNearestDrivers pickupPoint 5000 org1 (Just SUV) SFP.ONE_WAY (Just hour) (Proxy @StaticDriverAppFlow) <&> getIds
  res `shouldSatisfy` equals [closestDriver, furthestDriver]

testInRadius :: IO ()
testInRadius = do
  res <-
    runTransporterFlow "Test radius filtration" $
      Q.getNearestDrivers pickupPoint 800 org1 (Just SUV) SFP.ONE_WAY (Just hour) (Proxy @StaticDriverAppFlow) <&> getIds
  res `shouldSatisfy` equals [closestDriver]

testNotInRadius :: IO ()
testNotInRadius = do
  res <-
    runTransporterFlow "Test outside radius filtration" $
      Q.getNearestDrivers pickupPoint 0 org1 (Just SUV) SFP.ONE_WAY (Just hour) (Proxy @StaticDriverAppFlow) <&> getIds
  res `shouldSatisfy` equals []

testDowngradingDriverWithSUV :: IO ()
testDowngradingDriverWithSUV = do
  res <-
    runTransporterFlow "Test downgrading driver with SUV ride request" $
      Q.getNearestDrivers pickupPoint 10000 org1 (Just SUV) SFP.ONE_WAY (Just hour) (Proxy @StaticDriverAppFlow) <&> getIds
  res `shouldSatisfy` equals [closestDriver, furthestDriver, suvDriver]

testDowngradingDriverWithSedan :: IO ()
testDowngradingDriverWithSedan = do
  res <-
    runTransporterFlow "Test downgrading driver with sedan ride request" $
      Q.getNearestDrivers pickupPoint 10000 org1 (Just SEDAN) SFP.ONE_WAY (Just hour) (Proxy @StaticDriverAppFlow) <&> getIds
  res `shouldSatisfy` equals [suvDriver, sedanDriver]

testDowngradingDriverWithHatchback :: IO ()
testDowngradingDriverWithHatchback = do
  res <-
    runTransporterFlow "Test downgrading driver with hatchback ride request" $
      Q.getNearestDrivers pickupPoint 10000 org1 (Just HATCHBACK) SFP.ONE_WAY (Just hour) (Proxy @StaticDriverAppFlow) <&> getIds
  res `shouldSatisfy` equals [suvDriver, sedanDriver, hatchbackDriver]

testIsRental :: IO ()
testIsRental = do
  res <-
    runTransporterFlow "Test isRental" $ do
      setSuvDriverRental True
      Q.getNearestDrivers pickupPoint 10000 org1 (Just HATCHBACK) SFP.RENTAL (Just hour) (Proxy @StaticDriverAppFlow) <&> getIds
  res `shouldSatisfy` equals [suvDriver]

testNotRental :: IO ()
testNotRental = do
  res <-
    runTransporterFlow "Test notRental" $ do
      setSuvDriverRental False
      Q.getNearestDrivers pickupPoint 10000 org1 (Just HATCHBACK) SFP.RENTAL (Just hour) (Proxy @StaticDriverAppFlow) <&> getIds
  res `shouldSatisfy` equals []

getIds :: [Q.NearestDriversResult] -> [Text]
getIds = map (getId . (.driverId))

pickupPoint :: LatLong
pickupPoint = LatLong 12.994927 77.596386

org1 :: forall k. Id k
org1 = Id "7f7896dd-787e-4a0b-8675-e9e6fe93bb8f"

furthestDriver :: Text
furthestDriver = "001093df-4f7c-440f-b-furthest_driver"

closestDriver :: Text
closestDriver = "002093df-4f7c-440f-ba-closest_driver"

driverWithOldLocation :: Text
driverWithOldLocation = "002093df-4f-driver-with-old-location"

otherDriver :: Text
otherDriver = "003093df-4f7c-440f-bada-other_driver"

-- distance to next drivers is more than 5000
suvDriver :: Text
suvDriver = "003093df-4f7c-440f-bada-4-suv_driver"

sedanDriver :: Text
sedanDriver = "003093df-4f7c-440f-bada-sedan_driver"

hatchbackDriver :: Text
hatchbackDriver = "003093df-4f7c-440f--hatchback_driver"

setDriversActive :: Bool -> FlowR BecknTransport.AppEnv ()
setDriversActive isActive = Esq.runTransaction $ do
  let drivers = [furthestDriver, closestDriver, otherDriver, suvDriver, sedanDriver, hatchbackDriver, driverWithOldLocation]
  forM_ drivers (\driver -> Q.updateActivity @StaticDriverAppFlow (Id driver) isActive)

setSuvDriverRental :: Bool -> FlowR BecknTransport.AppEnv ()
setSuvDriverRental isRental = Esq.runTransaction $ do
  Q.updateRental @StaticDriverAppFlow (Id suvDriver) isRental

-- we can remove this when we flatten migrations
setDriverWithOldLocation :: FlowR BecknTransport.AppEnv ()
setDriverWithOldLocation = do
  now <- getCurrentTime
  Esq.runTransaction . void $
    QL.upsertGpsCoord @StaticDriverAppFlow (Id driverWithOldLocation) (LatLong 13.005432 77.59336) ((-86400) `addUTCTime` now) -- one day ago
