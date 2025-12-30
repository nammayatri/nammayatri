{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Mobility.ARDU.NearestDrivers (spec) where

-- import qualified Kernel.Storage.Esqueleto as Esq
import qualified "dynamic-offer-driver-app" Domain.Action.Internal.DriverMode as DDriverMode
import qualified "dynamic-offer-driver-app" Domain.Types.Common as DI
import qualified "dynamic-offer-driver-app" Environment as ARDUEnv
import EulerHS.Prelude
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Types.Error
import Kernel.Types.Flow (FlowR)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Mobility.ARDU.Fixtures as Fixtures
import qualified "dynamic-offer-driver-app" Storage.Cac.TransporterConfig as SCTC
import qualified "dynamic-offer-driver-app" Storage.Queries.DriverInformation as QDI
import qualified "dynamic-offer-driver-app" Storage.Queries.Person as Q
import qualified "dynamic-offer-driver-app" Storage.Queries.Person.GetNearestDrivers as S
import Test.Hspec
import "dynamic-offer-driver-app" Tools.Error (DriverInformationError (..))
import Utils

spec :: Spec
spec = do
  describe "getNearestDrivers function"
    . beforeAll_
      ( runARDUFlow "Turn on drivers" $ do
          setDriversActive True (Just DI.ONLINE)
      )
    . afterAll_ (runARDUFlow "Turn off drivers." $ setDriversActive False (Just DI.OFFLINE))
    $ do
      it "Test ordering" testOrder
      it "Test radius filtration" testInRadius
      it "Test outside radius filtration" testNotInRadius

hour :: Seconds
hour = 3600

createNearestDriverReq :: Meters -> UTCTime -> S.NearestDriversReq
createNearestDriverReq nearestRadius now =
  S.NearestDriversReq
    { cityServiceTiers = [],
      serviceTiers = [],
      fromLocLatLong = pickupPoint,
      merchantId = org1,
      driverPositionInfoExpiry = Just hour,
      isRental = False,
      isInterCity = False,
      isValueAddNP = True,
      onlinePayment = False,
      prepaidSubscriptionThreshold = Nothing,
      fleetPrepaidSubscriptionThreshold = Nothing,
      prepaidSubscriptionAndWalletEnabled = False,
      rideFare = Nothing,
      paymentInstrument = Nothing,
      paymentMode = Nothing,
      minWalletAmountForCashRides = Nothing,
      ..
    }

testOrder :: IO ()
testOrder = do
  now <- getCurrentTime
  res <-
    runARDUFlow "Test ordering" $
      (S.getNearestDrivers (createNearestDriverReq 5000 now) <&> getIds)
  res `shouldSatisfy` equals [closestDriver, furthestDriver]

testInRadius :: IO ()
testInRadius = do
  now <- getCurrentTime
  res <-
    runARDUFlow "Test radius filtration" $
      (S.getNearestDrivers (createNearestDriverReq 800 now) <&> getIds)
  res `shouldSatisfy` equals [closestDriver]

testNotInRadius :: IO ()
testNotInRadius = do
  now <- getCurrentTime
  res <-
    runARDUFlow "Test outside radius filtration" $
      (S.getNearestDrivers (createNearestDriverReq 10 now) <&> getIds)
  res `shouldSatisfy` equals []

getIds :: [Q.NearestDriversResult] -> [Text]
getIds = map (getId . (.driverId))

pickupPoint :: LatLong
pickupPoint = LatLong 12.994927 77.596386

org1 :: forall k. Id k
org1 = Id "nearest-drivers-testing-organization"

furthestDriver :: Text -- distance is 600
furthestDriver = "ND-furthest_driver-00000000000000000"

driverWithOldLocation :: Text
driverWithOldLocation = "ND-driver-with-old-location-00000000"

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

setDriversActive :: Bool -> Maybe DI.DriverMode -> FlowR ARDUEnv.AppEnv ()
setDriversActive isActive mode = do
  -- Esq.runTransaction $ do
  let drivers = [furthestDriver, closestDriver, suvDriver, sedanDriver, hatchbackDriver, driverWithOldLocation]
  let newFlowStatus = DDriverMode.getDriverFlowStatus mode isActive
  transporterConfig <-
    SCTC.findByMerchantOpCityId Fixtures.nammaYatriPartnerMerchantOperatingCityId Nothing
      >>= fromMaybeM (TransporterConfigNotFound Fixtures.nammaYatriPartnerMerchantOperatingCityId.getId)
  forM_
    drivers
    ( \driver -> do
        driverInfo <- QDI.findById (Id driver) >>= fromMaybeM DriverInfoNotFound
        DDriverMode.updateDriverModeAndFlowStatus (Id driver) transporterConfig isActive mode newFlowStatus driverInfo Nothing Nothing
    )
