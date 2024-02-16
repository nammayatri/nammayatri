{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Mobility.ARDU.NearestDrivers (spec) where

import qualified "dynamic-offer-driver-app" Domain.Types.DriverInformation as DI
import qualified "dynamic-offer-driver-app" Environment as ARDUEnv
import EulerHS.Prelude
import Kernel.External.Maps.Types (LatLong (..))
-- import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Flow (FlowR)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified "dynamic-offer-driver-app" Storage.Queries.DriverInformation as Q
import qualified "dynamic-offer-driver-app" Storage.Queries.Person as Q
import qualified "dynamic-offer-driver-app" Storage.Queries.Person.GetNearestDrivers as S
import Test.Hspec
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

testOrder :: IO ()
testOrder = do
  res <-
    runARDUFlow "Test ordering" $
      S.getNearestDrivers Nothing pickupPoint 5000 org1 False (Just hour) False <&> getIds
  res `shouldSatisfy` equals [closestDriver, furthestDriver]

testInRadius :: IO ()
testInRadius = do
  res <-
    runARDUFlow "Test radius filtration" $
      S.getNearestDrivers Nothing pickupPoint 800 org1 False (Just hour) False <&> getIds
  res `shouldSatisfy` equals [closestDriver]

testNotInRadius :: IO ()
testNotInRadius = do
  res <-
    runARDUFlow "Test outside radius filtration" $
      S.getNearestDrivers Nothing pickupPoint 10 org1 False (Just hour) False <&> getIds
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
  forM_ drivers (\driver -> Q.updateActivity (Id driver) isActive mode)
