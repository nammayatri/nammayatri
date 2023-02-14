 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.FareCalculator.OneWayFareCalculator.Flow
  ( OneWayFareParameters,
    ServiceHandle (..),
    calculateFare,
    doCalculateFare,
    fareSum,
    fareSumWithDiscount,
    buildOneWayFareBreakups,
  )
where

import Domain.Types.Booking
import Domain.Types.FarePolicy.FareBreakup
import Domain.Types.FarePolicy.OneWayFarePolicy (OneWayFarePolicy)
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Vehicle as Vehicle
import EulerHS.Prelude hiding (id)
import Kernel.Prelude
import Kernel.Storage.Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FareCalculator.OneWayFareCalculator.Calculator
  ( OneWayFareParameters (..),
    TripEndTime,
    calculateFareParameters,
    fareSum,
    fareSumWithDiscount,
  )
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.FarePolicy.OneWayFarePolicy as OWFarePolicy
import Tools.Error

type MonadHandler m = (MonadThrow m, Log m)

newtype ServiceHandle m = ServiceHandle
  { getFarePolicy :: Id Merchant -> Vehicle.Variant -> m (Maybe OneWayFarePolicy)
  }

serviceHandle :: (CacheFlow m r, EsqDBFlow m r) => ServiceHandle m
serviceHandle =
  ServiceHandle
    { getFarePolicy = \merchantId vehicleVariant -> do
        OWFarePolicy.findByMerchantIdAndVariant merchantId vehicleVariant
    }

calculateFare ::
  ( HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Vehicle.Variant ->
  Meters ->
  UTCTime ->
  m OneWayFareParameters
calculateFare = doCalculateFare serviceHandle

doCalculateFare ::
  MonadHandler m =>
  ServiceHandle m ->
  Id Merchant ->
  Vehicle.Variant ->
  Meters ->
  TripEndTime ->
  m OneWayFareParameters
doCalculateFare ServiceHandle {..} merchantId vehicleVariant distance endTime = do
  logTagInfo "FareCalculator" $ "Initiating fare calculation for organization " +|| merchantId ||+ " for " +|| vehicleVariant ||+ ""
  farePolicy <- getFarePolicy merchantId vehicleVariant >>= fromMaybeM NoFarePolicy
  let fareParams = calculateFareParameters farePolicy distance endTime
  logTagInfo
    "FareCalculator"
    $ "Fare parameters calculated: " +|| fareParams ||+ ""
  pure fareParams

buildOneWayFareBreakups :: MonadGuid m => OneWayFareParameters -> Id Booking -> m [FareBreakup]
buildOneWayFareBreakups fareParams bookingId = do
  baseFareBreakup <- buildBaseFareBreakup fareParams bookingId
  distanceFareBreakup <- buildDistanceFareBreakup fareParams bookingId
  discountFareBreakup <- buildDiscountFareBreakup fareParams.discount bookingId
  pure $ [baseFareBreakup, distanceFareBreakup] <> maybeToList discountFareBreakup

buildBaseFareBreakup :: MonadGuid m => OneWayFareParameters -> Id Booking -> m FareBreakup
buildBaseFareBreakup OneWayFareParameters {..} bookingId = do
  id <- Id <$> generateGUIDText
  let amount = nightShiftRate * fromIntegral baseFare
      description = "Base fare is " <> show amount <> " rupees"
  pure FareBreakup {..}

buildDistanceFareBreakup :: MonadGuid m => OneWayFareParameters -> Id Booking -> m FareBreakup
buildDistanceFareBreakup OneWayFareParameters {..} bookingId = do
  id <- Id <$> generateGUIDText
  let amount = nightShiftRate * fromIntegral distanceFare
      description = "Distance fare is " <> show amount <> " rupees"
  pure FareBreakup {..}

buildDiscountFareBreakup :: MonadGuid m => Maybe Money -> Id Booking -> m (Maybe FareBreakup)
buildDiscountFareBreakup mbDiscount bookingId = do
  forM mbDiscount $ \discount -> do
    id <- Id <$> generateGUIDText
    let amount = fromIntegral $ negate discount -- this amount should be always below zero
        description = "Discount is " <> show discount <> " rupees"
    pure FareBreakup {..}
