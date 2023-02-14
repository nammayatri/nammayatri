 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.FareCalculator.RentalFareCalculator.Flow
  ( RentalFareParameters,
    ServiceHandle (..),
    calculateRentalFare,
    doCalculateRentalFare,
    rentalFareSum,
    rentalFareSumWithDiscount,
    buildRentalFareBreakups,
  )
where

import Domain.Types.Booking (Booking)
import Domain.Types.FarePolicy.FareBreakup
import qualified Domain.Types.FarePolicy.RentalFarePolicy as DRentalFP
import EulerHS.Prelude hiding (id)
import Kernel.Prelude
import Kernel.Storage.Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FareCalculator.RentalFareCalculator.Calculator
  ( RentalFareParameters,
    calculateRentalFareParameters,
    rentalFareSum,
    rentalFareSumWithDiscount,
  )
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.FarePolicy.RentalFarePolicy as QRentalFP
import Tools.Error

type MonadHandler m = (MonadThrow m, Log m, MonadGuid m)

newtype ServiceHandle m = ServiceHandle
  { getRentalFarePolicy :: Id DRentalFP.RentalFarePolicy -> m (Maybe DRentalFP.RentalFarePolicy)
  }

serviceHandle :: (HasCacheConfig r, EsqDBFlow m r, HedisFlow m r) => ServiceHandle m
serviceHandle =
  ServiceHandle
    { getRentalFarePolicy = \rentalFarePolicyId -> do
        QRentalFP.findById rentalFarePolicyId
    }

calculateRentalFare ::
  (HasCacheConfig r, EsqDBFlow m r, HedisFlow m r) =>
  Id DRentalFP.RentalFarePolicy ->
  Meters ->
  UTCTime ->
  UTCTime ->
  m RentalFareParameters
calculateRentalFare = doCalculateRentalFare serviceHandle

doCalculateRentalFare ::
  MonadHandler m =>
  ServiceHandle m ->
  Id DRentalFP.RentalFarePolicy ->
  Meters ->
  UTCTime ->
  UTCTime ->
  m RentalFareParameters
doCalculateRentalFare ServiceHandle {..} rentalFarePolicyId distance startTime stopTime = do
  rentalFarePolicy <-
    getRentalFarePolicy rentalFarePolicyId
      >>= fromMaybeM NoRentalFarePolicy
  logTagInfo "RentalFareCalculator" $
    "Initiating rental fare calculation for organization "
      +|| rentalFarePolicy.merchantId ||+ " for "
      +|| rentalFarePolicy.vehicleVariant ||+ ""
  let fareParams = calculateRentalFareParameters rentalFarePolicy distance startTime stopTime
  logTagInfo
    "RentalFareCalculator"
    $ "Rental fare parameters calculated: " +|| fareParams ||+ ""
  pure fareParams

buildRentalFareBreakups :: MonadGuid m => RentalFareParameters -> Id Booking -> m [FareBreakup]
buildRentalFareBreakups fareParams bookingId = do
  baseFareBreakup <- buildBaseFareBreakup fareParams bookingId
  extraDistanceFareBreakup <- buildExtraDistanceFareBreakup fareParams bookingId
  extraTimeFareBreakup <- buildExtraTimeFareBreakup fareParams bookingId
  nextDaysFareBreakup <- buildNextDaysFareBreakup fareParams bookingId
  discountFareBreakup <- buildDiscountFareBreakup fareParams.discount bookingId
  pure $ [baseFareBreakup, extraDistanceFareBreakup, extraTimeFareBreakup] <> catMaybes [nextDaysFareBreakup, discountFareBreakup]

buildBaseFareBreakup :: MonadGuid m => RentalFareParameters -> Id Booking -> m FareBreakup
buildBaseFareBreakup fareParams bookingId = do
  id <- Id <$> generateGUIDText
  let amount = fromIntegral fareParams.baseFare
      description =
        "Base fare for "
          <> show fareParams.farePolicy.baseDistance
          <> " km and "
          <> show fareParams.farePolicy.baseDuration
          <> " hours is "
          <> show amount
          <> " rupees"
  pure FareBreakup {..}

buildExtraDistanceFareBreakup :: MonadGuid m => RentalFareParameters -> Id Booking -> m FareBreakup
buildExtraDistanceFareBreakup fareParams bookingId = do
  id <- Id <$> generateGUIDText
  let amount = fromIntegral fareParams.extraDistanceFare
      description =
        "Extra distance fare with fare policy "
          <> showRounded fareParams.farePolicy.extraKmFare
          <> " rupees per km is "
          <> show amount
          <> " rupees"
  pure FareBreakup {..}

buildExtraTimeFareBreakup :: MonadGuid m => RentalFareParameters -> Id Booking -> m FareBreakup
buildExtraTimeFareBreakup fareParams bookingId = do
  id <- Id <$> generateGUIDText
  let amount = fromIntegral fareParams.extraTimeFare
      description =
        "Extra time fare with fare policy "
          <> showRounded fareParams.farePolicy.extraMinuteFare
          <> " rupees per minute is "
          <> show amount
          <> " rupees"
  pure FareBreakup {..}

buildNextDaysFareBreakup :: MonadGuid m => RentalFareParameters -> Id Booking -> m (Maybe FareBreakup)
buildNextDaysFareBreakup fareParams bookingId = do
  let mbAmount = fromIntegral <$> fareParams.nextDaysFare
  forM mbAmount $ \amount -> do
    id <- Id <$> generateGUIDText
    let description =
          "Next days fare with fare policy "
            <> show (fromMaybe 0 fareParams.farePolicy.driverAllowanceForDay)
            <> " rupees per next day is "
            <> show amount
            <> " rupees"
    pure FareBreakup {..}

buildDiscountFareBreakup :: MonadGuid m => Maybe Money -> Id Booking -> m (Maybe FareBreakup)
buildDiscountFareBreakup mbDiscount bookingId = do
  forM mbDiscount $ \discount -> do
    id <- Id <$> generateGUIDText
    let amount = fromIntegral $ negate discount -- this amount should be always below zero
        description = "Discount is " <> show discount <> " rupees"
    pure FareBreakup {..}
