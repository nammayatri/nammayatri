{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.Penalty (postPenaltyCheck) where

import qualified API.Types.UI.Penalty
import Data.Either.Extra (eitherToMaybe)
import Data.OpenApi (ToSchema)
import qualified Domain.Action.UI.Ride.CancelRide.Internal as CancelRideInternal
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Ride as DRide
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types as Maps
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Event as Yudhishthira
import qualified Lib.Yudhishthira.Types as YT
import qualified Lib.Yudhishthira.Types.Application as YA
import qualified Storage.Cac.FarePolicy as QFP
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide
import Tools.Auth
import Tools.Constants
import Tools.Error

postPenaltyCheck ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.Penalty.PenaltyCheckReq ->
    Environment.Flow API.Types.UI.Penalty.PenaltyCheckRes
  )
postPenaltyCheck (mbPersonId, _merchantId, _merchantOpCityId) req = do
  driverId <- mbPersonId & fromMaybeM (InvalidRequest "Driver not authenticated")

  ride <- QRide.findById (Kernel.Types.Id.Id req.rideId) >>= fromMaybeM (RideNotFound req.rideId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)

  unless (ride.driverId == Kernel.Types.Id.cast driverId) $
    throwError (InvalidRequest "Driver does not own this ride")

  unless (ride.status == DRide.NEW) $
    throwError (InvalidRequest "Ride cannot be cancelled in current state")

  (isApplicable, penaltyAmount) <- case booking.fareParams.driverCancellationPenaltyAmount of
    Just penaltyAmount -> do
      tagData <- CancelRideInternal.buildPenaltyCheckContext booking ride req.point
      tagsE <- withTryCatch "computeNammaTags:PenaltyCheck" $ Yudhishthira.computeNammaTags YA.PenaltyCheck tagData
      let tags = fromMaybe [] $ eitherToMaybe tagsE
          isPenaltyApplicable = validCancellationPenaltyApplicable `elem` tags
          existingTags = fromMaybe [] ride.rideTags
          updatedTags = if isPenaltyApplicable && validCancellationPenaltyApplicable `notElem` existingTags then validCancellationPenaltyApplicable : existingTags else if not isPenaltyApplicable then filter (/= validCancellationPenaltyApplicable) existingTags else existingTags
      QRide.updateRideTags (Just updatedTags) ride.id
      return (isPenaltyApplicable, Just penaltyAmount)
    Nothing -> do
      return (False, Nothing)
  return $
    API.Types.UI.Penalty.PenaltyCheckRes
      { isCancellationPenaltyApplicable = Just isApplicable,
        cancellationPenaltyAmount = penaltyAmount
      }
