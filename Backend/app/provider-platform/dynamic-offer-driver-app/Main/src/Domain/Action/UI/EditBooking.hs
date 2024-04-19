{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.EditBooking where

import API.Types.UI.EditBooking
import Data.OpenApi (ToSchema)
import Domain.Types.BookingUpdateRequest
import qualified Domain.Types.LocationMapping as DLM
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import qualified Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Error.Throwing
import Servant hiding (throwError)
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.BookingUpdateRequest as QBUR
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.Ride as QR
import Tools.Auth

postEditResult ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest ->
    API.Types.UI.EditBooking.EditBookingRespondAction ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postEditResult (mbPersonId, _, _) bookingUpdateReqId action = do
  _ <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  now <- getCurrentTime
  bookingUpdateReq <- B.runInReplica $ QBUR.findById bookingUpdateReqId >>= fromMaybeM (InternalError $ "BookingUpdateRequest not found with id:" <> bookingUpdateReqId.getId)
  when (bookingUpdateReq.status /= USER_CONFIRMED) $ throwError $ InvalidRequest "BookingUpdateRequest is not in USER_CONFIRMED state"
  when (bookingUpdateReq.validTill < now) $ throwError $ InvalidRequest "BookingUpdateRequest is expired"
  if action == ACCEPT
    then do
      QBUR.updateStatusById DRIVER_ACCEPTED bookingUpdateReqId
      dropLocMapping <- QLM.getLatestEndByEntityId bookingUpdateReqId.getId >>= fromMaybeM (InternalError $ "Latest drop location mapping not found for bookingUpdateReqId: " <> bookingUpdateReqId.getId)
      dropLocMapBooking <- SLM.buildDropLocationMapping dropLocMapping.locationId bookingUpdateReq.bookingId.getId DLM.BOOKING (Just bookingUpdateReq.merchantId) (Just bookingUpdateReq.merchantOperatingCityId)
      ride <- QR.findActiveByRBId bookingUpdateReq.bookingId >>= fromMaybeM (InternalError $ "Ride not found for bookingId: " <> bookingUpdateReq.bookingId.getId)
      dropLocMapRide <- SLM.buildDropLocationMapping dropLocMapping.locationId ride.id.getId DLM.RIDE (Just bookingUpdateReq.merchantId) (Just bookingUpdateReq.merchantOperatingCityId)
      QLM.create dropLocMapBooking
      QLM.create dropLocMapRide
      let estimatedDistance = highPrecMetersToMeters <$> bookingUpdateReq.estimatedDistance
      QB.updateMultipleById bookingUpdateReq.estimatedFare bookingUpdateReq.maxEstimatedDistance estimatedDistance bookingUpdateReq.fareParamsId.getId bookingUpdateReq.bookingId
      return Success
    else do
      QBUR.updateStatusById DRIVER_REJECTED bookingUpdateReqId
      return Success
