{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.RideBooking.NotifyRideInfo (postNotifyRideInfoNotifyRideInfo) where

import qualified API.Types.Dashboard.RideBooking.NotifyRideInfo as NRI
import Data.OpenApi (ToSchema)
import qualified Domain.Action.Beckn.Common as Common
import qualified Domain.Types.Merchant as DM
import qualified "this" Domain.Types.Person
import qualified Domain.Types.Ride as DR
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Beckn.Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRiderConfig
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide
import Tools.Auth
import Tools.Error
import qualified Tools.Whatsapp

postNotifyRideInfoNotifyRideInfo :: ShortId DM.Merchant -> City -> Id Domain.Types.Person.Person -> NRI.NotifyRideInfoRequest -> Environment.Flow APISuccess
postNotifyRideInfoNotifyRideInfo merchantShortId opCity personId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  ride <- QRide.findById req.rideId >>= fromMaybeM (RideNotFound req.rideId.getId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  riderConfig <- QRiderConfig.findByMerchantOperatingCityId merchantOperatingCity.id >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCity.id.getId)
  when (ride.status == DR.CANCELLED || ride.status == DR.COMPLETED) $ throwError (InternalError $ "Cannot send message for Ride that is " <> show ride.status)
  case req.notificationType of
    NRI.WHATSAPP -> Common.sendRideBookingDetailsViaWhatsapp personId ride booking riderConfig
    NRI.SMS -> throwError (InternalError "SMS Notification is not implemented")
  pure Success
