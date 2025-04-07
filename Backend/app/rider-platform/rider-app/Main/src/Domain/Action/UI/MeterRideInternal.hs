module Domain.Action.UI.MeterRideInternal (postAddDestination, getGetCustomerInfo) where

import qualified API.Types.UI.MeterRideInternal
import qualified Data.Text
import Domain.Types
import Domain.Types.Location
import Domain.Types.LocationAddress
import qualified Domain.Types.LocationMapping as DLM
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (getDbHash)
import Kernel.External.Maps.Types (LatLong)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, generateGUID, getCurrentTime, throwError)
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import Tools.Error

postAddDestination ::
  ( Kernel.Prelude.Text ->
    Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    API.Types.UI.MeterRideInternal.MeterRideAddDestinationReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postAddDestination bppRideId mbToken addDestinationReq = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == mbToken) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  ride <- QRide.findByBPPRideId (Kernel.Types.Id.Id bppRideId) >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  unless (booking.tripCategory == Just (OneWay MeterRide)) $
    throwError $ InvalidRequest ("Invalid trip category " <> show booking.tripCategory)
  dropLocation <- buildLocation ride.merchantId ride.merchantOperatingCityId addDestinationReq.destinationLatLong addDestinationReq.destinationLocation
  QL.create dropLocation
  pickupMapForBooking <- SLM.buildDropLocationMapping dropLocation.id ride.bookingId.getId DLM.BOOKING ride.merchantId ride.merchantOperatingCityId
  QLM.create pickupMapForBooking
  pickupMapForRide <- SLM.buildDropLocationMapping dropLocation.id ride.id.getId DLM.RIDE ride.merchantId ride.merchantOperatingCityId
  QLM.create pickupMapForRide
  pure Kernel.Types.APISuccess.Success

buildLocation ::
  Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) ->
  Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) ->
  LatLong ->
  LocationAddress ->
  Environment.Flow Location
buildLocation merchantId mbMerchantOperatingCityId gps locationAddress = do
  guid <- generateGUID
  now <- getCurrentTime
  return $
    Location
      { id = guid,
        createdAt = now,
        updatedAt = now,
        lat = gps.lat,
        lon = gps.lon,
        address = locationAddress,
        merchantId = merchantId,
        merchantOperatingCityId = mbMerchantOperatingCityId
      }

getGetCustomerInfo :: (Kernel.Prelude.Maybe (Data.Text.Text) -> API.Types.UI.MeterRideInternal.CustomerInfo -> Environment.Flow API.Types.UI.MeterRideInternal.CustomerInfoResponse)
getGetCustomerInfo mbToken API.Types.UI.MeterRideInternal.CustomerInfo {..} = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == mbToken) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  phoneNumbersHashes <- getDbHash customerMobileNumber
  person <- QP.findByMobileNumberHashAndCountryCode customerMobileCountryCode phoneNumbersHashes
  case person of
    Just person' -> do
      personsWithSameDeviceId <- maybe (pure []) (QP.findAllByDeviceId . Just) person'.deviceId -- should consider merchant in querstion as well here, but for time being not doing that as very small set would be affected.
      let isMultipleDeviceIdExist = Just (length personsWithSameDeviceId > 1)
      pure $
        API.Types.UI.MeterRideInternal.CustomerInfoResponse
          { alreadyReferred = Just True,
            isMultipleDeviceIdExist
          }
    Nothing ->
      pure $
        API.Types.UI.MeterRideInternal.CustomerInfoResponse
          { alreadyReferred = Nothing,
            isMultipleDeviceIdExist = Nothing
          }
