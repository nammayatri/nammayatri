{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.MeterRide (postMeterRideAddDestination) where

import qualified API.Types.UI.MeterRide
import Data.OpenApi (ToSchema)
import qualified Domain.Action.UI.Ride as AUR
import Domain.Types.Location (Location (..), LocationAddress)
import qualified Domain.Types.LocationMapping as DLM
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Maps.Types (LatLong)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fork, fromMaybeM, generateGUID, getCurrentTime)
import Servant
import qualified SharedLogic.CallBAPInternal as CallBAPInternal
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.Ride as QRide
import Tools.Auth
import Tools.Error

postMeterRideAddDestination ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    API.Types.UI.MeterRide.MeterRideAddDestinationReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postMeterRideAddDestination (_mbPersonId, merchantId, merchantOpCityId) rideId meterRideRequest = do
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  dropLocation <- buildLocation merchantId merchantOpCityId meterRideRequest.gps meterRideRequest.location
  QL.create dropLocation
  newRideDropLocationMap <- SLM.buildDropLocationMapping dropLocation.id rideId.getId DLM.RIDE (Just merchantId) (Just merchantOpCityId)
  QLM.create newRideDropLocationMap
  newBookingDropLocationMap <- SLM.buildDropLocationMapping dropLocation.id ride.bookingId.getId DLM.BOOKING (Just merchantId) (Just merchantOpCityId)
  QLM.create newBookingDropLocationMap
  fork "update in bap" $ do
    appBackendBapInternal <- asks (.appBackendBapInternal)
    void $ CallBAPInternal.meterRideAddDestination appBackendBapInternal.apiKey appBackendBapInternal.url rideId.getId meterRideRequest
  pure Kernel.Types.APISuccess.Success

buildLocation ::
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  LatLong ->
  LocationAddress ->
  Environment.Flow Location
buildLocation merchantId merchantOperatingCityId gps locationAddress = do
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
        merchantId = Just merchantId,
        merchantOperatingCityId = Just merchantOperatingCityId
      }
