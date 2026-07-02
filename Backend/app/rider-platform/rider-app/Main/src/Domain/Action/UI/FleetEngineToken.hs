{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Mints a short-lived Google Fleet Engine (On-demand Rides & Deliveries)
-- *consumer* JWT for a given ride so the rider Consumer SDK can subscribe to the
-- trip. The Fleet Engine @tripId@ is the (1:1) BPP ride id; the JWT is minted on
-- the fly from the merchant-operating-city's encrypted service-account JSON.
--
-- NOTE: depends on the @Kernel.External.FleetEngine.*@ modules from shared-kernel
-- (branch claude/fleet-engine-odrd-sdk); a shared-kernel pin bump is required for
-- this to compile.
module Domain.Action.UI.FleetEngineToken
  ( FleetEngineConsumerTokenRes (..),
    getFleetEngineConsumerToken,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.Merchant as Merchant
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import qualified Domain.Types.MerchantServiceConfig as DOSC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import Environment
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.FleetEngine.Auth as FEAuth
import Kernel.External.FleetEngine.Config (FleetEngineCfg)
import qualified Kernel.External.FleetEngine.Config as FEConfig
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QOMSC
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide
import Tools.Error

data FleetEngineConsumerTokenRes = FleetEngineConsumerTokenRes
  { token :: Text,
    tripId :: Text,
    providerId :: Text,
    expiresInSeconds :: Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

getFleetEngineConsumerToken ::
  (Id Person.Person, Id Merchant.Merchant) ->
  Id Ride.Ride ->
  Flow FleetEngineConsumerTokenRes
getFleetEngineConsumerToken (personId, _merchantId) rideId = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  unless (booking.riderId == personId) $ throwError AccessDenied
  cfg <- getFleetEngineCfg booking.merchantId booking.merchantOperatingCityId >>= fromMaybeM (InternalError "Fleet Engine not configured")
  saText <- decrypt cfg.serviceAccountJson
  sa <- case FEAuth.parseServiceAccount saText of
    Left err -> throwError $ InternalError ("Fleet Engine: invalid service account: " <> T.pack err)
    Right sa -> pure sa
  let tripId = ride.bppRideId.getId
      ttl = fromMaybe FEConfig.defaultConsumerTokenTtl cfg.consumerTokenTtlSeconds
  eToken <- liftIO $ FEAuth.mintFleetEngineToken sa (FEAuth.ConsumerToken tripId) ttl
  token <- case eToken of
    Left err -> throwError $ InternalError ("Fleet Engine: consumer token mint failed: " <> T.pack err)
    Right token -> pure token
  pure
    FleetEngineConsumerTokenRes
      { token = token,
        tripId = tripId,
        providerId = cfg.providerId,
        expiresInSeconds = ttl
      }

getFleetEngineCfg ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id Merchant.Merchant ->
  Id MerchantOperatingCity ->
  m (Maybe FleetEngineCfg)
getFleetEngineCfg merchantId merchantOpCityId = do
  mbServiceConfig <- QOMSC.findByMerchantOpCityIdAndService merchantId merchantOpCityId (DOSC.FleetEngineService DOSC.GoogleFleetEngine)
  pure $ case mbServiceConfig of
    Just sc -> case sc.serviceConfig of
      DOSC.FleetEngineServiceConfig cfg -> Just cfg
      _ -> Nothing
    Nothing -> Nothing
