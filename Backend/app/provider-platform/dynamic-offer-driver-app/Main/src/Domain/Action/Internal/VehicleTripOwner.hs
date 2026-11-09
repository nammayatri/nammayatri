{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.VehicleTripOwner
  ( VehicleTripOwnerReq (..),
    WaybillChangedReq (..),
    resolveVehicleTripOwner,
    waybillChanged,
  )
where

import BecknV2.FRFS.Enums (VehicleCategory (..))
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Domain.Types.Merchant (Merchant)
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error (MerchantError (..))
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (fromMaybeM, getCurrentTime, logInfo, throwError)
import qualified Lib.GtfsDataServer.Flow as NandiFlow
import Lib.GtfsDataServer.Types
import SharedLogic.IntegratedBPPConfig (findIbppConfigByCityVehicleAndFeedKey, getGimsBaseUrl)
import SharedLogic.VehicleTripOwner
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error (GenericError (InvalidRequest))

data VehicleTripOwnerReq = VehicleTripOwnerReq
  { merchantOperatingCityId :: Text,
    gtfsId :: Text,
    vehicleNumber :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

resolveVehicleTripOwner :: Maybe Text -> VehicleTripOwnerReq -> Flow VehicleTripOwner
resolveVehicleTripOwner apiKey req = do
  locationTrackingServiceKey <- asks (.locationTrackingServiceKey)
  unless (apiKey == Just locationTrackingServiceKey) $ throwError $ InvalidRequest "Invalid API key"
  integratedBPPConfig <- findIbppConfigByCityVehicleAndFeedKey (Id req.merchantOperatingCityId) (show BUS) req.gtfsId
  syncOwnerFromBus integratedBPPConfig (DIBC.feedKey integratedBPPConfig) (T.strip req.vehicleNumber)

syncOwnerFromBus :: DIBC.IntegratedBPPConfig -> Text -> Text -> Flow VehicleTripOwner
syncOwnerFromBus integratedBPPConfig feedKey busNo = do
  existing <- withVehicleTripOwnerRedis $ Hedis.get ownerKey
  busLiveTrip integratedBPPConfig feedKey busNo >>= \case
    Nothing -> markBusIdle existing
    Just (details, trip) -> writeLiveCrew existing details trip >>= maybe (markBusIdle existing) pure
  where
    ownerKey = vehicleTripOwnerKey feedKey busNo

    writeLiveCrew :: Maybe VehicleTripOwner -> GimsCurrentTripDetailsResp -> GimsTripInfo -> Flow (Maybe VehicleTripOwner)
    writeLiveCrew existing details trip = do
      let sameWaybill = (existing >>= (.waybillNo)) == Just details.waybillNo
      ttl <- if sameWaybill then remainingTtl else pure activeOwnerTtl
      setActiveTripOwner
        integratedBPPConfig
        feedKey
        []
        TripOwnerTarget
          { targetWaybillNo = details.waybillNo,
            targetVehicleNumber = busNo,
            crewBadges = catMaybes [details.gimsDriverId, details.gimsConductorId]
          }
        trip.trip_number
        ttl

    remainingTtl :: Flow Int
    remainingTtl = do
      ttl <- withVehicleTripOwnerRedis $ Hedis.ttl ownerKey
      pure $ if ttl > 0 then fromIntegral ttl else activeOwnerTtl

    markBusIdle :: Maybe VehicleTripOwner -> Flow VehicleTripOwner
    markBusIdle existing = do
      now <- getCurrentTime
      let wasActive = maybe False (.activeTrip) existing
          ttl = if wasActive then noActiveTripTtl else unresolvedNoActiveTripTtl
      withVehicleTripOwnerRedis $ Hedis.setExp ownerKey (noActiveTripOwner now) ttl
      logInfo $ "VehicleTripOwner: no active trip on the bus - " <> ownerKey
      pure (noActiveTripOwner now)

busLiveTrip :: DIBC.IntegratedBPPConfig -> Text -> Text -> Flow (Maybe (GimsCurrentTripDetailsResp, GimsTripInfo))
busLiveTrip integratedBPPConfig feedKey busNo = do
  baseUrl <- getGimsBaseUrl integratedBPPConfig
  mbDetails <-
    NandiFlow.gimsCurrentTripDetailsIfOnline baseUrl feedKey $
      GimsCurrentTripDetailsReq {previousTripNumber = 0, gimsConductorId = Nothing, gimsDriverId = Nothing, vehicleNumber = Just busNo}
  pure $ do
    details <- mbDetails
    guard (T.strip details.vehicleNumber == busNo)
    trip <- find (.is_active_trip) (details.upcoming <> maybe [] pure details.current <> details.history)
    pure (details, trip)

unresolvedNoActiveTripTtl :: Int
unresolvedNoActiveTripTtl = 600

data WaybillChangedReq = WaybillChangedReq
  { city :: Context.City,
    gtfsId :: Text,
    waybillNo :: Text,
    status :: Maybe Text,
    vehicleNumber :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

waybillChanged :: Id Merchant -> Maybe Text -> WaybillChangedReq -> Flow APISuccess
waybillChanged merchantId apiKey req = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $ throwError $ InvalidRequest "Invalid BPP internal api key"
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchantId req.city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchantId.getId <> "-city-" <> show req.city)
  integratedBPPConfig <- findIbppConfigByCityVehicleAndFeedKey merchantOpCity.id (show BUS) req.gtfsId
  let feedKey = DIBC.feedKey integratedBPPConfig
      busNo = T.strip req.vehicleNumber
  existing <- withVehicleTripOwnerRedis $ Hedis.get (vehicleTripOwnerKey feedKey busNo)
  let heldByThisWaybill = maybe False (\owner -> owner.activeTrip && owner.waybillNo == Just req.waybillNo) (existing :: Maybe VehicleTripOwner)
  when heldByThisWaybill $ void $ syncOwnerFromBus integratedBPPConfig feedKey busNo
  pure Success
