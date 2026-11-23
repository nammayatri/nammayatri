module SharedLogic.FleetOperatorV2
  ( FleetTms (..),
    FleetMode (..),
    fleetTms,
    fleetMode,
    getBmsBackendConfig,
    currentOperation,
    tripAction,
    currentTripDetails,
    activeTrip,
    currentTripNumber,
    recordTripNumber,
    clearTripNumber,
  )
where

import Data.List (sortOn)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime, minutesToTimeZone, utcToLocalTime)
import qualified Domain.Types.Extra.MerchantServiceConfig as DMSC
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Environment (Flow)
import qualified Environment as Fields
import qualified EulerHS.Types as ET
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id (Id, getId)
import Kernel.Utils.Common (fromMaybeM, logError, logWarning, throwError, withTryCatch)
import Kernel.Utils.Monitoring.Prometheus.Servant (SanitizedUrl)
import Lib.BmsBackend.Transit.Common (TmsError, callTms)
import qualified Lib.BmsBackend.Transit.Common as Fields
import qualified Lib.BmsBackend.Transit.Fleet.API as FleetAPI
import Lib.BmsBackend.Transit.Fleet.Types
  ( AnchorReq (..),
    CurrentOperationV2,
    CurrentTripDetailsV1Req (..),
    DutyView,
    FleetAnchorReq (..),
    TripActionV1 (..),
    TripActionV1Req (..),
    TripActionV2 (..),
    TripActionV2Req (..),
    TripDataV1,
  )
import qualified Lib.BmsBackend.Transit.Fleet.Types as Fields
import Lib.BmsBackend.Types (BmsBackendConfig)
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.GtfsDataServer.Types
  ( GimsActiveTripResp (..),
    GimsCurrentOperationResp (..),
    GimsCurrentTripDetailsReq (..),
    GimsCurrentTripDetailsResp (..),
    GimsOperationAnchor (..),
    GimsTripAction (..),
    GimsTripActionReq (..),
    GimsTripInfo (..),
  )
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Tools.Error (GenericError (InternalError), MerchantError (MerchantServiceConfigNotFound))
import qualified Tools.Fields as Fields

data FleetMode = FleetV1 | FleetV2 CurrentOperationV2

data FleetTms = FleetTms
  { bmsConfig :: BmsBackendConfig,
    feedId :: Text,
    actorId :: Maybe Text,
    mode :: FleetMode
  }

fleetTms :: Id DMOC.MerchantOperatingCity -> Text -> Maybe (Id DP.Person) -> GimsOperationAnchor -> Flow FleetTms
fleetTms merchantOpCityId gtfsFeedId mbPersonId anchor = do
  cfg <- serviceConfig DMSC.Tms merchantOpCityId
  let base = FleetTms {bmsConfig = cfg, feedId = gtfsFeedId, actorId = getId <$> mbPersonId, mode = FleetV1}
  resolvedMode <- fleetMode base anchor
  pure base {mode = resolvedMode}

fleetMode :: FleetTms -> GimsOperationAnchor -> Flow FleetMode
fleetMode ctx anchor = do
  result <-
    withTryCatch "FleetOperatorV2:fleetMode" $
      callFleet ctx "fleetCurrentOperationV2" FleetAPI.currentOperationV2API $
        FleetAPI.currentOperationV2
          ctx.feedId
          AnchorReq
            { vehicleNumber = anchor.vehicleNumber,
              driverToken = anchor.gimsDriverId,
              conductorToken = anchor.gimsConductorId,
              dutyGroupId = Nothing
            }
  case result of
    Right op | isLiveRun op -> pure (FleetV2 op)
    Right _ -> pure FleetV1
    Left err -> do
      unless (isNotFound err) $ logWarning $ "FleetOperatorV2: v2 run lookup failed, using v1 - " <> show err
      pure FleetV1

isLiveRun :: CurrentOperationV2 -> Bool
isLiveRun op = isJust op.active || any (isStatus "upcoming") op.upcoming

isNotFound :: SomeException -> Bool
isNotFound err = maybe False (\tmsErr -> tmsErr.status == 404) (fromException err :: Maybe TmsError)

getBmsBackendConfig :: Id DMOC.MerchantOperatingCity -> Flow BmsBackendConfig
getBmsBackendConfig = serviceConfig DMSC.DtcBms

serviceConfig :: DMSC.BmsProvider -> Id DMOC.MerchantOperatingCity -> Flow BmsBackendConfig
serviceConfig provider merchantOpCityId = do
  merchantServiceConfig <-
    getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, merchantId = Nothing, serviceName = Just (DMSC.BmsService provider)}) Nothing
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantOpCityId.getId "Bms" (show provider))
  case merchantServiceConfig.serviceConfig of
    DMSC.BmsServiceConfig cfg -> pure cfg
    _ -> throwError $ InternalError "Unknown Service Config"

callFleet :: (SanitizedUrl api, ToJSON a) => FleetTms -> Text -> Proxy api -> (Maybe Text -> Maybe Text -> ET.EulerClient a) -> Flow a
callFleet ctx apiName proxy client = callTms ctx.bmsConfig apiName proxy (client (Just "*") ctx.actorId)

toFleetAnchor :: GimsOperationAnchor -> FleetAnchorReq
toFleetAnchor anchor =
  FleetAnchorReq
    { conductorToken = anchor.gimsConductorId,
      driverToken = anchor.gimsDriverId,
      vehicleNumber = anchor.vehicleNumber
    }

isStatus :: Text -> DutyView -> Bool
isStatus wanted duty = duty.status == wanted

runDuties :: CurrentOperationV2 -> [DutyView]
runDuties op = sortOn (.tripOrder) (op.history <> maybeToList op.active <> op.upcoming)

operableDuties :: CurrentOperationV2 -> [DutyView]
operableDuties = filter (\duty -> not (isStatus "cancelled" duty || isStatus "skipped" duty)) . runDuties

currentOperation :: FleetTms -> GimsOperationAnchor -> Flow GimsCurrentOperationResp
currentOperation ctx anchor = case ctx.mode of
  FleetV2 op -> do
    let tripNumbers = map (.tripNumber) (operableDuties op)
    pure
      GimsCurrentOperationResp
        { waybill_no = op.waybillNo,
          number_of_trips = length tripNumbers,
          trip_numbers = Just tripNumbers,
          gimsConductorId = op.conductorToken,
          gimsDriverId = op.driverToken
        }
  FleetV1 -> do
    resp <- callFleet ctx "fleetCurrentOperationV1" FleetAPI.currentOperationV1API (FleetAPI.currentOperationV1 ctx.feedId (toFleetAnchor anchor))
    pure
      GimsCurrentOperationResp
        { waybill_no = resp.waybillNo,
          number_of_trips = resp.numberOfTrips,
          trip_numbers = Just resp.tripNumbers,
          gimsConductorId = resp.conductorToken,
          gimsDriverId = resp.driverToken
        }

tripAction :: FleetTms -> GimsTripActionReq -> Flow ()
tripAction ctx req = case ctx.mode of
  FleetV2 op -> forM_ (toTripActionV2 req.action) $ \v2Action ->
    void $
      callFleet ctx "fleetTripActionV2" FleetAPI.tripActionV2API $
        FleetAPI.tripActionV2
          ctx.feedId
          TripActionV2Req
            { vehicleNumber = Nothing,
              driverToken = Nothing,
              conductorToken = Nothing,
              dutyGroupId = Just op.dutyGroupId,
              action = v2Action,
              tripNumber = if v2Action == ActionRollback then Nothing else req.tripNumber,
              timestamp = fromIntegral <$> req.timestamp,
              reason = Nothing
            }
  FleetV1 -> do
    resp <-
      callFleet ctx "fleetTripActionV1" FleetAPI.tripActionV1API $
        FleetAPI.tripActionV1
          ctx.feedId
          TripActionV1Req
            { action = toTripActionV1 req.action,
              tripNumber = req.tripNumber,
              timestamp = fromIntegral <$> req.timestamp,
              conductorToken = req.gimsConductorId,
              driverToken = req.gimsDriverId,
              vehicleNumber = req.vehicleNumber
            }
    unless resp.success $ throwError $ InternalError "Transit trip action was not applied"

toTripActionV1 :: GimsTripAction -> TripActionV1
toTripActionV1 = \case
  GimsTripActionStart -> V1Start
  GimsTripActionEnd -> V1End
  GimsTripActionReset -> V1Reset
  GimsTripActionRollback -> V1Start

toTripActionV2 :: GimsTripAction -> Maybe TripActionV2
toTripActionV2 = \case
  GimsTripActionStart -> Just ActionStart
  GimsTripActionEnd -> Just ActionEnd
  GimsTripActionRollback -> Just ActionRollback
  GimsTripActionReset -> Nothing

currentTripDetails :: FleetTms -> GimsCurrentTripDetailsReq -> Flow GimsCurrentTripDetailsResp
currentTripDetails ctx req = case ctx.mode of
  FleetV2 op ->
    pure
      GimsCurrentTripDetailsResp
        { waybillNo = op.waybillNo,
          vehicleNumber = fromMaybe "" op.vehicleNumber,
          gimsConductorId = op.conductorToken,
          gimsDriverId = op.driverToken,
          history = map (dutyTripInfo op) op.history,
          current = dutyTripInfo op <$> op.active,
          upcoming = map (dutyTripInfo op) op.upcoming
        }
  FleetV1 -> do
    resp <-
      callFleet ctx "fleetCurrentTripDetailsV1" FleetAPI.currentTripDetailsV1API $
        FleetAPI.currentTripDetailsV1
          ctx.feedId
          CurrentTripDetailsV1Req
            { previousTripNumber = req.previousTripNumber,
              conductorToken = req.gimsConductorId,
              driverToken = req.gimsDriverId,
              vehicleNumber = req.vehicleNumber
            }
    pure
      GimsCurrentTripDetailsResp
        { waybillNo = resp.waybillNo,
          vehicleNumber = resp.vehicleNumber,
          gimsConductorId = resp.conductorToken,
          gimsDriverId = resp.driverToken,
          history = map tripInfo resp.history,
          current = tripInfo <$> resp.current,
          upcoming = map tripInfo resp.upcoming
        }

tripInfo :: TripDataV1 -> GimsTripInfo
tripInfo trip =
  let rid = fromMaybe "" trip.routeId
   in GimsTripInfo
        { trip_number = trip.tripNumber,
          route_id = rid,
          route_number = fromMaybe rid trip.routeNumber,
          route_name = fromMaybe "" trip.routeName,
          is_active_trip = trip.isActiveTrip,
          duty_date = trip.dutyDate,
          start_time = trip.startTime,
          end_time = trip.endTime
        }

dutyTripInfo :: CurrentOperationV2 -> DutyView -> GimsTripInfo
dutyTripInfo op duty =
  GimsTripInfo
    { trip_number = duty.tripNumber,
      route_id = duty.routeId,
      route_number = duty.routeId,
      route_name = "",
      is_active_trip = isStatus "active" duty,
      duty_date = Just (show op.operationDate),
      start_time = Just (istClock duty.scheduledStartAt),
      end_time = Just (istClock duty.scheduledEndAt)
    }

istClock :: UTCTime -> Text
istClock = T.pack . formatTime defaultTimeLocale "%H:%M" . utcToLocalTime (minutesToTimeZone 330)

activeTrip :: FleetTms -> GimsOperationAnchor -> Flow (Maybe GimsActiveTripResp)
activeTrip ctx anchor = case ctx.mode of
  FleetV2 op ->
    pure $
      Just
        GimsActiveTripResp
          { waybill_no = op.waybillNo,
            active_trip_number = (.tripNumber) <$> op.active,
            route_id = (.routeId) <$> op.active
          }
  FleetV1 -> do
    result <- withTryCatch "FleetOperatorV2:activeTrip" $ callFleet ctx "fleetActiveTripV1" FleetAPI.activeTripV1API (FleetAPI.activeTripV1 ctx.feedId (toFleetAnchor anchor))
    case result of
      Right resp ->
        pure $
          Just
            GimsActiveTripResp
              { waybill_no = resp.waybillNo,
                active_trip_number = resp.activeTripNumber,
                route_id = resp.routeId
              }
      Left err -> do
        logError $ "Error getting active trip: " <> show err
        pure Nothing

currentTripNumber :: FleetTms -> Text -> Flow Int
currentTripNumber ctx redisKey = case ctx.mode of
  FleetV2 op -> pure $ maybe 0 (.tripNumber) (lastMay (filter (isJust . (.recordedStartTime)) (operableDuties op)))
  FleetV1 -> fromMaybe 0 <$> Hedis.get redisKey

recordTripNumber :: FleetTms -> Text -> Int -> Flow ()
recordTripNumber ctx redisKey tripNo = case ctx.mode of
  FleetV2 _ -> pure ()
  FleetV1 -> Hedis.setExp redisKey tripNo 172800

clearTripNumber :: FleetTms -> Text -> Flow ()
clearTripNumber ctx redisKey = case ctx.mode of
  FleetV2 _ -> pure ()
  FleetV1 -> void $ Hedis.del redisKey
