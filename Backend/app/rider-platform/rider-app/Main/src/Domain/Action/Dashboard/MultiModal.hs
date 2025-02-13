{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.MultiModal (postMultiModalMultimodalFrfsDataPreprocess, postMultiModalMultimodalFrfsDataStatus, postMultiModalMultimodalFrfsDataVersionIsReady, postMultiModalMultimodalFrfsDataVersionApply) where

import qualified API.Types.RiderPlatform.Management.MultiModal
import qualified API.Types.RiderPlatform.Management.MultiModal as MTypes
import qualified AWS.S3 as S3
import qualified BecknV2.FRFS.Enums
import qualified Data.ByteString as BS
import qualified Data.HashMap.Lazy as HM
import Data.List (sortOn)
import qualified Data.Text as T
import Domain.Types.Extra.Rollout
import Domain.Types.GTFS
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Rollout
import qualified Domain.Types.Route as DR
import qualified Domain.Types.RouteStopMapping as DRSM
import qualified Domain.Types.Stage as DStage
import Domain.Types.Station
import qualified Domain.Types.Version as DVersion
import qualified Domain.Types.VersionStageMapping as DVSMapping
import qualified Environment
import qualified EulerHS.Language as L
import EulerHS.Types (base64Encode)
import Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Types.TimeBound
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Version as CQV
import qualified Storage.Queries.Rollout as QRollout
import qualified Storage.Queries.Route as QR
import qualified Storage.Queries.RouteStopMapping as QRSM
import qualified Storage.Queries.Stage as QStage
import qualified Storage.Queries.Station as QS
import qualified Storage.Queries.Version as QV
import qualified Storage.Queries.VersionStageMapping as QVSM
import System.Exit (ExitCode (..))
import System.Process
import Tools.Error

postMultiModalMultimodalFrfsDataPreprocess :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.MultiModal.PreprocessFRFSDataReq -> Environment.Flow API.Types.RiderPlatform.Management.MultiModal.PreprocessFRFSDataResp)
postMultiModalMultimodalFrfsDataPreprocess merchantShortId opCity req = do
  moc <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId-" <> merchantShortId.getShortId <> " opCity-" <> show opCity)
  case req.inputDataType of
    MTypes.GTFS_DATA -> case req.fileFormat of
      MTypes.GTFS -> do
        versionId <- generateGUID
        now <- getCurrentTime
        filePath <- S3.createFilePath "/multimodal/" ("version-" <> versionId) S3.Zip "zip"
        oldVersions <- CQV.findAllReadyToApplyByMerchantOperatingCityAndVehicleTypeAndDataType (Just moc.id) req.vehicleType Domain.Types.Extra.Rollout.GTFS False
        let oldVersionTag = fromMaybe 0 (getMaxVersionTag oldVersions)
            version = DVersion.Version {id = Kernel.Types.Id.Id versionId, inputDataType = Domain.Types.Extra.Rollout.GTFS, isReadyToApply = False, vehicleType = req.vehicleType, versionTag = oldVersionTag + 1, gtfsLink = Just filePath, merchantId = Just moc.merchantId, merchantOperatingCityId = Just moc.id, createdAt = now, updatedAt = now}
        void $ QV.create version
        stages <- QStage.findByMerchantOperatingCityAndVehicleTypeAndInputDataType (Just moc.id) req.vehicleType Domain.Types.Extra.Rollout.GTFS
        let sortedStages = sortOn (.order) stages
        let outputFile = "cleaned-" <> req.file
        fork "Processing Stages" $ processStages sortedStages version.id req.file outputFile filePath
        pure $
          MTypes.PreprocessFRFSDataResp
            { versionId = versionId,
              versionTag = oldVersionTag + 1
            }
      MTypes.JSON -> error "Logic yet to be decided"
      MTypes.CSV -> error "Logic yet to be decided"
    MTypes.FARE_DATA -> error "Logic yet to be decided"
  where
    getMaxVersionTag :: [DVersion.Version] -> Maybe Int
    getMaxVersionTag [] = Nothing
    getMaxVersionTag versions = Just . maximum $ map (.versionTag) versions

    processStages :: [DStage.Stage] -> Kernel.Types.Id.Id DVersion.Version -> FilePath -> FilePath -> Text -> Environment.Flow ()
    processStages [] _ _ _ _ = pure ()
    processStages (stage : stages) versionId inputFile outputFile uploadFilePath = do
      isSuccess <- processStage stage.name versionId inputFile outputFile uploadFilePath stage
      bool (processStages stages versionId inputFile outputFile uploadFilePath) (pure ()) isSuccess

    processStage :: DStage.StageName -> Kernel.Types.Id.Id DVersion.Version -> FilePath -> FilePath -> Text -> DStage.Stage -> Environment.Flow (Bool)
    processStage DStage.PREPROCESSING _ _ _ _ _ = error "Logic yet to be decided"
    processStage DStage.VALIDATION versionId inputFile outputFile _ stage = do
      vsmId <- generateGUID
      now <- getCurrentTime
      let vsMapping = DVSMapping.VersionStageMapping {id = vsmId, versionId = versionId.getId, stageId = stage.id.getId, status = DVSMapping.Inprogress, stageData = Nothing, failureReason = Nothing, stageName = show stage.name, updatedAt = now, createdAt = now}
      void $ QVSM.create vsMapping
      isValid <- validateGTFSData inputFile outputFile vsmId
      when isValid $ void $ QVSM.updateSuccessById DVSMapping.Completed vsmId
      pure isValid
    processStage DStage.UPLOAD versionId _ outputFile uploadFilePath stage = do
      vsmId <- generateGUID
      now <- getCurrentTime
      let vsMapping = DVSMapping.VersionStageMapping {id = vsmId, versionId = versionId.getId, stageId = stage.id.getId, status = DVSMapping.Inprogress, stageData = Nothing, failureReason = Nothing, stageName = show stage.name, updatedAt = now, createdAt = now}
      void $ QVSM.create vsMapping
      void $ uploadGTFSData outputFile uploadFilePath vsmId
      void $ QVSM.updateSuccessById DVSMapping.Completed vsmId
      pure True

    validateGTFSData :: FilePath -> FilePath -> Kernel.Types.Id.Id DVSMapping.VersionStageMapping -> Environment.Flow (Bool)
    validateGTFSData inputFile outputFile vsmId = do
      let validateCommand = "gtfstidy"
          args = ["-v", inputFile]
      (exitCode, _, errorText) <- liftIO $ readProcessWithExitCode validateCommand args ""
      case exitCode of
        ExitSuccess -> do
          let cleanupCommand = "gtfstidy --remove-red-trips --remove-red-stops --drop-shapes -o " <> outputFile <> " " <> inputFile
          liftIO $ callCommand cleanupCommand
          pure True
        ExitFailure code -> do
          void $ QVSM.updateFailureById (Just $ "GTFS validation failed with exit code " <> show code <> ":" <> show errorText) DVSMapping.Failed vsmId
          pure False

    uploadGTFSData :: FilePath -> Text -> Kernel.Types.Id.Id DVSMapping.VersionStageMapping -> Environment.Flow ()
    uploadGTFSData outputFile filePath vsmId = do
      zipFile <- L.runIO $ base64Encode <$> BS.readFile outputFile
      void $ S3.put (T.unpack filePath) zipFile
      void $ QVSM.updateSuccessById DVSMapping.Completed vsmId

-- Get All Stages for Data Type and City
-- Iterate on each stage, till reching the end, and create next stage if current is completed
-- PREPROCESSING -> Fail : Flag Failure in seperate `anomaly` column in the CUSTOM CSVs and upload of S3
--               -> Success: Create GTFS and move to next stage.
-- VALIDATION    -> Fail : Flag Failure in seperate `anomaly` column in the GTFS CSVs and upload of S3
--               -> Success: move to next stage.
-- UPLOAD        -> Fail : Flag Failure in Failure Reason.
--               -> Success: Done.

postMultiModalMultimodalFrfsDataStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.MultiModal.FRFSDataStatusReq -> Environment.Flow API.Types.RiderPlatform.Management.MultiModal.FRFSDataStatusResp)
postMultiModalMultimodalFrfsDataStatus _ _ req = do
  versionStageMappings <- QVSM.findAllByVersionId req.versionId
  stageData <- traverse mkFRFSDataStatus versionStageMappings
  return $ API.Types.RiderPlatform.Management.MultiModal.FRFSDataStatusResp req.versionId stageData

mkFRFSDataStatus :: (MonadFlow m) => DVSMapping.VersionStageMapping -> m API.Types.RiderPlatform.Management.MultiModal.StageInfo
mkFRFSDataStatus mapping = return $ API.Types.RiderPlatform.Management.MultiModal.StageInfo mapping.stageName (mapStatus mapping.status)

mapStatus :: DVSMapping.Status -> API.Types.RiderPlatform.Management.MultiModal.StageStatus
mapStatus DVSMapping.Inprogress = API.Types.RiderPlatform.Management.MultiModal.INPROGRESS
mapStatus DVSMapping.Completed = API.Types.RiderPlatform.Management.MultiModal.COMPLETED
mapStatus DVSMapping.Failed = API.Types.RiderPlatform.Management.MultiModal.FAILED

postMultiModalMultimodalFrfsDataVersionIsReady :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.MultiModal.ReadyVersionReq -> Environment.Flow API.Types.RiderPlatform.Management.MultiModal.ReadyVersionsResp)
postMultiModalMultimodalFrfsDataVersionIsReady _merchantShortId _opCity req = do
  readyVersions <- QV.findAllReadyToApplyByMerchantOperatingCityAndVehicleTypeAndDataType True (Just $ Kernel.Types.Id.Id req.cityId) req.vehicleType (mapInputDataType req.inputDataType)
  versionLists <- traverse mkReadyVersionList readyVersions
  return $ API.Types.RiderPlatform.Management.MultiModal.ReadyVersionsResp versionLists

mapInputDataType :: API.Types.RiderPlatform.Management.MultiModal.RawDataType -> RawDataType
mapInputDataType API.Types.RiderPlatform.Management.MultiModal.GTFS_DATA = GTFS
mapInputDataType API.Types.RiderPlatform.Management.MultiModal.FARE_DATA = FARE

mkReadyVersionList :: (MonadFlow m) => DVersion.Version -> m API.Types.RiderPlatform.Management.MultiModal.PreprocessFRFSDataResp
mkReadyVersionList version = return $ API.Types.RiderPlatform.Management.MultiModal.PreprocessFRFSDataResp version.id.getId version.versionTag

postMultiModalMultimodalFrfsDataVersionApply :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.MultiModal.ApplyVersionReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMultiModalMultimodalFrfsDataVersionApply _merchantShortId _opCity req = do
  unless (req.rolloutPercent >= 1 && req.rolloutPercent <= 100) $ throwError $ InternalError $ "Invalid rolloutPercent: " <> show req.rolloutPercent
  version <- QV.findByPrimaryKey (Kernel.Types.Id.Id req.versionId) >>= fromMaybeM (InternalError $ "Version with id: " <> show req.versionId <> " not found.")
  unless version.isReadyToApply $ throwError $ InternalError $ "Version with id: " <> show req.versionId <> " is not ready to apply."
  now <- getCurrentTime
  rolloutVersions <- QRollout.findAllByMerchantOperatingCityAndVehicleType (Just $ Kernel.Types.Id.Id req.cityId) req.vehicleType
  oldRolloutVersion <- maybe (throwError $ InternalError $ "No rollout version found for city: " <> show req.cityId <> " and vehicleType: " <> show req.vehicleType) return (listToMaybe <$> filter (\v -> v.versionTag /= req.versionTag) $ rolloutVersions)
  let mbNewRolloutVersion = listToMaybe <$> filter (\v -> v.versionTag == req.versionTag) $ rolloutVersions
      updatedOldVersion = oldRolloutVersion {Domain.Types.Rollout.percentageRollout = 100 - req.rolloutPercent, Domain.Types.Rollout.updatedAt = now}
  case mbNewRolloutVersion of
    Nothing -> do
      id <- generateGUID
      let newVersion = Domain.Types.Rollout.Rollout {id, inputDataType = version.inputDataType, vehicleType = req.vehicleType, versionTag = req.versionTag, merchantId = version.merchantId, merchantOperatingCityId = version.merchantOperatingCityId, createdAt = now, percentageRollout = req.rolloutPercent, updatedAt = now}
      void $ QRollout.create newVersion
    Just newRolloutVersion -> do
      let newVersion = newRolloutVersion {Domain.Types.Rollout.percentageRollout = req.rolloutPercent, Domain.Types.Rollout.updatedAt = now}
      void $ QRollout.updateByPrimaryKey newVersion
  void $ if updatedOldVersion.percentageRollout == 0 then QRollout.deleteByVersionId updatedOldVersion.id else QRollout.updateByPrimaryKey updatedOldVersion
  return Kernel.Types.APISuccess.Success

_convertBusGTFSToQueries :: BusGTFS -> m ()
_convertBusGTFSToQueries _gtfs = do
  error "Logic yet to be decided"

_convertMetroGTFSToQueries :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Merchant -> Kernel.Types.Id.Id MerchantOperatingCity -> MetroGTFS -> Int -> m ()
_convertMetroGTFSToQueries merchantId mocId gtfs versionTag = do
  when (length gtfs.mgFeed.f_stops == 0) $ throwError $ InternalError "No stops found in the GTFS file"
  when (length gtfs.mgFeed.f_routes == 0) $ throwError $ InternalError "No routes found in the GTFS file"
  let routeStopMapping = map (mkRouteStopMapping gtfs.mgFeed.f_trips gtfs.mgFeed.f_stops gtfs.mgFeed.f_stop_times) gtfs.mgFeed.f_routes
  when (length routeStopMapping == 0) $ throwError $ InternalError "No stop times found in the GTFS file"
  stations <- gtfsToDomainStopsList merchantId mocId gtfs.mgFeed.f_stops versionTag
  routes <- gtfsToDomainRouteList merchantId mocId routeStopMapping versionTag
  domainRouteStopMappings <- gtfsToDomainRouteStopMappingList merchantId mocId routeStopMapping versionTag
  void $ QS.createMany stations
  void $ QR.createMany routes
  void $ QRSM.createMany domainRouteStopMappings

gtfsToDomainStopsList :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Merchant -> Kernel.Types.Id.Id MerchantOperatingCity -> [Stop] -> Int -> m [Station]
gtfsToDomainStopsList merchantId mocId stops versionTag = mapM (gtfsToDomainStop merchantId mocId (Just versionTag)) stops

gtfsToDomainStop :: (MonadFlow m) => Kernel.Types.Id.Id Merchant -> Kernel.Types.Id.Id MerchantOperatingCity -> Maybe Int -> Stop -> m Station
gtfsToDomainStop merchantId merchantOperatingCityId versionTag Stop {..} = do
  id <- generateGUID
  now <- getCurrentTime
  pure
    Station
      { address = Nothing,
        code = s_stop_id,
        id,
        lat = Just s_stop_lat,
        lon = Just s_stop_lon,
        merchantId,
        merchantOperatingCityId,
        name = s_stop_name,
        possibleTypes = Nothing,
        timeBounds = Unbounded,
        vehicleType = BecknV2.FRFS.Enums.METRO, -- make generic?
        versionTag,
        createdAt = now,
        updatedAt = now
      }

gtfsToDomainRouteList :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Merchant -> Kernel.Types.Id.Id MerchantOperatingCity -> [RouteStopMapping] -> Int -> m [DR.Route]
gtfsToDomainRouteList merchantId mocId gtfsRouteStopMapping versionTag = mapM (gtfsToDomainRoute merchantId mocId (Just versionTag)) gtfsRouteStopMapping

gtfsToDomainRoute :: (MonadFlow m) => Kernel.Types.Id.Id Merchant -> Kernel.Types.Id.Id MerchantOperatingCity -> Maybe Int -> RouteStopMapping -> m DR.Route
gtfsToDomainRoute merchantId merchantOperatingCityId versionTag RouteStopMapping {..} = do
  id <- generateGUID
  now <- getCurrentTime
  firstStopTupple <- fromMaybeM (InternalError "No stops found for route") (listToMaybe stops)
  lastStopTupple <- fromMaybeM (InternalError "No stops found for route") ((listToMaybe . reverse) stops)
  let firstStop = (\(s, _, _) -> s) firstStopTupple
      lastStop = (\(s, _, _) -> s) lastStopTupple
  pure
    DR.Route
      { code = route.r_route_id,
        color = route.r_route_color,
        endPoint = LatLong firstStop.s_stop_lat firstStop.s_stop_lon,
        id,
        longName = route.r_route_long_name,
        merchantId,
        merchantOperatingCityId,
        polyline = route.r_polyline,
        shortName = route.r_route_short_name,
        startPoint = LatLong lastStop.s_stop_lat lastStop.s_stop_lon,
        timeBounds = Unbounded,
        vehicleType = BecknV2.FRFS.Enums.METRO,
        versionTag,
        createdAt = now,
        updatedAt = now
      }

gtfsToDomainRouteStopMappingList :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Merchant -> Kernel.Types.Id.Id MerchantOperatingCity -> [RouteStopMapping] -> Int -> m [DRSM.RouteStopMapping]
gtfsToDomainRouteStopMappingList merchantId mocId gtfsRouteStopMapping versionTag = concat <$> mapM processRouteStopMapping gtfsRouteStopMapping
  where
    processRouteStopMapping :: (MonadFlow m) => RouteStopMapping -> m [DRSM.RouteStopMapping]
    processRouteStopMapping routeStopMapping = do
      let route = routeStopMapping.route
          stops = routeStopMapping.stops
      mapM (gtfsToDomainRouteStopMapping merchantId mocId (Just versionTag) route) stops

gtfsToDomainRouteStopMapping :: (MonadFlow m) => Kernel.Types.Id.Id Merchant -> Kernel.Types.Id.Id MerchantOperatingCity -> Maybe Int -> Route -> (Stop, StopTime, Maybe Int) -> m DRSM.RouteStopMapping
gtfsToDomainRouteStopMapping merchantId merchantOperatingCityId versionTag Route {..} (Stop {..}, StopTime {..}, travelTime) = do
  now <- getCurrentTime
  pure
    DRSM.RouteStopMapping
      { estimatedTravelTimeFromPreviousStop = fmap Seconds travelTime,
        providerCode = r_route_id,
        routeCode = r_route_id,
        sequenceNum = st_stop_sequence,
        stopCode = s_stop_id,
        stopName = s_stop_name,
        stopPoint = LatLong s_stop_lat s_stop_lon,
        timeBounds = Unbounded,
        vehicleType = BecknV2.FRFS.Enums.METRO, -- make generic?
        createdAt = now,
        updatedAt = now,
        ..
      }

data RouteStopMapping = RouteStopMapping
  { route :: Route,
    stops :: [(Stop, StopTime, Maybe Int)]
  }
  deriving (Show)

--  stop.id -> stopTime.tripId -> trip.routeId -> route
mkRouteStopMapping :: [Trip] -> [Stop] -> [StopTime] -> Route -> RouteStopMapping
mkRouteStopMapping trips stops stopTimes route =
  let routeTrips = filter (\t -> t_route_id t == r_route_id route) trips
      tripIds = map t_trip_id routeTrips
      routeStopTimes = filter (\st -> st_trip_id st `elem` tripIds) stopTimes
      stopMap = HM.fromList [(s_stop_id s, s) | s <- stops]
      pairedStopAndTimes =
        mapMaybe
          ( \st -> do
              s <- HM.lookup (st_stop_id st) stopMap
              pure (s, st)
          )
          routeStopTimes
      sortedStops = sortOn (st_stop_sequence . snd) pairedStopAndTimes
      travelTimes = case sortedStops of
        [] -> []
        [_] -> [Nothing]
        _ ->
          let pairs = zip sortedStops (tail sortedStops)
              times = map (\((_, curr), (_, next)) -> Just $ calculateTravelTime curr next) pairs
           in [Nothing] ++ times -- First (origin) stop has no travel time
      stopsWithTravel = zipWith (\(s, t) tt -> (s, t, tt)) sortedStops travelTimes
   in RouteStopMapping route stopsWithTravel

calculateTravelTime :: StopTime -> StopTime -> Int
calculateTravelTime curr next =
  let toSeconds (Time h m s) = h * 3600 + m * 60 + s
      departure = toSeconds (st_departure_time curr)
      arrival = toSeconds (st_arrival_time next)
   in arrival - departure
