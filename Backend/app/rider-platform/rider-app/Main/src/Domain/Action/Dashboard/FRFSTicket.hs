module Domain.Action.Dashboard.FRFSTicket
  ( getFRFSTicketFrfsRoutes,
    getFRFSTicketFrfsRouteFareList,
    putFRFSTicketFrfsRouteFareUpsert,
    putFRFSTicketFrfsRouteTypeUpsert,
    getFRFSTicketFrfsStageFareList,
    putFRFSTicketFrfsStageFareUpsert,
    getFRFSTicketFrfsRouteStations,
    postFRFSTicketFrfsStatusUpdate,
    getFRFSTicketFrfsGtfs,
  )
where

import qualified API.Types.RiderPlatform.Management.FRFSTicket
import qualified BecknV2.FRFS.Enums
import BecknV2.FRFS.Utils
import qualified Dashboard.Common as Common
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Csv
import Data.List (groupBy)
import qualified Data.Map.Strict as M
import qualified Data.Text
import qualified Data.Vector as V
import qualified Domain.Action.Internal.FRFS as InternalFRFS
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import qualified Domain.Types.FRFSGtfsStageFare as DFRFSGtfsStageFare
import qualified Domain.Types.FRFSQuoteCategoryType as DTFRFSQuoteCategoryType
import qualified Domain.Types.FRFSRouteTypeMapping as DFRFSRouteTypeMapping
import qualified Domain.Types.FRFSVehicleServiceTier as DFRFSVehicleServiceTier
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant
import qualified Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (find, groupBy, id, length, map, null)
import Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as DTB
import Kernel.Utils.Common (fromMaybeM, generateGUID, getCurrentTime, throwError)
import Kernel.Utils.Logging (logError, logInfo)
import qualified SharedLogic.FRFSUtils as FRFSUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.FRFSGtfsStageFare as CQFRFSGtfsStageFare
import qualified Storage.CachedQueries.FRFSRouteTypeMapping as CQFRFSRouteTypeMapping
import qualified Storage.CachedQueries.FRFSVehicleServiceTier as CQFRFSVehicleServiceTier
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Storage.Queries.FRFSFarePolicy as QFRFSFarePolicy
import qualified Storage.Queries.FRFSGtfsStageFare as QFRFSGtfsStageFare
import Storage.Queries.FRFSRouteFareProduct as QFRFSRouteFareProduct
import qualified Storage.Queries.FRFSRouteTypeMapping as QFRFSRouteTypeMapping
import qualified Storage.Queries.FRFSVehicleServiceTier as QFRFSVehicleServiceTier
import Storage.Queries.StopFare as QRSF

postFRFSTicketFrfsStatusUpdate :: (ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Maybe Text -> API.Types.RiderPlatform.Management.FRFSTicket.FRFSStatusUpdateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFRFSTicketFrfsStatusUpdate _merchantShortId _opCity _mbRequestorId req =
  InternalFRFS.frfsStatusUpdate $ InternalFRFS.FRFSStatusUpdateReq {bookingIds = map cast req.bookingIds}

gtfsDashboardWaitMaxSec :: Kernel.Prelude.Int
gtfsDashboardWaitMaxSec = 10

getFRFSTicketFrfsGtfs ::
  ( ShortId Domain.Types.Merchant.Merchant ->
    Kernel.Types.Beckn.Context.City ->
    Kernel.Prelude.Maybe (Id Common.IntegratedBPPConfig) ->
    Kernel.Prelude.Maybe Common.PlatformType ->
    BecknV2.FRFS.Enums.VehicleCategory ->
    Environment.Flow API.Types.RiderPlatform.Management.FRFSTicket.FRFSGtfsRes
  )
getFRFSTicketFrfsGtfs merchantShortId opCity mbIntegratedBppConfigId mbPlatformType vehicleType = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  uiRes <- FRFSTicketService.getFrfsGtfs (Kernel.Prelude.Nothing, merchant.id) (cast <$> mbIntegratedBppConfigId) (toDomainPlatformType <$> mbPlatformType) gtfsDashboardWaitMaxSec opCity vehicleType
  pure $ toDashboardGtfsRes uiRes
  where
    toDomainPlatformType Common.MULTIMODAL = DIBC.MULTIMODAL
    toDomainPlatformType Common.PARTNERORG = DIBC.PARTNERORG
    toDomainPlatformType Common.APPLICATION = DIBC.APPLICATION
    toDashboardGtfsRes r =
      API.Types.RiderPlatform.Management.FRFSTicket.FRFSGtfsRes
        { integratedBppConfigId = cast r.integratedBppConfigId,
          ready = r.ready,
          routes = map toRoute r.routes,
          stops = map toStop r.stops,
          routeStops = map toRouteStop r.routeStops,
          fares = map toFare r.fares
        }
    toRoute x = API.Types.RiderPlatform.Management.FRFSTicket.FRFSGtfsRouteAPI {code = x.code, shortName = x.shortName, vehicleType = x.vehicleType, variant = x.variant}
    toStop x = API.Types.RiderPlatform.Management.FRFSTicket.FRFSGtfsStopAPI {code = x.code, name = x.name, lat = x.lat, lon = x.lon}
    toRouteStop x = API.Types.RiderPlatform.Management.FRFSTicket.FRFSGtfsRouteStopAPI {routeCode = x.routeCode, stopCode = x.stopCode, sequenceNum = x.sequenceNum, stopType = x.stopType}
    toFare x = API.Types.RiderPlatform.Management.FRFSTicket.FRFSGtfsFareAPI {code = x.code, name = x.name, providerCode = x.providerCode, validityDuration = x.validityDuration}

getFRFSTicketFrfsRoutes :: (ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow [API.Types.RiderPlatform.Management.FRFSTicket.FRFSDashboardRouteAPI])
getFRFSTicketFrfsRoutes merchantShortId opCity searchStr limit offset vehicleType = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)

  integratedBPPConfigs <- SIBC.findAllIntegratedBPPConfig merchantOpCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleType) DIBC.APPLICATION

  SIBC.fetchAllIntegratedBPPConfigResult integratedBPPConfigs $ \integratedBPPConfig -> do
    routes <- case searchStr of
      Just str -> OTPRest.findAllMatchingRoutes (Just str) (Just limit) (Just offset) vehicleType integratedBPPConfig
      Nothing -> OTPRest.getRoutesByVehicleType integratedBPPConfig vehicleType

    frfsRoutes <- forM routes $ \rte -> do
      routeTypes <- buildRouteTypeAPIs integratedBPPConfig rte.code
      pure $
        API.Types.RiderPlatform.Management.FRFSTicket.FRFSDashboardRouteAPI
          { code = rte.code,
            shortName = rte.shortName,
            longName = rte.longName,
            startPoint = rte.startPoint,
            endPoint = rte.startPoint,
            integratedBppConfigId = cast integratedBPPConfig.id,
            routeTypes = routeTypes
          }
    pure frfsRoutes

buildRouteTypeAPIs :: DIBC.IntegratedBPPConfig -> Data.Text.Text -> Environment.Flow [API.Types.RiderPlatform.Management.FRFSTicket.FRFSRouteTypeAPI]
buildRouteTypeAPIs integratedBPPConfig routeCode = do
  mappings <- CQFRFSRouteTypeMapping.findAllByRouteCodeAndIntegratedBppConfigId routeCode integratedBPPConfig.id
  fmap catMaybes $
    forM mappings $ \mapping -> do
      mbVehicleServiceTier <- QFRFSVehicleServiceTier.findById mapping.vehicleServiceTierId
      pure $
        mbVehicleServiceTier <&> \vehicleServiceTier ->
          API.Types.RiderPlatform.Management.FRFSTicket.FRFSRouteTypeAPI
            { serviceTier = vehicleServiceTier._type,
              routeType = mapping.routeType
            }

getFRFSTicketFrfsRouteFareList :: (ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Text.Text -> Id Common.IntegratedBPPConfig -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow API.Types.RiderPlatform.Management.FRFSTicket.FRFSRouteFareAPI)
getFRFSTicketFrfsRouteFareList merchantShortId opCity routeCode integratedBPPConfigId vehicleType = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)

  merchantOperatingCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)

  integratedBPPConfig <- SIBC.findIntegratedBPPConfig (Just $ cast integratedBPPConfigId) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleType) DIBC.APPLICATION
  fetchedRoute <- OTPRest.getRouteByRouteId integratedBPPConfig routeCode >>= fromMaybeM (InvalidRequest "Invalid route code")

  -- TODO :: To be fixed properly to handle multi-dimensional Fare Product
  fareProducts <- QFRFSRouteFareProduct.findByRouteCode routeCode integratedBPPConfig.id
  fareProduct <- find (\fareProduct -> fareProduct.timeBounds == DTB.Unbounded && fareProduct.vehicleType == vehicleType) fareProducts & fromMaybeM (InternalError "FRFS Fare Product Not Found")
  farePolicy <- QFRFSFarePolicy.findById fareProduct.farePolicyId >>= fromMaybeM (InternalError $ "FRFS Fare Policy Not Found : " <> fareProduct.farePolicyId.getId)
  routeFares <- QRSF.findByRouteCode farePolicy.id

  let groupedFares = groupBy (\a b -> a.startStopCode == b.startStopCode) routeFares -- TODO: Sort the fares by startStopCode
  let sortedGroupedFares = sortBy (comparing (negate . length)) groupedFares
  stopFares <- forM sortedGroupedFares $ \fares -> do
    let maybeFirstFare = listToMaybe fares
    case maybeFirstFare of
      Nothing -> throwError (InvalidRequest "No fares found for the start stop")
      Just firstFare -> do
        startStop <- OTPRest.getStationByGtfsIdAndStopCode firstFare.startStopCode integratedBPPConfig >>= fromMaybeM (InvalidRequest $ "Invalid from station id: " <> firstFare.startStopCode <> " or integratedBPPConfigID: " <> integratedBPPConfig.id.getId)

        endStops <- forM fares $ \fare -> do
          endStop <- OTPRest.getStationByGtfsIdAndStopCode fare.endStopCode integratedBPPConfig >>= fromMaybeM (InvalidRequest $ "Invalid to station id: " <> fare.endStopCode <> " or integratedBPPConfigID: " <> integratedBPPConfig.id.getId)
          return
            API.Types.RiderPlatform.Management.FRFSTicket.FRFSEndStopsFareAPI
              { name = endStop.name,
                code = endStop.code,
                amount = fare.amount,
                currency = fare.currency,
                lat = endStop.lat,
                lon = endStop.lon
              }

        return
          API.Types.RiderPlatform.Management.FRFSTicket.FRFSStopFareMatrixAPI
            { startStop =
                API.Types.RiderPlatform.Management.FRFSTicket.FRFSStartStopsAPI
                  { name = startStop.name,
                    code = startStop.code,
                    lat = startStop.lat,
                    lon = startStop.lon
                  },
              endStops = endStops
            }

  let frfsRouteFare =
        API.Types.RiderPlatform.Management.FRFSTicket.FRFSRouteFareAPI
          { code = fetchedRoute.code,
            shortName = fetchedRoute.shortName,
            longName = fetchedRoute.longName,
            fares = stopFares
          }

  return frfsRouteFare

data FareUpdateCSVRow = FareUpdateCSVRow
  { routeCode :: Text,
    startStopCode :: Text,
    endStopCode :: Text,
    amount :: Text
  }

instance FromNamedRecord FareUpdateCSVRow where
  parseNamedRecord r =
    FareUpdateCSVRow
      <$> r .: "Route ID"
      <*> r .: "Start Stop ID"
      <*> r .: "End Stop ID"
      <*> r .: "Price (In Rupees)"

data UpsertRouteFareResp = UpsertRouteFareResp {unprocessedRouteFares :: [Kernel.Prelude.Text], success :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

putFRFSTicketFrfsRouteFareUpsert :: (ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Text.Text -> Id Common.IntegratedBPPConfig -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.RiderPlatform.Management.FRFSTicket.UpsertRouteFareReq -> Environment.Flow API.Types.RiderPlatform.Management.FRFSTicket.UpsertRouteFareResp)
putFRFSTicketFrfsRouteFareUpsert merchantShortId opCity _routeCode integratedBPPConfigId vehicleType req = do
  fareUpdates <- readCsv req.file

  unprocessedFares <- forM fareUpdates $ \row -> do
    let amountVal = row.amount
    case highPrecMoneyFromText amountVal of
      Nothing -> do
        let message = "Invalid amount format for route " <> row.routeCode <> " (" <> row.startStopCode <> " -> " <> row.endStopCode <> ")"
        throwError (InvalidRequest message)
      Just value -> do
        merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)

        merchantOperatingCity <-
          CQMOC.findByMerchantIdAndCity merchant.id opCity
            >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)

        integratedBPPConfig <- SIBC.findIntegratedBPPConfig (Just $ cast integratedBPPConfigId) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleType) DIBC.APPLICATION
        -- TODO :: To be fixed properly to handle multi-dimensional Fare Product
        fareProducts <- QFRFSRouteFareProduct.findByRouteCode row.routeCode integratedBPPConfig.id
        fareProduct <- find (\fareProduct -> fareProduct.timeBounds == DTB.Unbounded && fareProduct.vehicleType == vehicleType) fareProducts & fromMaybeM (InternalError "FRFS Fare Product Not Found")
        farePolicy <- QFRFSFarePolicy.findById fareProduct.farePolicyId >>= fromMaybeM (InternalError $ "FRFS Fare Policy Not Found : " <> fareProduct.farePolicyId.getId)
        existingFares <- QRSF.findByRouteStartAndStopCode farePolicy.id row.startStopCode row.endStopCode

        case existingFares of
          Nothing -> do
            logInfo $ "No matching fare found for route " <> row.routeCode <> " with startStopCode " <> row.startStopCode <> " and endStopCode " <> row.endStopCode
            pure [(row.routeCode, row.startStopCode, row.endStopCode)]
          _ -> do
            QRSF.updateFareByStopCodes value farePolicy.id row.startStopCode row.endStopCode DTFRFSQuoteCategoryType.ADULT
            logInfo $ "Updated fare for route " <> row.routeCode <> " from " <> row.startStopCode <> " to " <> row.endStopCode <> " with amount " <> show value
            pure []

  let allUnprocessedFares = concat unprocessedFares

  let totalUpdates = length fareUpdates
  let successfulUpdates = totalUpdates - length allUnprocessedFares

  if successfulUpdates == totalUpdates
    then do
      pure $
        API.Types.RiderPlatform.Management.FRFSTicket.UpsertRouteFareResp
          { unprocessedRouteFares = [],
            success = "All fields updated successfully"
          }
    else do
      let unprocessedRouteFares =
            map
              ( \(routeCode, startStopCode, endStopCode) ->
                  "Route: " <> routeCode <> ", Start Stop: " <> startStopCode <> ", End Stop: " <> endStopCode
              )
              allUnprocessedFares
      pure $
        API.Types.RiderPlatform.Management.FRFSTicket.UpsertRouteFareResp
          { unprocessedRouteFares = unprocessedRouteFares,
            success = "Partial update completed"
          }
  where
    readCsv :: FilePath -> Environment.Flow [FareUpdateCSVRow]
    readCsv csvFile = do
      csvData <- L.runIO $ BS.readFile csvFile
      case (decodeByName $ LBS.fromStrict csvData :: Either String (Header, V.Vector FareUpdateCSVRow)) of
        Left err -> throwError (InvalidRequest $ show err)
        Right (_, v) -> return $ V.toList v

getFRFSTicketFrfsRouteStations :: (ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow [API.Types.RiderPlatform.Management.FRFSTicket.FRFSStationAPI])
getFRFSTicketFrfsRouteStations merchantShortId opCity searchStr limit offset vehicleType = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  integratedBPPConfigs <- SIBC.findAllIntegratedBPPConfig merchantOpCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleType) DIBC.APPLICATION

  SIBC.fetchAllIntegratedBPPConfigResult integratedBPPConfigs $ \integratedBPPConfig -> do
    stations <- case searchStr of
      Just str -> OTPRest.findAllMatchingStations (Just str) (Just limit) (Just offset) vehicleType integratedBPPConfig
      Nothing -> OTPRest.getStationsByVehicleType vehicleType integratedBPPConfig

    frfsStations <- forM stations $ \station -> do
      pure $
        API.Types.RiderPlatform.Management.FRFSTicket.FRFSStationAPI
          { name = station.name,
            code = station.code,
            lat = station.lat,
            lon = station.lon,
            address = station.address,
            regionalName = station.regionalName,
            hindiName = station.hindiName,
            integratedBppConfigId = cast integratedBPPConfig.id
          }

    pure frfsStations

readCsvRows :: FromNamedRecord a => FilePath -> Environment.Flow [a]
readCsvRows csvFile = do
  csvData <- L.runIO $ BS.readFile csvFile
  case decodeByName (LBS.fromStrict csvData) of
    Left err -> throwError (InvalidRequest $ show err)
    Right (_, v) -> pure $ V.toList v

-- Ops paste tier names straight out of GIMS, so go through the permissive FromJSON instance
-- (it accepts "A/C", "Shuttle", ...) rather than Read, which only takes the constructor names.
parseServiceTierType :: Data.Text.Text -> Maybe BecknV2.FRFS.Enums.ServiceTierType
parseServiceTierType tierText = case A.fromJSON (A.String $ Data.Text.strip tierText) of
  A.Success tier -> Just tier
  A.Error _ -> Nothing

data RouteTypeCSVRow = RouteTypeCSVRow
  { routeCode :: Data.Text.Text,
    serviceTier :: Data.Text.Text,
    routeType :: Data.Text.Text
  }

instance FromNamedRecord RouteTypeCSVRow where
  parseNamedRecord r =
    RouteTypeCSVRow
      <$> r .: "Route ID"
      <*> r .: "Service Tier"
      <*> r .: "Route Type"

putFRFSTicketFrfsRouteTypeUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.IntegratedBPPConfig -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.RiderPlatform.Management.FRFSTicket.UpsertRouteTypeReq -> Environment.Flow API.Types.RiderPlatform.Management.FRFSTicket.UpsertRouteTypeResp)
putFRFSTicketFrfsRouteTypeUpsert merchantShortId opCity integratedBPPConfigId vehicleType req = do
  rows <- readCsvRows req.file
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOperatingCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigById (cast integratedBPPConfigId)
  rejections <- catMaybes <$> forM rows (upsertRow merchant merchantOperatingCity integratedBPPConfig)
  pure $
    API.Types.RiderPlatform.Management.FRFSTicket.UpsertRouteTypeResp
      { unprocessedRouteTypes = rejections,
        success = mkUpsertSummary (length rows) (length rejections)
      }
  where
    reject row reason = do
      logError $ "FRFS route type upsert skipped route " <> row.routeCode <> " tier " <> row.serviceTier <> ": " <> reason
      pure $ Just $ "Route: " <> row.routeCode <> ", Service Tier: " <> row.serviceTier <> " - " <> reason

    upsertRow merchant merchantOperatingCity integratedBPPConfig row =
      case parseServiceTierType row.serviceTier of
        Nothing -> reject row "unrecognised service tier"
        Just serviceTier ->
          OTPRest.getRouteByRouteId integratedBPPConfig row.routeCode >>= \case
            Nothing -> reject row "route not found"
            Just _ ->
              CQFRFSVehicleServiceTier.findByServiceTierAndMerchantOperatingCityIdAndIntegratedBPPConfigId serviceTier merchantOperatingCity.id integratedBPPConfig.id >>= \case
                Nothing -> reject row "service tier not configured for this city"
                Just vehicleServiceTier -> do
                  let routeType = FRFSUtils.normalizeRouteType row.routeType
                  if Data.Text.null routeType
                    then QFRFSRouteTypeMapping.deleteByRouteCodeAndVehicleServiceTierIdAndIntegratedBppConfigId integratedBPPConfig.id row.routeCode vehicleServiceTier.id
                    else do
                      now <- getCurrentTime
                      existing <- QFRFSRouteTypeMapping.findByPrimaryKey integratedBPPConfig.id row.routeCode vehicleServiceTier.id
                      case existing of
                        Just mapping -> QFRFSRouteTypeMapping.updateByPrimaryKey mapping {DFRFSRouteTypeMapping.routeType = routeType, DFRFSRouteTypeMapping.updatedAt = now}
                        Nothing ->
                          QFRFSRouteTypeMapping.create
                            DFRFSRouteTypeMapping.FRFSRouteTypeMapping
                              { integratedBppConfigId = integratedBPPConfig.id,
                                routeCode = row.routeCode,
                                vehicleServiceTierId = vehicleServiceTier.id,
                                routeType = routeType,
                                merchantId = merchant.id,
                                merchantOperatingCityId = merchantOperatingCity.id,
                                createdAt = now,
                                updatedAt = now
                              }
                  CQFRFSRouteTypeMapping.clearCache row.routeCode integratedBPPConfig.id
                  pure Nothing

getFRFSTicketFrfsStageFareList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow [API.Types.RiderPlatform.Management.FRFSTicket.FRFSStageFareAPI])
getFRFSTicketFrfsStageFareList merchantShortId opCity vehicleType = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOperatingCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  stageFares <- QFRFSGtfsStageFare.findAllByVehicleTypeAndMerchantOperatingCityId vehicleType merchantOperatingCity.id
  serviceTierByTierId <- buildServiceTierMap stageFares
  pure $
    sortBy (comparing (\fare -> (fare.serviceTier, fare.routeType, fare.stage))) $
      mapMaybe (toStageFareAPI serviceTierByTierId) stageFares
  where
    toStageFareAPI serviceTierByTierId stageFare =
      M.lookup stageFare.vehicleServiceTierId serviceTierByTierId <&> \serviceTier ->
        API.Types.RiderPlatform.Management.FRFSTicket.FRFSStageFareAPI
          { serviceTier = serviceTier,
            routeType = stageFare.routeType,
            stage = stageFare.stage,
            amount = stageFare.amount,
            cessCharge = stageFare.cessCharge,
            currency = stageFare.currency
          }

buildServiceTierMap :: [DFRFSGtfsStageFare.FRFSGtfsStageFare] -> Environment.Flow (M.Map (Id DFRFSVehicleServiceTier.FRFSVehicleServiceTier) BecknV2.FRFS.Enums.ServiceTierType)
buildServiceTierMap stageFares = foldM addServiceTier M.empty (map (.vehicleServiceTierId) stageFares)
  where
    addServiceTier acc vehicleServiceTierId
      | M.member vehicleServiceTierId acc = pure acc
      | otherwise = do
        mbVehicleServiceTier <- QFRFSVehicleServiceTier.findById vehicleServiceTierId
        pure $ maybe acc (\vehicleServiceTier -> M.insert vehicleServiceTierId vehicleServiceTier._type acc) mbVehicleServiceTier

data StageFareCSVRow = StageFareCSVRow
  { serviceTier :: Data.Text.Text,
    routeType :: Data.Text.Text,
    stage :: Data.Text.Text,
    amount :: Data.Text.Text,
    cessCharge :: Data.Text.Text
  }

instance FromNamedRecord StageFareCSVRow where
  parseNamedRecord r =
    StageFareCSVRow
      <$> r .: "Service Tier"
      <*> r .: "Route Type"
      <*> r .: "Stage"
      <*> r .: "Amount (In Rupees)"
      <*> r .: "Cess Charge (In Rupees)"

putFRFSTicketFrfsStageFareUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.IntegratedBPPConfig -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.RiderPlatform.Management.FRFSTicket.UpsertStageFareReq -> Environment.Flow API.Types.RiderPlatform.Management.FRFSTicket.UpsertStageFareResp)
putFRFSTicketFrfsStageFareUpsert merchantShortId opCity integratedBPPConfigId vehicleType req = do
  rows <- readCsvRows req.file
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOperatingCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigById (cast integratedBPPConfigId)
  -- New rows inherit the currency already in use for this city rather than assuming one.
  existingCityFares <- QFRFSGtfsStageFare.findAllByVehicleTypeAndMerchantOperatingCityId vehicleType merchantOperatingCity.id
  let cityCurrency = maybe INR (.currency) (listToMaybe existingCityFares)
  rejections <- catMaybes <$> forM rows (upsertRow merchant merchantOperatingCity integratedBPPConfig cityCurrency)
  pure $
    API.Types.RiderPlatform.Management.FRFSTicket.UpsertStageFareResp
      { unprocessedStageFares = rejections,
        success = mkUpsertSummary (length rows) (length rejections)
      }
  where
    reject row reason = do
      logError $ "FRFS stage fare upsert skipped tier " <> row.serviceTier <> " stage " <> row.stage <> ": " <> reason
      pure $ Just $ "Service Tier: " <> row.serviceTier <> ", Route Type: " <> row.routeType <> ", Stage: " <> row.stage <> " - " <> reason

    upsertRow merchant merchantOperatingCity integratedBPPConfig cityCurrency row =
      case (parseServiceTierType row.serviceTier, readMaybe (Data.Text.unpack $ Data.Text.strip row.stage), highPrecMoneyFromText row.amount) of
        (Nothing, _, _) -> reject row "unrecognised service tier"
        (_, Nothing, _) -> reject row "stage is not an integer"
        (_, _, Nothing) -> reject row "amount is not a valid number"
        (Just serviceTier, Just stage, Just amount) ->
          CQFRFSVehicleServiceTier.findByServiceTierAndMerchantOperatingCityIdAndIntegratedBPPConfigId serviceTier merchantOperatingCity.id integratedBPPConfig.id >>= \case
            Nothing -> reject row "service tier not configured for this city"
            Just vehicleServiceTier -> do
              let mbRouteType = if Data.Text.null (FRFSUtils.normalizeRouteType row.routeType) then Nothing else Just (FRFSUtils.normalizeRouteType row.routeType)
                  mbCessCharge = highPrecMoneyFromText row.cessCharge
              now <- getCurrentTime
              stageFares <- QFRFSGtfsStageFare.findAllByVehicleTypeAndStageAndMerchantOperatingCityId vehicleType stage merchantOperatingCity.id
              let matchesRow stageFare =
                    stageFare.vehicleServiceTierId == vehicleServiceTier.id
                      && (FRFSUtils.normalizeRouteType <$> stageFare.routeType) == mbRouteType
              case find matchesRow stageFares of
                Just stageFare ->
                  QFRFSGtfsStageFare.updateByPrimaryKey
                    stageFare
                      { DFRFSGtfsStageFare.amount = amount,
                        DFRFSGtfsStageFare.cessCharge = mbCessCharge,
                        DFRFSGtfsStageFare.updatedAt = now
                      }
                Nothing -> do
                  stageFareId <- generateGUID
                  QFRFSGtfsStageFare.create
                    DFRFSGtfsStageFare.FRFSGtfsStageFare
                      { id = stageFareId,
                        stage = stage,
                        amount = amount,
                        currency = cityCurrency,
                        vehicleServiceTierId = vehicleServiceTier.id,
                        vehicleType = vehicleType,
                        routeType = mbRouteType,
                        cessCharge = mbCessCharge,
                        discountIds = [],
                        merchantId = merchant.id,
                        merchantOperatingCityId = merchantOperatingCity.id,
                        createdAt = now,
                        updatedAt = now
                      }
              CQFRFSGtfsStageFare.clearCache vehicleType stage merchantOperatingCity.id
              pure Nothing

mkUpsertSummary :: Int -> Int -> Data.Text.Text
mkUpsertSummary total rejected
  | rejected == 0 = "All " <> show total <> " rows updated successfully"
  | otherwise = show (total - rejected) <> " of " <> show total <> " rows updated"
