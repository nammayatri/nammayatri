module Domain.Action.Dashboard.FRFSTicket
  ( getFRFSTicketFrfsRoutes,
    getFRFSTicketFrfsRouteFareList,
    putFRFSTicketFrfsRouteFareUpsert,
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
import qualified Domain.Types.FRFSVehicleServiceTier as DFRFSVehicleServiceTier
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant
import qualified Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (find, groupBy, id, length, map, null, readMaybe)
import Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as DTB
import Kernel.Utils.Common (fromMaybeM, throwError)
import Kernel.Utils.Logging (logError, logInfo)
import qualified SharedLogic.FRFSUtils as FRFSUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.FRFSGtfsStageFare as CQFRFSGtfsStageFare
import qualified Storage.CachedQueries.FRFSVehicleServiceTier as CQFRFSVehicleServiceTier
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Storage.Queries.FRFSFarePolicy as QFRFSFarePolicy
import qualified Storage.Queries.FRFSGtfsStageFare as QFRFSGtfsStageFare
import Storage.Queries.FRFSRouteFareProduct as QFRFSRouteFareProduct
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
      pure $
        API.Types.RiderPlatform.Management.FRFSTicket.FRFSDashboardRouteAPI
          { code = rte.code,
            shortName = rte.shortName,
            longName = rte.longName,
            startPoint = rte.startPoint,
            endPoint = rte.startPoint,
            integratedBppConfigId = cast integratedBPPConfig.id,
            routeTag = rte.routeTag
          }
    pure frfsRoutes

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
  fareUpdates :: [FareUpdateCSVRow] <- readCsvRows req.file

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

-- Ops paste tier names from GIMS ("A/C", "Shuttle"), which FromJSON accepts but Read cannot parse.
parseServiceTierType :: Data.Text.Text -> Maybe BecknV2.FRFS.Enums.ServiceTierType
parseServiceTierType tierText = case A.fromJSON (A.String $ Data.Text.strip tierText) of
  A.Success tier -> Just tier
  A.Error _ -> Nothing

getFRFSTicketFrfsStageFareList :: (ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow [API.Types.RiderPlatform.Management.FRFSTicket.FRFSStageFareAPI])
getFRFSTicketFrfsStageFareList merchantShortId opCity vehicleType = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOperatingCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  stageFares <- QFRFSGtfsStageFare.findAllByVehicleTypeAndMerchantOperatingCityId vehicleType merchantOperatingCity.id
  serviceTierByTierId <- buildServiceTierMap stageFares
  let apiStageFares = mapMaybe (toStageFareAPI serviceTierByTierId) stageFares
  when (length apiStageFares /= length stageFares) $
    logInfo $ "FRFS stage fare list omitted " <> show (length stageFares - length apiStageFares) <> " row(s) whose service tier no longer exists"
  pure $ sortBy (comparing (\fare -> (fare.serviceTier, fare.routeTag, fare.stage))) apiStageFares
  where
    toStageFareAPI serviceTierByTierId stageFare =
      M.lookup stageFare.vehicleServiceTierId serviceTierByTierId <&> \serviceTier ->
        API.Types.RiderPlatform.Management.FRFSTicket.FRFSStageFareAPI
          { serviceTier = serviceTier,
            routeTag = stageFare.routeTag,
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
    routeTag :: Data.Text.Text,
    stage :: Data.Text.Text,
    amount :: Data.Text.Text,
    cessCharge :: Data.Text.Text
  }

instance FromNamedRecord StageFareCSVRow where
  parseNamedRecord r =
    StageFareCSVRow
      <$> r .: "Service Tier"
      <*> r .: "Route Tag"
      <*> r .: "Stage"
      <*> r .: "Amount (In Rupees)"
      <*> r .: "Cess Charge (In Rupees)"

putFRFSTicketFrfsStageFareUpsert :: (ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Id Common.IntegratedBPPConfig -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.RiderPlatform.Management.FRFSTicket.UpsertStageFareReq -> Environment.Flow API.Types.RiderPlatform.Management.FRFSTicket.UpsertStageFareResp)
putFRFSTicketFrfsStageFareUpsert merchantShortId opCity integratedBPPConfigId vehicleType req = do
  rows :: [StageFareCSVRow] <- readCsvRows req.file
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOperatingCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig (Just $ cast integratedBPPConfigId) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleType) DIBC.APPLICATION
  -- New rows inherit the currency already in use for this city rather than assuming one.
  existingCityFares <- QFRFSGtfsStageFare.findAllByVehicleTypeAndMerchantOperatingCityId vehicleType merchantOperatingCity.id
  let cityCurrency = maybe INR (.currency) (listToMaybe existingCityFares)
  -- Folded, not mapped, so each row can see the keys earlier rows already claimed.
  rejections <- reverse . snd <$> foldM (upsertRow merchant merchantOperatingCity integratedBPPConfig cityCurrency) (M.empty, []) rows
  pure $
    API.Types.RiderPlatform.Management.FRFSTicket.UpsertStageFareResp
      { unprocessedStageFares = rejections,
        success = mkUpsertSummary (length rows) (length rejections)
      }
  where
    upsertRow merchant merchantOperatingCity integratedBPPConfig cityCurrency (seen, rejections) row =
      case (parseServiceTierType row.serviceTier, readMaybe (Data.Text.unpack $ Data.Text.strip row.stage), highPrecMoneyFromText row.amount, parseCessCharge row.cessCharge) of
        (Nothing, _, _, _) -> reject "unrecognised service tier"
        (_, Nothing, _, _) -> reject "stage is not an integer"
        -- The lookup clamps to `max 0 stage`, so a negative row would report success yet never be charged.
        (_, Just stage, _, _) | stage < 0 -> reject "stage cannot be negative"
        (_, _, Nothing, _) -> reject "amount is not a valid number"
        (_, _, _, Nothing) -> reject "cess charge is not a valid number"
        (Just serviceTier, Just stage, Just amount, Just mbCessCharge) ->
          CQFRFSVehicleServiceTier.findByServiceTierAndMerchantOperatingCityIdAndIntegratedBPPConfigId serviceTier merchantOperatingCity.id integratedBPPConfig.id >>= \case
            Nothing -> reject "service tier not configured for this city"
            Just vehicleServiceTier -> do
              let mbRouteTag = FRFSUtils.normalizedRouteTagOf (Just row.routeTag)
                  rowKey = (vehicleServiceTier.id, stage, mbRouteTag)
              -- A city holds a tier per BPP config, so one grid can list a ServiceTierType twice; applying both would silently keep the last.
              if M.member rowKey seen
                then reject "duplicate of an earlier row in this upload"
                else do
                  now <- getCurrentTime
                  stageFares <- QFRFSGtfsStageFare.findAllByVehicleTypeAndStageAndMerchantOperatingCityId vehicleType stage merchantOperatingCity.id
                  let matchesRow stageFare =
                        stageFare.vehicleServiceTierId == vehicleServiceTier.id
                          && FRFSUtils.normalizedRouteTagOf stageFare.routeTag == mbRouteTag
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
                            routeTag = mbRouteTag,
                            cessCharge = mbCessCharge,
                            discountIds = [],
                            merchantId = merchant.id,
                            merchantOperatingCityId = merchantOperatingCity.id,
                            createdAt = now,
                            updatedAt = now
                          }
                  CQFRFSGtfsStageFare.clearCache vehicleType stage merchantOperatingCity.id
                  pure (M.insert rowKey () seen, rejections)
      where
        reject reason = do
          logError $ "FRFS stage fare upsert skipped tier " <> row.serviceTier <> " stage " <> row.stage <> ": " <> reason
          pure (seen, ("Service Tier: " <> row.serviceTier <> ", Route Tag: " <> row.routeTag <> ", Stage: " <> row.stage <> " - " <> reason) : rejections)

-- | Nothing rejects the row; Just Nothing is an empty cell -- without the split, garbage silently clears the stored cess.
parseCessCharge :: Data.Text.Text -> Maybe (Maybe HighPrecMoney)
parseCessCharge raw
  | Data.Text.null (Data.Text.strip raw) = Just Nothing
  | otherwise = Just <$> highPrecMoneyFromText (Data.Text.strip raw)

mkUpsertSummary :: Int -> Int -> Data.Text.Text
mkUpsertSummary total rejected
  | rejected == 0 = "All " <> show total <> " rows updated successfully"
  | otherwise = show (total - rejected) <> " of " <> show total <> " rows updated"
