module Domain.Action.Dashboard.FRFSTicket
  ( getFRFSTicketFrfsRoutes,
    getFRFSTicketFrfsRouteFareList,
    putFRFSTicketFrfsRouteFareUpsert,
    getFRFSTicketFrfsRouteStations,
  )
where

import qualified API.Types.RiderPlatform.Management.FRFSTicket
import qualified BecknV2.FRFS.Enums
import BecknV2.FRFS.Utils
import qualified Dashboard.Common as Common
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Csv
import Data.List (groupBy)
import qualified Data.Text
import qualified Data.Vector as V
import qualified Domain.Types.FRFSQuoteCategoryType as DTFRFSQuoteCategoryType
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant
import qualified Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (find, groupBy, id, length, map, null)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as DTB
import Kernel.Utils.Common (fromMaybeM, throwError)
import Kernel.Utils.Logging (logInfo)
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Storage.Queries.FRFSFarePolicy as QFRFSFarePolicy
import Storage.Queries.FRFSRouteFareProduct as QFRFSRouteFareProduct
import Storage.Queries.StopFare as QRSF

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
            integratedBppConfigId = cast integratedBPPConfig.id
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
