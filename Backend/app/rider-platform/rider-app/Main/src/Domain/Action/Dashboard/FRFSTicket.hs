module Domain.Action.Dashboard.FRFSTicket
  ( getFRFSTicketFrfsRoutes,
    postFRFSTicketFrfsRouteAdd,
    postFRFSTicketFrfsRouteDelete,
    getFRFSTicketFrfsRouteFareList,
    putFRFSTicketFrfsRouteFareUpsert,
    getFRFSTicketFrfsRouteStations,
    postFRFSTicketFrfsStationAdd,
    postFRFSTicketFrfsStationDelete,
  )
where

import qualified API.Types.RiderPlatform.Management.FRFSTicket
import qualified BecknV2.FRFS.Enums
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Csv
import Data.List (groupBy)
import Data.Maybe (listToMaybe)
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Data.Vector as V
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant
import Domain.Types.Route
import Domain.Types.Station
import qualified Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (groupBy, id)
import qualified Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Types.TimeBound
import qualified Kernel.Types.TimeBound as DTB
import Kernel.Utils.Common (fromMaybeM, throwError)
import Kernel.Utils.Logging (logInfo)
import Storage.CachedQueries.IntegratedBPPConfig as QIBC
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.Queries.FRFSFarePolicy as QFRFSFarePolicy
import Storage.Queries.FRFSRouteFareProduct as QFRFSRouteFareProduct
import Storage.Queries.Route as QRoute
import Storage.Queries.RouteExtra as RE
import Storage.Queries.RouteStopFare as QRSF
import Storage.Queries.RouteStopMapping as QRSM
import Storage.Queries.Station as QStation
import Tools.Error

getFRFSTicketFrfsRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow [API.Types.RiderPlatform.Management.FRFSTicket.FRFSDashboardRouteAPI])
getFRFSTicketFrfsRoutes merchantShortId opCity searchStr limit offset vehicleType = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)

  integratedBPPConfig <-
    QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOpCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleType) DIBC.APPLICATION
      >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOpCity.id.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory vehicleType ||+ "Platform Type:" +|| DIBC.APPLICATION ||+ "")
  routes <- case searchStr of
    Just str -> RE.findAllMatchingRoutes (Just str) (Just $ fromIntegral limit) (Just $ fromIntegral offset) merchantOpCity.id vehicleType
    Nothing -> QRoute.findAllByVehicleType (Just limit) (Just offset) vehicleType integratedBPPConfig.id

  frfsRoutes <- forM routes $ \rte -> do
    pure $
      API.Types.RiderPlatform.Management.FRFSTicket.FRFSDashboardRouteAPI
        { code = rte.code,
          shortName = rte.shortName,
          longName = rte.longName,
          startPoint = rte.startPoint,
          endPoint = rte.startPoint
        }
  pure frfsRoutes

postFRFSTicketFrfsRouteAdd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.RiderPlatform.Management.FRFSTicket.FRFSRouteReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFRFSTicketFrfsRouteAdd merchantShortId opCity code vehicleType req = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)

  merchantOperatingCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)

  integratedBPPConfig <-
    QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleType) DIBC.APPLICATION
      >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOperatingCity.id.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory vehicleType ||+ "Platform Type:" +|| DIBC.APPLICATION ||+ "")
  existingRoute <- QRoute.findByRouteCode code integratedBPPConfig.id
  case existingRoute of
    Just _ -> throwError $ InvalidRequest "Route code already exists"
    Nothing -> do
      newId <- generateGUID
      now <- getCurrentTime

      let newRoute =
            Domain.Types.Route.Route
              { id = newId,
                code = code,
                color = req.color,
                shortName = req.shortName,
                longName = req.longName,
                startPoint = req.startPoint,
                endPoint = req.endPoint,
                vehicleType = vehicleType,
                timeBounds = req.timeBounds,
                merchantId = merchant.id,
                merchantOperatingCityId = merchantOperatingCity.id,
                polyline = req.polyline,
                integratedBppConfigId = integratedBPPConfig.id,
                createdAt = now,
                updatedAt = now
              }
      QRoute.create newRoute
      pure Success

postFRFSTicketFrfsRouteDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFRFSTicketFrfsRouteDelete _merchantShortId _opCity code _vehicleType = do
  merchant <- QM.findByShortId _merchantShortId >>= fromMaybeM (MerchantDoesNotExist _merchantShortId.getShortId)

  merchantOperatingCity <-
    CQMOC.findByMerchantIdAndCity merchant.id _opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show _opCity)

  integratedBPPConfig <-
    QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory _vehicleType) DIBC.APPLICATION
      >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOperatingCity.id.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory _vehicleType ||+ "Platform Type:" +|| DIBC.APPLICATION ||+ "")
  _ <- QRoute.findByRouteCode code integratedBPPConfig.id >>= fromMaybeM (InvalidRequest "This route code can't be deleted")
  routeMappings <- QRSM.findByRouteCode code integratedBPPConfig.id
  unless (null routeMappings) $ throwError InvalidAction
  QRoute.deleteByRouteCode code integratedBPPConfig.id
  pure Success

getFRFSTicketFrfsRouteFareList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow API.Types.RiderPlatform.Management.FRFSTicket.FRFSRouteFareAPI)
getFRFSTicketFrfsRouteFareList merchantShortId opCity routeCode vehicleType = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)

  merchantOperatingCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)

  integratedBPPConfig <-
    QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleType) DIBC.APPLICATION
      >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOperatingCity.id.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory vehicleType ||+ "Platform Type:" +|| DIBC.APPLICATION ||+ "")
  fetchedRoute <- QRoute.findByRouteCode routeCode integratedBPPConfig.id >>= fromMaybeM (InvalidRequest "Invalid route code")

  -- TODO :: To be fixed properly to handle multi-dimensional Fare Product
  fareProducts <- QFRFSRouteFareProduct.findByRouteCode routeCode integratedBPPConfig.id
  fareProduct <- find (\fareProduct -> fareProduct.timeBounds == DTB.Unbounded && fareProduct.vehicleType == vehicleType) fareProducts & fromMaybeM (InternalError "FRFS Fare Product Not Found")
  farePolicy <- QFRFSFarePolicy.findById fareProduct.farePolicyId >>= fromMaybeM (InternalError $ "FRFS Fare Policy Not Found : " <> fareProduct.farePolicyId.getId)
  routeFares <- QRSF.findByRouteCode farePolicy.id routeCode

  let groupedFares = groupBy (\a b -> a.startStopCode == b.startStopCode) routeFares
  let sortedGroupedFares = sortBy (comparing (negate . length)) groupedFares
  stopFares <- forM sortedGroupedFares $ \fares -> do
    let maybeFirstFare = listToMaybe fares
    case maybeFirstFare of
      Nothing -> throwError (InvalidRequest "No fares found for the start stop")
      Just firstFare -> do
        startStop <- QStation.findByStationCode firstFare.startStopCode integratedBPPConfig.id >>= fromMaybeM (InvalidRequest $ "Invalid from station id: " <> firstFare.startStopCode <> " or integratedBPPConfigID: " <> integratedBPPConfig.id.getId)

        endStops <- forM fares $ \fare -> do
          endStop <- QStation.findByStationCode fare.endStopCode integratedBPPConfig.id >>= fromMaybeM (InvalidRequest $ "Invalid to station id: " <> fare.endStopCode <> " or integratedBPPConfigID: " <> integratedBPPConfig.id.getId)
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

putFRFSTicketFrfsRouteFareUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.RiderPlatform.Management.FRFSTicket.UpsertRouteFareReq -> Environment.Flow API.Types.RiderPlatform.Management.FRFSTicket.UpsertRouteFareResp)
putFRFSTicketFrfsRouteFareUpsert merchantShortId opCity _routeCode vehicleType req = do
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

        integratedBPPConfig <-
          QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleType) DIBC.APPLICATION
            >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOperatingCity.id.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory vehicleType ||+ "Platform Type:" +|| DIBC.APPLICATION ||+ "")
        -- TODO :: To be fixed properly to handle multi-dimensional Fare Product
        fareProducts <- QFRFSRouteFareProduct.findByRouteCode row.routeCode integratedBPPConfig.id
        fareProduct <- find (\fareProduct -> fareProduct.timeBounds == DTB.Unbounded && fareProduct.vehicleType == vehicleType) fareProducts & fromMaybeM (InternalError "FRFS Fare Product Not Found")
        farePolicy <- QFRFSFarePolicy.findById fareProduct.farePolicyId >>= fromMaybeM (InternalError $ "FRFS Fare Policy Not Found : " <> fareProduct.farePolicyId.getId)
        existingFares <- QRSF.findByRouteStartAndStopCode farePolicy.id row.routeCode row.startStopCode row.endStopCode

        case existingFares of
          Nothing -> do
            logInfo $ "No matching fare found for route " <> row.routeCode <> " with startStopCode " <> row.startStopCode <> " and endStopCode " <> row.endStopCode
            pure [(row.routeCode, row.startStopCode, row.endStopCode)]
          _ -> do
            QRSF.updateFareByRouteCodeAndStopCodes value farePolicy.id row.routeCode row.startStopCode row.endStopCode
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

getFRFSTicketFrfsRouteStations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow [API.Types.RiderPlatform.Management.FRFSTicket.FRFSStationAPI])
getFRFSTicketFrfsRouteStations merchantShortId opCity searchStr limit offset vehicleType = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  integratedBPPConfig <-
    QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOpCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleType) DIBC.APPLICATION
      >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOpCity.id.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory vehicleType ||+ "Platform Type:" +|| DIBC.APPLICATION ||+ "")
  stations <- case searchStr of
    Just str -> findAllMatchingStations (Just str) (Just $ fromIntegral limit) (Just $ fromIntegral offset) merchantOpCity.id vehicleType
    Nothing -> QStation.findAllByVehicleType (Just limit) (Just offset) vehicleType integratedBPPConfig.id

  frfsStations <- forM stations $ \station -> do
    pure $
      API.Types.RiderPlatform.Management.FRFSTicket.FRFSStationAPI
        { name = station.name,
          code = station.code,
          lat = station.lat,
          lon = station.lon,
          address = station.address
        }

  pure frfsStations

postFRFSTicketFrfsStationAdd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.RiderPlatform.Management.FRFSTicket.FRFSStationReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFRFSTicketFrfsStationAdd merchantShortId opCity code vehicleType req = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  integratedBPPConfig <-
    QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOpCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleType) DIBC.APPLICATION
      >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOpCity.id.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory vehicleType ||+ "Platform Type:" +|| DIBC.APPLICATION ||+ "")
  stationExists <- QStation.findByStationCode code integratedBPPConfig.id
  newId <- generateGUID
  now <- getCurrentTime
  case stationExists of
    Just _ -> throwError $ InvalidRequest "Station code already exists"
    Nothing -> do
      let newStation =
            Domain.Types.Station.Station
              { id = newId,
                vehicleType = vehicleType,
                name = req.name,
                possibleTypes = req.possibleTypes,
                code = code,
                lat = Just req.lat,
                lon = Just req.lon,
                address = req.address,
                merchantId = merchant.id,
                timeBounds = Kernel.Types.TimeBound.Unbounded,
                merchantOperatingCityId = merchantOpCity.id,
                integratedBppConfigId = integratedBPPConfig.id,
                createdAt = now,
                updatedAt = now
              }
      QStation.create newStation
      pure Success

postFRFSTicketFrfsStationDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFRFSTicketFrfsStationDelete merchantShortId opCity code _vehicleType = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  integratedBPPConfig <-
    QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOpCity.id (frfsVehicleCategoryToBecknVehicleCategory _vehicleType) DIBC.APPLICATION
      >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOpCity.id.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory _vehicleType ||+ "Platform Type:" +|| DIBC.APPLICATION ||+ "")
  _ <- QStation.findByStationCode code integratedBPPConfig.id >>= fromMaybeM (InvalidRequest "This station code can't be deleted")
  stopMappings <- QRSM.findByStopCode code integratedBPPConfig.id
  unless (null stopMappings) $ throwError InvalidAction
  QStation.deleteByStationCode code integratedBPPConfig.id
  pure Success
