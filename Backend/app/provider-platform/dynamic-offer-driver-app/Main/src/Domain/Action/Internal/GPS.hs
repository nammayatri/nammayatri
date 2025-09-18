module Domain.Action.Internal.GPS where

import Data.OpenApi
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import qualified Domain.Types.TripTransaction as DTT
import Environment
import EulerHS.Prelude
import qualified Kernel.External.GPS.Types as GPS
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.MerchantServiceConfig as QServiceConfig
import qualified Storage.Queries.TripTransactionExtra as QTT

data DriverByPlateResp = DriverByPlateResp
  { driverId :: Id Person,
    merchantId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    routeCode :: Maybe Text,
    busNumber :: Maybe Text,
    groupId :: Maybe Text,
    vehicleServiceTierType :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

getDriversByPlates :: Text -> [Text] -> Flow [DriverByPlateResp]
getDriversByPlates apiKey plateNumbers = do
  activeTripss <- QTT.findActiveTripsByVehicleNumbers plateNumbers
  catMaybes <$> mapM (validateAndConvertTrip apiKey) activeTripss

validateAndConvertTrip :: Text -> DTT.TripTransaction -> Flow (Maybe DriverByPlateResp)
validateAndConvertTrip apiKey trip = do
  isValidApiKey <- checkGpsApiKey apiKey trip.merchantOperatingCityId
  if isValidApiKey
    then do
      response <- tripToDriverByPlateResp trip
      return $ Just response
    else do
      logWarning $ "GPS API key validation failed for vehicle: " <> trip.vehicleNumber
      return Nothing

-- Helper function that returns Bool instead of throwing
checkGpsApiKey :: Text -> Id MerchantOperatingCity -> Flow Bool
checkGpsApiKey providedApiKey merchantOpCityId = do
  mbServiceConfig <- QServiceConfig.findByServiceAndCity (DEMSC.GpsService GPS.Hunteyed) merchantOpCityId

  case mbServiceConfig of
    Nothing -> return False
    Just serviceConfig -> do
      case serviceConfig.serviceConfig of
        DEMSC.GpsServiceConfig gpsConfig ->
          return $ providedApiKey == gpsConfig.apiKey
        _ -> return False

tripToDriverByPlateResp :: DTT.TripTransaction -> Flow DriverByPlateResp
tripToDriverByPlateResp trip = do
  return $
    DriverByPlateResp
      { driverId = trip.driverId,
        merchantId = trip.merchantId,
        merchantOperatingCityId = trip.merchantOperatingCityId,
        routeCode = Just trip.routeCode,
        busNumber = Just trip.vehicleNumber, -- vehicleNumber is the plate number
        groupId = Nothing, -- Can be extended later if needed
        vehicleServiceTierType = show trip.vehicleServiceTierType -- BUS_NON_AC, BUS_AC, etc.
      }
