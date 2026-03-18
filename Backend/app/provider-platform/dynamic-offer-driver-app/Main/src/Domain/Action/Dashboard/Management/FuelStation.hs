module Domain.Action.Dashboard.Management.FuelStation
  ( listFuelStations,
    createFuelStation,
    updateFuelStation,
    deleteFuelStation,
  )
where

import qualified Data.Text as T
import Domain.Types.FuelStation
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.Throwing
import qualified Storage.CachedQueries.FuelStation as CQFuelStation
import qualified Storage.Queries.FuelStationExtra as QFuelStationExtra

-- | Request type for creating a fuel station via dashboard.
data CreateFuelStationReq = CreateFuelStationReq
  { name :: Text,
    address :: Text,
    lat :: Double,
    lon :: Double,
    fuelTypes :: [Text],
    brand :: Maybe Text,
    operatingHoursStart :: Maybe Text,
    operatingHoursEnd :: Maybe Text,
    isOpen24h :: Bool,
    phoneNumber :: Maybe Text,
    city :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- | Request type for updating a fuel station via dashboard.
data UpdateFuelStationReq = UpdateFuelStationReq
  { name :: Maybe Text,
    address :: Maybe Text,
    lat :: Maybe Double,
    lon :: Maybe Double,
    fuelTypes :: Maybe [Text],
    brand :: Maybe Text,
    operatingHoursStart :: Maybe Text,
    operatingHoursEnd :: Maybe Text,
    isOpen24h :: Maybe Bool,
    phoneNumber :: Maybe Text,
    isActive :: Maybe Bool,
    city :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- | Response for create/update operations.
data FuelStationActionResp = FuelStationActionResp
  { id :: Text,
    result :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- | List fuel stations for admin dashboard with optional city filter.
listFuelStations ::
  Id Merchant ->
  Id MerchantOperatingCity ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Flow [FuelStation]
listFuelStations _merchantId merchantOpCityId mbCity mbLimit mbOffset = do
  let limit' = fromMaybe 50 mbLimit
      offset' = fromMaybe 0 mbOffset
  case mbCity of
    Just city -> QFuelStationExtra.findAllByCityPaginated city limit' offset'
    Nothing -> QFuelStationExtra.findAllByMerchantOpCityPaginated merchantOpCityId limit' offset'

-- | Create a new fuel station from admin dashboard.
createFuelStation ::
  Id Merchant ->
  Id MerchantOperatingCity ->
  CreateFuelStationReq ->
  Flow FuelStationActionResp
createFuelStation merchantId merchantOpCityId req = do
  newId <- generateGUID
  now <- getCurrentTime
  let fuelStation =
        FuelStation
          { id = newId,
            name = req.name,
            address = req.address,
            lat = req.lat,
            lon = req.lon,
            fuelTypes = req.fuelTypes,
            brand = req.brand,
            operatingHoursStart = req.operatingHoursStart,
            operatingHoursEnd = req.operatingHoursEnd,
            isOpen24h = req.isOpen24h,
            phoneNumber = req.phoneNumber,
            isActive = True,
            city = req.city,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOpCityId,
            createdAt = now,
            updatedAt = now
          }
  QFuelStationExtra.createFuelStation fuelStation
  CQFuelStation.invalidateCache merchantOpCityId
  pure $ FuelStationActionResp {id = getId newId, result = "Success"}

-- | Update an existing fuel station from admin dashboard.
updateFuelStation ::
  Id Merchant ->
  Id MerchantOperatingCity ->
  Id FuelStation ->
  UpdateFuelStationReq ->
  Flow FuelStationActionResp
updateFuelStation _merchantId merchantOpCityId stationId req = do
  station <- QFuelStationExtra.findByIdAndActive stationId >>= fromMaybeM (InvalidRequest "Fuel station not found")
  now <- getCurrentTime
  let updatedStation =
        station
          { name = fromMaybe station.name req.name,
            address = fromMaybe station.address req.address,
            lat = fromMaybe station.lat req.lat,
            lon = fromMaybe station.lon req.lon,
            fuelTypes = fromMaybe station.fuelTypes req.fuelTypes,
            brand = req.brand <|> station.brand,
            operatingHoursStart = req.operatingHoursStart <|> station.operatingHoursStart,
            operatingHoursEnd = req.operatingHoursEnd <|> station.operatingHoursEnd,
            isOpen24h = fromMaybe station.isOpen24h req.isOpen24h,
            phoneNumber = req.phoneNumber <|> station.phoneNumber,
            isActive = fromMaybe station.isActive req.isActive,
            city = fromMaybe station.city req.city,
            updatedAt = now
          }
  QFuelStationExtra.updateFuelStation updatedStation
  CQFuelStation.invalidateCache merchantOpCityId
  pure $ FuelStationActionResp {id = getId stationId, result = "Success"}

-- | Soft-delete a fuel station (set isActive = False).
deleteFuelStation ::
  Id Merchant ->
  Id MerchantOperatingCity ->
  Id FuelStation ->
  Flow FuelStationActionResp
deleteFuelStation _merchantId merchantOpCityId stationId = do
  _station <- QFuelStationExtra.findByIdAndActive stationId >>= fromMaybeM (InvalidRequest "Fuel station not found")
  QFuelStationExtra.softDeleteFuelStation stationId
  CQFuelStation.invalidateCache merchantOpCityId
  pure $ FuelStationActionResp {id = getId stationId, result = "Deleted"}
