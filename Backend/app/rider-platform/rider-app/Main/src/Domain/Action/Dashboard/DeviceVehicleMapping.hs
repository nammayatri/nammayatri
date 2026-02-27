{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.DeviceVehicleMapping
  ( getDeviceVehicleMappingDeviceVehicleMappingList,
    postDeviceVehicleMappingDeviceVehicleMappingUpsert,
  )
where

import qualified API.Types
import qualified API.Types.RiderPlatform.Management.DeviceVehicleMapping as APIDvm
import qualified BecknV2.OnDemand.Enums
import qualified Dashboard.Common
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Csv (FromNamedRecord (..), Header, decodeByName, (.:))
import Data.OpenApi (ToSchema)
import qualified Data.Vector as V
import qualified Domain.Types.DeviceVehicleMapping
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Prelude hiding (forM_, length, map)
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.IntegratedBPPConfig as CQIBC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DeviceVehicleMapping as QDvm
import Tools.Auth

-- CSV row type
data DeviceVehicleMappingCsvRow = DeviceVehicleMappingCsvRow
  { device_id :: Text,
    fleet_id :: Text
  }

instance FromNamedRecord DeviceVehicleMappingCsvRow where
  parseNamedRecord r =
    DeviceVehicleMappingCsvRow
      <$> r .: "device_id"
      <*> r .: "fleet_id"

getFeedKey :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Environment.Flow Text
getFeedKey merchantOpCityId = do
  integratedBPPConfig <-
    CQIBC.findByDomainAndCityAndVehicleCategory
      "FRFS"
      merchantOpCityId
      BecknV2.OnDemand.Enums.BUS
      Domain.Types.IntegratedBPPConfig.APPLICATION
      >>= fromMaybeM (InternalError "IntegratedBPPConfig not found for BUS")
  pure integratedBPPConfig.feedKey

getDeviceVehicleMappingDeviceVehicleMappingList ::
  ( Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
    Kernel.Types.Beckn.Context.City ->
    Environment.Flow APIDvm.DeviceVehicleMappingListRes
  )
getDeviceVehicleMappingDeviceVehicleMappingList merchantShortId opCity = do
  merchant <-
    CQM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)

  merchantOpCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM
        ( MerchantOperatingCityNotFound $
            "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity
        )

  gtfsId <- getFeedKey merchantOpCity.id
  mappings <- QDvm.findAllByGtfsId gtfsId
  let items = map toItem mappings
  pure $
    APIDvm.DeviceVehicleMappingListRes
      { APIDvm.mappings = items
      }
  where
    toItem dvm =
      APIDvm.DeviceVehicleMappingItem
        { deviceId = dvm.deviceId,
          vehicleNo = dvm.vehicleNo,
          gtfsId = dvm.gtfsId,
          createdAt = dvm.createdAt,
          updatedAt = dvm.updatedAt
        }

postDeviceVehicleMappingDeviceVehicleMappingUpsert ::
  ( Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
    Kernel.Types.Beckn.Context.City ->
    APIDvm.UpsertDeviceVehicleMappingReq ->
    Environment.Flow APIDvm.UpsertDeviceVehicleMappingResp
  )
postDeviceVehicleMappingDeviceVehicleMappingUpsert merchantShortId opCity req = do
  merchant <-
    CQM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)

  merchantOpCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM
        ( MerchantOperatingCityNotFound $
            "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity
        )

  gtfsId <- getFeedKey merchantOpCity.id
  csvRows <- readCsv req.file

  forM_ csvRows $ \row ->
    upsertRow row.device_id row.fleet_id gtfsId

  pure $
    APIDvm.UpsertDeviceVehicleMappingResp
      { success = "All mappings upserted successfully",
        totalInserted = length csvRows
      }
  where
    readCsv :: FilePath -> Environment.Flow [DeviceVehicleMappingCsvRow]
    readCsv csvFile = do
      csvData <- liftIO $ BS.readFile csvFile
      case (decodeByName $ LBS.fromStrict csvData :: Either String (Header, V.Vector DeviceVehicleMappingCsvRow)) of
        Left err -> throwError (InvalidRequest $ show err)
        Right (_, v) -> pure $ V.toList v

    upsertRow :: Text -> Text -> Text -> Environment.Flow ()
    upsertRow deviceId vehicleNo gtfsId = do
      now <- getCurrentTime
      existing <- QDvm.findByDeviceIdAndGtfsId deviceId gtfsId
      case existing of
        Just dvm ->
          QDvm.updateByPrimaryKey
            Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping
              { deviceId = deviceId,
                vehicleNo = vehicleNo,
                gtfsId = gtfsId,
                createdAt = dvm.createdAt,
                updatedAt = now,
                merchantId = Kernel.Prelude.Nothing,
                merchantOperatingCityId = Kernel.Prelude.Nothing
              }
        Nothing -> do
          QDvm.create
            Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping
              { deviceId = deviceId,
                vehicleNo = vehicleNo,
                gtfsId = gtfsId,
                createdAt = now,
                updatedAt = now,
                merchantId = Kernel.Prelude.Nothing,
                merchantOperatingCityId = Kernel.Prelude.Nothing
              }
