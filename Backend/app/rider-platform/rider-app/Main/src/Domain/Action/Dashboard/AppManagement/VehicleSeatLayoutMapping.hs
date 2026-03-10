module Domain.Action.Dashboard.AppManagement.VehicleSeatLayoutMapping
  ( listVehicleSeatLayoutMapping,
    upsertVehicleSeatLayoutMapping,
    deleteVehicleSeatLayoutMapping,
  )
where

import qualified API.Types.Dashboard.AppManagement.VehicleSeatLayoutMapping as API
import qualified Domain.Types.Merchant
import qualified Domain.Types.VehicleSeatLayoutMapping
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.VehicleSeatLayoutMappingExtra as CQVehicleSeatLayoutMapping
import qualified Storage.Queries.VehicleSeatLayoutMapping as Queries

listVehicleSeatLayoutMapping ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  Text ->
  Environment.Flow [API.VehicleSeatLayoutMappingItem]
listVehicleSeatLayoutMapping _merchantShortId _opCity limit offset gtfsId = do
  mappings <- Queries.findAllByGtfsId limit offset gtfsId
  return $ map toItem mappings
  where
    toItem m =
      API.VehicleSeatLayoutMappingItem
        { id = m.id,
          vehicleNo = m.vehicleNo,
          gtfsId = m.gtfsId,
          seatLayoutId = m.seatLayoutId
        }

upsertVehicleSeatLayoutMapping ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  API.VehicleSeatLayoutMappingUpsertReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
upsertVehicleSeatLayoutMapping merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  mbExisting <- Queries.findByVehicleNoAndGtfsId req.vehicleNo req.gtfsId
  case mbExisting of
    Just _ -> do
      Queries.updateSeatLayoutIdByVehicleNoAndGtfsId req.seatLayoutId req.vehicleNo req.gtfsId
      CQVehicleSeatLayoutMapping.invalidateCache req.vehicleNo req.gtfsId
    Nothing -> do
      now <- getCurrentTime
      newId <- generateGUID
      let mapping =
            Domain.Types.VehicleSeatLayoutMapping.VehicleSeatLayoutMapping
              { id = newId,
                vehicleNo = req.vehicleNo,
                gtfsId = req.gtfsId,
                seatLayoutId = req.seatLayoutId,
                merchantId = merchant.id,
                merchantOperatingCityId = merchantOperatingCity.id,
                createdAt = now,
                updatedAt = now
              }
      Queries.create mapping
  return Kernel.Types.APISuccess.Success

deleteVehicleSeatLayoutMapping ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Text ->
  Text ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
deleteVehicleSeatLayoutMapping _merchantShortId _opCity vehicleNo gtfsId = do
  Queries.deleteByVehicleNoAndGtfsId vehicleNo gtfsId
  CQVehicleSeatLayoutMapping.invalidateCache vehicleNo gtfsId
  return Kernel.Types.APISuccess.Success
