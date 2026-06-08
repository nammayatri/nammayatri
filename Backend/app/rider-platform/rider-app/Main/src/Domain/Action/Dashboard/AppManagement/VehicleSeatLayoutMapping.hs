module Domain.Action.Dashboard.AppManagement.VehicleSeatLayoutMapping
  ( listVehicleSeatLayoutMapping,
    upsertVehicleSeatLayoutMapping,
    deleteVehicleSeatLayoutMapping,
  )
where

import qualified API.Types.Dashboard.AppManagement.VehicleSeatLayoutMapping as API
import Data.List (nub)
import qualified Data.Map.Strict as Map
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
          seatLayoutId = m.seatLayoutId,
          seatSelectionType = m.seatSelectionType
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

  let uniqueGtfsIds = nub $ map (.gtfsId) req.entries
  existingMappings <- Queries.findAllByGtfsIds uniqueGtfsIds

  let initialMap = Map.fromList [((m.gtfsId, m.vehicleNo), m) | m <- existingMappings]

  void $
    foldM
      ( \mappingMap entry -> do
          now <- getCurrentTime
          let key = (entry.gtfsId, entry.vehicleNo)
          case Map.lookup key mappingMap of
            Just existingMapping -> do
              let effectiveSeatSelectionType = entry.seatSelectionType <|> existingMapping.seatSelectionType
              Queries.updateSeatLayoutIdByVehicleNoAndGtfsId entry.seatLayoutId effectiveSeatSelectionType entry.vehicleNo entry.gtfsId
              CQVehicleSeatLayoutMapping.invalidateCache entry.vehicleNo entry.gtfsId
              pure mappingMap
            Nothing -> do
              newId <- generateGUID
              let mapping =
                    Domain.Types.VehicleSeatLayoutMapping.VehicleSeatLayoutMapping
                      { id = newId,
                        vehicleNo = entry.vehicleNo,
                        gtfsId = entry.gtfsId,
                        seatLayoutId = entry.seatLayoutId,
                        seatSelectionType = entry.seatSelectionType,
                        merchantId = merchant.id,
                        merchantOperatingCityId = merchantOperatingCity.id,
                        createdAt = now,
                        updatedAt = now
                      }
              Queries.create mapping
              pure $ Map.insert key mapping mappingMap
      )
      initialMap
      req.entries
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
