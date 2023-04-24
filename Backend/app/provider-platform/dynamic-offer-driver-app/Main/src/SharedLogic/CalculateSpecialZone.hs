{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.CalculateSpecialZone where

import qualified Domain.Types.FareProduct as DFareProduct
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SpecialZoneLink as DSpecialZoneLink
import Domain.Types.Vehicle (Variant)
import Kernel.External.Maps (LatLong)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import qualified Lib.Types.SpecialLocation as DSpecialLocation
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.FareProduct as CQFareProduct
import qualified Storage.CachedQueries.SpecialZoneCategoryPriority as CQSpecialZoneCategoryPriority
import qualified Storage.Queries.FareProduct as QFareProduct
import qualified Storage.Queries.SpecialZoneLink as QSpecialZoneLink
import Tools.Error

getPickupSpecialZone :: (MonadFlow m, MonadReader r m, HasEsqEnv m r, CacheFlow m r) => Id DM.Merchant -> DSpecialLocation.SpecialLocation -> m (DSpecialLocation.SpecialLocation, Int)
getPickupSpecialZone merchantId pickupSpecialZone = do
  pickupSpecialZonePriority <- CQSpecialZoneCategoryPriority.findByMerchantIdAndCategory merchantId (show pickupSpecialZone.category) >>= fromMaybeM (InternalError "PickupSpecialZoneCategoryPriority Not Found")
  return (pickupSpecialZone, pickupSpecialZonePriority.pickupPriority)

getDropSpecialZone :: (MonadFlow m, MonadReader r m, HasEsqEnv m r, CacheFlow m r) => Id DM.Merchant -> DSpecialLocation.SpecialLocation -> m (DSpecialLocation.SpecialLocation, Int)
getDropSpecialZone merchantId dropSpecialZone = do
  dropSpecialZonePriority <- CQSpecialZoneCategoryPriority.findByMerchantIdAndCategory merchantId (show dropSpecialZone.category) >>= fromMaybeM (InternalError "DropSpecialZoneCategoryPriority Not Found")
  return (dropSpecialZone, dropSpecialZonePriority.dropPriority)

calculateFareProductsAndSpecialZone :: (MonadFlow m, MonadReader r m, HasEsqEnv m r, CacheFlow m r) => Id DM.Merchant -> LatLong -> LatLong -> m ([DFareProduct.FareProduct], Maybe Text)
calculateFareProductsAndSpecialZone merchantId fromLocationLatLong toLocationLatLong = do
  mbPickupSpecialZone <- mapM (getPickupSpecialZone merchantId . fst) =<< QSpecialLocation.findSpecialLocationByLatLong fromLocationLatLong
  mbDropSpecialZone <- mapM (getDropSpecialZone merchantId . fst) =<< QSpecialLocation.findSpecialLocationByLatLong toLocationLatLong
  case (mbPickupSpecialZone, mbDropSpecialZone) of
    (Just (pickupSpecialZone, pickupPriority), Just (dropSpecialZone, dropPriority)) ->
      if pickupPriority > dropPriority
        then getSpecialZoneFareProducts merchantId dropSpecialZone DSpecialZoneLink.DROP
        else getSpecialZoneFareProducts merchantId pickupSpecialZone DSpecialZoneLink.PICKUP
    (Just (pickupSpecialZone, _), Nothing) -> getSpecialZoneFareProducts merchantId pickupSpecialZone DSpecialZoneLink.PICKUP
    (Nothing, Just (dropSpecialZone, _)) -> getSpecialZoneFareProducts merchantId dropSpecialZone DSpecialZoneLink.DROP
    (Nothing, Nothing) -> getFareProducts merchantId

getSpecialZoneFareProducts :: (MonadFlow m, MonadReader r m, HasEsqEnv m r) => Id DM.Merchant -> DSpecialLocation.SpecialLocation -> DSpecialZoneLink.PickupOrDropType -> m ([DFareProduct.FareProduct], Maybe Text)
getSpecialZoneFareProducts merchantId specialZone pickupOrDrop = do
  let specialZoneTag = show specialZone.category <> " - " <> show pickupOrDrop
  specialZones <- QSpecialZoneLink.findByMerchantIdAndSpecialZoneIdForPickupOrDrop merchantId specialZone.id pickupOrDrop
  fareProducts <- QFareProduct.findBySpecialZoneFareProductIds (specialZones <&> (.fareProductId))
  return (fareProducts, Just specialZoneTag)

getFareProducts :: (MonadFlow m, MonadReader r m, HasEsqEnv m r) => Id DM.Merchant -> m ([DFareProduct.FareProduct], Maybe Text)
getFareProducts merchantId = do
  specialZones <- QSpecialZoneLink.findByMerchantId merchantId
  fareProducts <- QFareProduct.findByMerchantIdAndNotInSpecialZone merchantId (specialZones <&> (.fareProductId))
  return (fareProducts, Nothing)

calculateFareProductAndSpecialZone :: (MonadFlow m, MonadReader r m, HasEsqEnv m r, CacheFlow m r) => Id DM.Merchant -> LatLong -> LatLong -> Variant -> m (DFareProduct.FareProduct, Maybe Text)
calculateFareProductAndSpecialZone merchantId fromLocationLatLong toLocationLatLong vehicleVariant = do
  mbPickupSpecialZone <- mapM (getPickupSpecialZone merchantId . fst) =<< QSpecialLocation.findSpecialLocationByLatLong fromLocationLatLong
  mbDropSpecialZone <- mapM (getDropSpecialZone merchantId . fst) =<< QSpecialLocation.findSpecialLocationByLatLong toLocationLatLong
  case (mbPickupSpecialZone, mbDropSpecialZone) of
    (Just (pickupSpecialZone, pickupPriority), Just (dropSpecialZone, dropPriority)) ->
      if pickupPriority > dropPriority
        then getSpecialZoneFareProduct merchantId dropSpecialZone DSpecialZoneLink.DROP vehicleVariant
        else getSpecialZoneFareProduct merchantId pickupSpecialZone DSpecialZoneLink.PICKUP vehicleVariant
    (Just (pickupSpecialZone, _), Nothing) -> getSpecialZoneFareProduct merchantId pickupSpecialZone DSpecialZoneLink.PICKUP vehicleVariant
    (Nothing, Just (dropSpecialZone, _)) -> getSpecialZoneFareProduct merchantId dropSpecialZone DSpecialZoneLink.DROP vehicleVariant
    (Nothing, Nothing) -> getFareProduct merchantId vehicleVariant

getSpecialZoneFareProduct :: (MonadFlow m, MonadReader r m, HasEsqEnv m r, CacheFlow m r) => Id DM.Merchant -> DSpecialLocation.SpecialLocation -> DSpecialZoneLink.PickupOrDropType -> Variant -> m (DFareProduct.FareProduct, Maybe Text)
getSpecialZoneFareProduct merchantId specialZone pickupOrDrop vehicleVariant = do
  let specialZoneTag = show specialZone.category <> " - " <> show pickupOrDrop
  specialZoneLink <- QSpecialZoneLink.findByMerchantIdAndSpecialZoneIdForPickupOrDropAndVehicleVariant merchantId specialZone.id pickupOrDrop vehicleVariant >>= fromMaybeM (InternalError "SpecialZoneLink Not Found")
  fareProduct <- CQFareProduct.findById specialZoneLink.fareProductId >>= fromMaybeM (InternalError "FareProduct Not Found")
  return (fareProduct, Just specialZoneTag)

getFareProduct :: (MonadFlow m, MonadReader r m, HasEsqEnv m r) => Id DM.Merchant -> Variant -> m (DFareProduct.FareProduct, Maybe Text)
getFareProduct merchantId vehicleVariant = do
  specialZoneLinks <- QSpecialZoneLink.findByMerchantId merchantId
  fareProduct <- QFareProduct.findByMerchantIdAndVehicleVariantAndNotInSpecialZone merchantId vehicleVariant (specialZoneLinks <&> (.fareProductId)) >>= fromMaybeM (InternalError "FareProduct Not Found")
  return (fareProduct, Nothing)
