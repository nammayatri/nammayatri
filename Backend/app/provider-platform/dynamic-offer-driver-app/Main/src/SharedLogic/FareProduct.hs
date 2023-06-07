{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.FareProduct where

import qualified Domain.Types.FareProduct as DFareProduct
import Domain.Types.Merchant
import Domain.Types.Vehicle.Variant (Variant (..))
import Kernel.External.Maps (LatLong)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import qualified Lib.Queries.SpecialLocationPriority as QSpecialLocationPriority
import qualified Lib.Types.SpecialLocation as DSpecialLocation
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.FareProduct as QFareProduct
import Tools.Error

getPickupSpecialLocation ::
  (MonadFlow m, MonadReader r m, HasEsqEnv m r, EsqDBReplicaFlow m r) =>
  Id Merchant ->
  DSpecialLocation.SpecialLocation ->
  m (DSpecialLocation.SpecialLocation, Int)
getPickupSpecialLocation merchantId pickupSpecialLocation = do
  pickupSpecialLocationPriority <- Esq.runInReplica $ QSpecialLocationPriority.findByMerchantIdAndCategory merchantId.getId pickupSpecialLocation.category
  return (pickupSpecialLocation, maybe 999 (.pickupPriority) pickupSpecialLocationPriority)

getDropSpecialLocation ::
  (MonadFlow m, MonadReader r m, HasEsqEnv m r, EsqDBReplicaFlow m r) =>
  Id Merchant ->
  DSpecialLocation.SpecialLocation ->
  m (DSpecialLocation.SpecialLocation, Int)
getDropSpecialLocation merchantId dropSpecialLocation = do
  dropSpecialLocationPriority <- Esq.runInReplica $ QSpecialLocationPriority.findByMerchantIdAndCategory merchantId.getId dropSpecialLocation.category
  return (dropSpecialLocation, maybe 999 (.dropPriority) dropSpecialLocationPriority)

getFareProductForVariants :: (CacheFlow m r, EsqDBFlow m r, MonadReader r m, EsqDBReplicaFlow m r) => Id Merchant -> LatLong -> LatLong -> m [DFareProduct.FareProduct]
getFareProductForVariants merchantId fromLocationLatLong toLocationLatLong = do
  mbPickupSpecialLocation <- mapM (getPickupSpecialLocation merchantId . fst) =<< QSpecialLocation.findSpecialLocationByLatLong fromLocationLatLong
  mbDropSpecialLocation <- mapM (getDropSpecialLocation merchantId . fst) =<< QSpecialLocation.findSpecialLocationByLatLong toLocationLatLong
  case (mbPickupSpecialLocation, mbDropSpecialLocation) of
    (Just (pickupSpecialLocation, pickupPriority), Just (dropSpecialLocation, dropPriority)) ->
      if pickupPriority > dropPriority
        then getFareProducts $ DFareProduct.Drop dropSpecialLocation.id
        else getFareProducts $ DFareProduct.Pickup pickupSpecialLocation.id
    (Just (pickupSpecialLocation, _), Nothing) -> getFareProducts $ DFareProduct.Pickup pickupSpecialLocation.id
    (Nothing, Just (dropSpecialLocation, _)) -> getFareProducts $ DFareProduct.Drop dropSpecialLocation.id
    (Nothing, Nothing) -> QFareProduct.findAllFareProductForVariants merchantId DFareProduct.Default
  where
    getFareProducts area = do
      fareProducts <- QFareProduct.findAllFareProductForVariants merchantId area
      if null fareProducts && area /= DFareProduct.Default
        then QFareProduct.findAllFareProductForVariants merchantId DFareProduct.Default
        else return fareProducts

getFareProductForVariant :: (CacheFlow m r, EsqDBFlow m r, MonadReader r m, EsqDBReplicaFlow m r) => Id Merchant -> LatLong -> LatLong -> Variant -> m DFareProduct.FareProduct
getFareProductForVariant merchantId fromLocationLatLong toLocationLatLong vehicleVariant = do
  mbPickupSpecialLocation <- mapM (getPickupSpecialLocation merchantId . fst) =<< QSpecialLocation.findSpecialLocationByLatLong fromLocationLatLong
  mbDropSpecialLocation <- mapM (getDropSpecialLocation merchantId . fst) =<< QSpecialLocation.findSpecialLocationByLatLong toLocationLatLong
  case (mbPickupSpecialLocation, mbDropSpecialLocation) of
    (Just (pickupSpecialLocation, pickupPriority), Just (dropSpecialLocation, dropPriority)) ->
      if pickupPriority > dropPriority
        then getFareProduct $ DFareProduct.Drop dropSpecialLocation.id
        else getFareProduct $ DFareProduct.Pickup pickupSpecialLocation.id
    (Just (pickupSpecialLocation, _), Nothing) -> getFareProduct $ DFareProduct.Pickup pickupSpecialLocation.id
    (Nothing, Just (dropSpecialLocation, _)) -> getFareProduct $ DFareProduct.Drop dropSpecialLocation.id
    (Nothing, Nothing) -> QFareProduct.findOneFareProductForVariant merchantId DFareProduct.Default vehicleVariant >>= fromMaybeM NoFareProduct
  where
    getFareProduct area = do
      mbFareProduct <- QFareProduct.findOneFareProductForVariant merchantId area vehicleVariant
      case mbFareProduct of
        Just fareProduct -> return fareProduct
        Nothing ->
          if area /= DFareProduct.Default
            then QFareProduct.findOneFareProductForVariant merchantId DFareProduct.Default vehicleVariant >>= fromMaybeM NoFareProduct
            else fromMaybeM NoFareProduct mbFareProduct

getSpecialLocationTag :: (CacheFlow m r, EsqDBFlow m r, MonadReader r m, EsqDBReplicaFlow m r) => Id Merchant -> LatLong -> LatLong -> m (Maybe Text)
getSpecialLocationTag merchantId fromLocationLatLong toLocationLatLong = do
  mbPickupSpecialLocation <- mapM (getPickupSpecialLocation merchantId . fst) =<< QSpecialLocation.findSpecialLocationByLatLong fromLocationLatLong
  mbDropSpecialLocation <- mapM (getDropSpecialLocation merchantId . fst) =<< QSpecialLocation.findSpecialLocationByLatLong toLocationLatLong
  case (mbPickupSpecialLocation, mbDropSpecialLocation) of
    (Just (pickupSpecialLocation, pickupPriority), Just (dropSpecialLocation, dropPriority)) ->
      if pickupPriority > dropPriority
        then return $ Just (dropTag dropSpecialLocation)
        else return $ Just (pickupTag pickupSpecialLocation)
    (Just (pickupSpecialLocation, _), Nothing) -> return $ Just (pickupTag pickupSpecialLocation)
    (Nothing, Just (dropSpecialLocation, _)) -> return $ Just (dropTag dropSpecialLocation)
    (Nothing, Nothing) -> return Nothing
  where
    pickupTag specialLocation = show specialLocation.category <> " - Pickup"
    dropTag specialLocation = show specialLocation.category <> " - Drop"
