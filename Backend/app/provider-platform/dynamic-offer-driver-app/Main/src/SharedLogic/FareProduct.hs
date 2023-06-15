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

data FareProducts = FareProducts
  { fareProducts :: [DFareProduct.FareProduct],
    area :: DFareProduct.Area,
    specialLocationTag :: Maybe Text
  }

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

getAllFareProducts :: (CacheFlow m r, EsqDBFlow m r, MonadReader r m, EsqDBReplicaFlow m r) => Id Merchant -> LatLong -> LatLong -> m FareProducts
getAllFareProducts merchantId fromLocationLatLong toLocationLatLong = do
  mbPickupSpecialLocation <- mapM (getPickupSpecialLocation merchantId . fst) =<< QSpecialLocation.findSpecialLocationByLatLong fromLocationLatLong
  mbDropSpecialLocation <- mapM (getDropSpecialLocation merchantId . fst) =<< QSpecialLocation.findSpecialLocationByLatLong toLocationLatLong
  case (mbPickupSpecialLocation, mbDropSpecialLocation) of
    (Just (pickupSpecialLocation, pickupPriority), Just (dropSpecialLocation, dropPriority)) ->
      if pickupPriority > dropPriority
        then getDropFareProductsAndSpecialLocationTag dropSpecialLocation
        else getPickupFareProductsAndSpecialLocationTag pickupSpecialLocation
    (Just (pickupSpecialLocation, _), Nothing) -> getPickupFareProductsAndSpecialLocationTag pickupSpecialLocation
    (Nothing, Just (dropSpecialLocation, _)) -> getDropFareProductsAndSpecialLocationTag dropSpecialLocation
    (Nothing, Nothing) -> getDefaultFareProducts
  where
    getPickupFareProductsAndSpecialLocationTag pickupSpecialLocation = do
      let area = DFareProduct.Pickup pickupSpecialLocation.id
      fareProducts <- getFareProducts area
      return $
        FareProducts
          { fareProducts,
            area = area,
            specialLocationTag = Just $ pickupTag pickupSpecialLocation
          }
    getDropFareProductsAndSpecialLocationTag dropSpecialLocation = do
      let area = DFareProduct.Drop dropSpecialLocation.id
      fareProducts <- getFareProducts area
      return $
        FareProducts
          { fareProducts,
            area,
            specialLocationTag = Just $ dropTag dropSpecialLocation
          }

    getDefaultFareProducts = do
      fareProducts <- QFareProduct.findAllFareProductForVariants merchantId DFareProduct.Default
      return $
        FareProducts
          { fareProducts,
            area = DFareProduct.Default,
            specialLocationTag = Nothing
          }

    pickupTag specialLocation = show specialLocation.category <> " - Pickup"
    dropTag specialLocation = show specialLocation.category <> " - Drop"

    getFareProducts area = do
      fareProducts <- QFareProduct.findAllFareProductForVariants merchantId area
      if null fareProducts && area /= DFareProduct.Default
        then QFareProduct.findAllFareProductForVariants merchantId DFareProduct.Default
        else return fareProducts
