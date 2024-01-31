{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.FareProduct where

import qualified Domain.Types.Common as DTC
import qualified Domain.Types.FareProduct as DFareProduct
import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Kernel.Beam.Functions as B
import Kernel.External.Maps (LatLong)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import qualified Lib.Queries.SpecialLocationPriority as QSpecialLocationPriority
import qualified Lib.Types.SpecialLocation as DSpecialLocation
import qualified Storage.CachedQueries.FareProduct as QFareProduct

data FareProducts = FareProducts
  { fareProducts :: [DFareProduct.FareProduct],
    area :: DFareProduct.Area,
    specialLocationTag :: Maybe Text
  }

getPickupSpecialLocation ::
  (EsqDBFlow m r, EsqDBReplicaFlow m r) =>
  Id Merchant ->
  DSpecialLocation.SpecialLocation ->
  m (DSpecialLocation.SpecialLocation, Int)
getPickupSpecialLocation merchantId pickupSpecialLocation = do
  pickupSpecialLocationPriority <- B.runInReplica $ QSpecialLocationPriority.findByMerchantIdAndCategory merchantId.getId pickupSpecialLocation.category
  -- pickupSpecialLocationPriority <- QSpecialLocationPriority.findByMerchantIdAndCategory merchantId.getId pickupSpecialLocation.category
  return (pickupSpecialLocation, maybe 999 (.pickupPriority) pickupSpecialLocationPriority)

getDropSpecialLocation ::
  (EsqDBFlow m r, EsqDBReplicaFlow m r) =>
  Id Merchant ->
  DSpecialLocation.SpecialLocation ->
  m (DSpecialLocation.SpecialLocation, Int)
getDropSpecialLocation merchantId dropSpecialLocation = do
  dropSpecialLocationPriority <- B.runInReplica $ QSpecialLocationPriority.findByMerchantIdAndCategory merchantId.getId dropSpecialLocation.category
  -- dropSpecialLocationPriority <- QSpecialLocationPriority.findByMerchantIdAndCategory merchantId.getId dropSpecialLocation.category
  return (dropSpecialLocation, maybe 999 (.dropPriority) dropSpecialLocationPriority)

getAllFareProducts :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Merchant -> Id DMOC.MerchantOperatingCity -> LatLong -> Maybe LatLong -> DTC.TripCategory -> m FareProducts
getAllFareProducts merchantId merchantOpCityId fromLocationLatLong mToLocationLatLong tripCategory = do
  mbPickupSpecialLocation <- mapM (getPickupSpecialLocation merchantId . fst) =<< QSpecialLocation.findSpecialLocationByLatLong fromLocationLatLong
  mbDropSpecialLocation <- maybe (pure Nothing) (\toLoc -> mapM (getDropSpecialLocation merchantId . fst) =<< QSpecialLocation.findSpecialLocationByLatLong toLoc) mToLocationLatLong
  case (mbPickupSpecialLocation, mbDropSpecialLocation) of
    (Just (pickupSpecialLocation, pickupPriority), Just (dropSpecialLocation, dropPriority)) ->
      if pickupPriority > dropPriority
        then getDropFareProductsAndSpecialLocationTag dropSpecialLocation $ mkSpecialLocationTag pickupSpecialLocation.category dropSpecialLocation.category "Drop"
        else getPickupFareProductsAndSpecialLocationTag pickupSpecialLocation $ mkSpecialLocationTag pickupSpecialLocation.category dropSpecialLocation.category "Pickup"
    (Just (pickupSpecialLocation, _), Nothing) -> getPickupFareProductsAndSpecialLocationTag pickupSpecialLocation $ mkSpecialLocationTag pickupSpecialLocation.category "None" "Pickup"
    (Nothing, Just (dropSpecialLocation, _)) -> getDropFareProductsAndSpecialLocationTag dropSpecialLocation $ mkSpecialLocationTag "None" dropSpecialLocation.category "Drop"
    (Nothing, Nothing) -> getDefaultFareProducts
  where
    getPickupFareProductsAndSpecialLocationTag pickupSpecialLocation specialLocationTag = do
      let area = DFareProduct.Pickup pickupSpecialLocation.id
      fareProducts <- getFareProducts area
      return $
        FareProducts
          { fareProducts,
            area = area,
            specialLocationTag = Just specialLocationTag
          }
    getDropFareProductsAndSpecialLocationTag dropSpecialLocation specialLocationTag = do
      let area = DFareProduct.Drop dropSpecialLocation.id
      fareProducts <- getFareProducts area
      return $
        FareProducts
          { fareProducts,
            area,
            specialLocationTag = Just specialLocationTag
          }

    getDefaultFareProducts = do
      fareProducts <- QFareProduct.findAllFareProductForVariants merchantOpCityId tripCategory DFareProduct.Default
      return $
        FareProducts
          { fareProducts,
            area = DFareProduct.Default,
            specialLocationTag = Nothing
          }

    mkSpecialLocationTag pickupSpecialLocationCategory dropSpecialLocationCategory priority = pickupSpecialLocationCategory <> "_" <> dropSpecialLocationCategory <> "_" <> "Priority" <> priority

    getFareProducts area = do
      fareProducts <- QFareProduct.findAllFareProductForVariants merchantOpCityId tripCategory area
      if null fareProducts && area /= DFareProduct.Default
        then QFareProduct.findAllFareProductForVariants merchantOpCityId tripCategory DFareProduct.Default
        else return fareProducts
