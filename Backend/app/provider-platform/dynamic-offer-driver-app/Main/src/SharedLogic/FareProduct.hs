{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.FareProduct where

import Control.Lens ((^?), _head)
import qualified Domain.Types as DTC
import qualified Domain.Types as DVST
import qualified Domain.Types.FareProduct as DFareProduct
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Kernel.Beam.Functions as B
import Kernel.External.Maps (LatLong)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as DTB
import Kernel.Utils.Common
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import qualified Lib.Queries.SpecialLocationPriority as QSpecialLocationPriority
import qualified Lib.Types.SpecialLocation as DSpecialLocation
import qualified Lib.Types.SpecialLocation as SL
import qualified Storage.CachedQueries.FareProduct as QFareProduct

data FareProducts = FareProducts
  { fareProducts :: [DFareProduct.FareProduct],
    area :: SL.Area,
    specialLocationName :: Maybe Text,
    specialLocationTag :: Maybe Text
  }

getPickupSpecialLocation ::
  (EsqDBFlow m r, EsqDBReplicaFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  DSpecialLocation.SpecialLocation ->
  m (DSpecialLocation.SpecialLocation, Int)
getPickupSpecialLocation merchantOpCityId pickupSpecialLocation = do
  pickupSpecialLocationPriority <- Esq.runInReplica $ QSpecialLocationPriority.findByMerchantOpCityIdAndCategory merchantOpCityId.getId pickupSpecialLocation.category
  return (pickupSpecialLocation, maybe 999 (.pickupPriority) pickupSpecialLocationPriority)

getDropSpecialLocation ::
  (EsqDBFlow m r, EsqDBReplicaFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  DSpecialLocation.SpecialLocation ->
  m (DSpecialLocation.SpecialLocation, Int)
getDropSpecialLocation merchantOpCityId dropSpecialLocation = do
  dropSpecialLocationPriority <- B.runInReplica $ QSpecialLocationPriority.findByMerchantOpCityIdAndCategory merchantOpCityId.getId dropSpecialLocation.category
  return (dropSpecialLocation, maybe 999 (.dropPriority) dropSpecialLocationPriority)

getSearchSources :: Bool -> [DFareProduct.SearchSource]
getSearchSources isDashboard = [DFareProduct.ALL] <> (if isDashboard then [DFareProduct.DASHBOARD] else [DFareProduct.MOBILE_APP])

getAllFareProducts :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Merchant -> Id DMOC.MerchantOperatingCity -> [DFareProduct.SearchSource] -> LatLong -> Maybe LatLong -> Maybe (Id DSpecialLocation.SpecialLocation) -> Maybe (Id DSpecialLocation.SpecialLocation) -> DTC.TripCategory -> m FareProducts
getAllFareProducts _merchantId merchantOpCityId searchSources fromLocationLatLong mToLocationLatLong mbFromSpecialLocationId mbToSpecialLocationId tripCategory = do
  -- If SpecialLocation IDs provided directly (fixed route search), use them
  case (mbFromSpecialLocationId, mbToSpecialLocationId) of
    (Just fromId, Just toId) -> do
      mbFromLoc <- QSpecialLocation.findById fromId
      mbToLoc <- QSpecialLocation.findById toId
      case (mbFromLoc, mbToLoc) of
        (Just fromLoc, Just toLoc) -> do
          let specialLocationTag = mkSpecialLocationTag fromLoc.category toLoc.category "FixedRoute"
          mbFareProducts <- getPickupDropFareProducts fromLoc toLoc specialLocationTag
          maybe getDefaultFareProducts return mbFareProducts
        _ -> getDefaultFareProducts
    _ -> do
      -- Existing flow: lookup SpecialLocation by lat/long
      mbPickupSpecialLocation <- mapM (getPickupSpecialLocation merchantOpCityId) =<< QSpecialLocation.findPickupSpecialLocationByLatLong fromLocationLatLong
      mbDropSpecialLocation <- maybe (pure Nothing) (\toLoc -> mapM (getDropSpecialLocation merchantOpCityId) =<< Esq.runInReplica (QSpecialLocation.findSpecialLocationByLatLong' toLoc)) mToLocationLatLong
      case (mbPickupSpecialLocation, mbDropSpecialLocation) of
        (Just (pickupSpecialLocation, pickupPriority), Just (dropSpecialLocation, dropPriority)) -> do
          mbPickupDropFareProducts <- getPickupDropFareProducts pickupSpecialLocation dropSpecialLocation $ mkSpecialLocationTag pickupSpecialLocation.category dropSpecialLocation.category "None"
          case mbPickupDropFareProducts of
            Just pickupDropFareProducts -> return pickupDropFareProducts
            Nothing -> do
              if pickupPriority > dropPriority
                then getDropFareProductsAndSpecialLocationTag dropSpecialLocation $ mkSpecialLocationTag pickupSpecialLocation.category dropSpecialLocation.category "Drop"
                else getPickupFareProductsAndSpecialLocationTag pickupSpecialLocation $ mkSpecialLocationTag pickupSpecialLocation.category dropSpecialLocation.category "Pickup"
        (Just (pickupSpecialLocation, _), Nothing) -> getPickupFareProductsAndSpecialLocationTag pickupSpecialLocation $ mkSpecialLocationTag pickupSpecialLocation.category "None" "Pickup"
        (Nothing, Just (dropSpecialLocation, _)) -> getDropFareProductsAndSpecialLocationTag dropSpecialLocation $ mkSpecialLocationTag "None" dropSpecialLocation.category "Drop"
        (Nothing, Nothing) -> getDefaultFareProducts
  where
    getPickupDropFareProducts pickupSpecialLocation dropSpecialLocation specialLocationTag = do
      let area = SL.PickupDrop pickupSpecialLocation.id dropSpecialLocation.id
      areaProducts <- QFareProduct.findAllUnboundedFareProductForVariants merchantOpCityId searchSources tripCategory area
      otherFareProducts <- QFareProduct.findAllUnboundedFareProductForArea merchantOpCityId searchSources area
      if null areaProducts && null otherFareProducts
        then return Nothing
        else do
          fareProducts <- mapM getBoundedOrDefaultFareProduct areaProducts
          return $
            Just $
              FareProducts
                { fareProducts,
                  area,
                  specialLocationName = Just pickupSpecialLocation.locationName,
                  specialLocationTag = Just specialLocationTag
                }

    getPickupFareProductsAndSpecialLocationTag pickupSpecialLocation specialLocationTag = do
      let area = SL.Pickup pickupSpecialLocation.id
          specialLocationName = pickupSpecialLocation.locationName
      fareProducts <- getFareProducts area
      return $
        FareProducts
          { fareProducts,
            area = area,
            specialLocationName = Just specialLocationName,
            specialLocationTag = Just specialLocationTag
          }
    getDropFareProductsAndSpecialLocationTag dropSpecialLocation specialLocationTag = do
      let area = SL.Drop dropSpecialLocation.id
          specialLocationName = dropSpecialLocation.locationName
      fareProducts <- getFareProducts area
      return $
        FareProducts
          { fareProducts,
            area,
            specialLocationName = Just specialLocationName,
            specialLocationTag = Just specialLocationTag
          }

    getDefaultFareProducts = do
      defFareProducts <- QFareProduct.findAllUnboundedFareProductForVariants merchantOpCityId searchSources tripCategory SL.Default
      fareProducts <- mapM getBoundedOrDefaultFareProduct defFareProducts
      return $
        FareProducts
          { fareProducts,
            area = SL.Default,
            specialLocationName = Nothing,
            specialLocationTag = Nothing
          }

    mkSpecialLocationTag pickupSpecialLocationCategory dropSpecialLocationCategory priority = pickupSpecialLocationCategory <> "_" <> dropSpecialLocationCategory <> "_" <> "Priority" <> priority

    getFareProducts area = do
      fareProducts <- QFareProduct.findAllUnboundedFareProductForVariants merchantOpCityId searchSources tripCategory area
      otherFareProducts <- QFareProduct.findAllUnboundedFareProductForArea merchantOpCityId searchSources area
      if null fareProducts && area /= SL.Default && null otherFareProducts
        then do
          defFareProducts <- QFareProduct.findAllUnboundedFareProductForVariants merchantOpCityId searchSources tripCategory SL.Default
          mapM getBoundedOrDefaultFareProduct defFareProducts
        else do
          mapM getBoundedOrDefaultFareProduct fareProducts

    getBoundedOrDefaultFareProduct fareProduct = do
      boundedFareProduct <- getBoundedFareProduct fareProduct.merchantOperatingCityId searchSources fareProduct.tripCategory fareProduct.vehicleServiceTier fareProduct.area
      return $ fromMaybe fareProduct boundedFareProduct

getBoundedFareProduct :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DMOC.MerchantOperatingCity -> [DFareProduct.SearchSource] -> DTC.TripCategory -> DVST.ServiceTierType -> SL.Area -> m (Maybe DFareProduct.FareProduct)
getBoundedFareProduct merchantOpCityId searchSources tripCategory serviceTier area = do
  fareProducts <- QFareProduct.findAllBoundedByMerchantVariantArea merchantOpCityId searchSources tripCategory serviceTier area
  currentIstTime <- getLocalCurrentTime 19800
  return $ DTB.findBoundedDomain fareProducts currentIstTime ^? _head
