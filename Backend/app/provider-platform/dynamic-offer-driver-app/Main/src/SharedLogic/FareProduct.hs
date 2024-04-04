{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.FareProduct where

import Control.Applicative ((<|>))
import Data.Time hiding (getCurrentTime)
import Data.Time.Calendar.WeekDate
import qualified Domain.Action.UI.FareProduct as UFP
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.FareProduct as DFareProduct
import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.ServiceTierType as DVST
import qualified Kernel.Beam.Functions as B
import Kernel.External.Maps (LatLong)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import qualified Lib.Queries.SpecialLocationPriority as QSpecialLocationPriority
import qualified Lib.Types.SpecialLocation as DSpecialLocation
import qualified Lib.Types.SpecialLocation as SL
import qualified Storage.CachedQueries.FareProduct as QFareProduct

data FareProducts = FareProducts
  { fareProducts :: [DFareProduct.FareProduct],
    area :: SL.Area,
    specialLocationTag :: Maybe Text
  }

getPickupSpecialLocation ::
  (KvDbFlow m r, EsqDBReplicaFlow m r) =>
  Id Merchant ->
  DSpecialLocation.SpecialLocation ->
  m (DSpecialLocation.SpecialLocation, Int)
getPickupSpecialLocation merchantId pickupSpecialLocation = do
  pickupSpecialLocationPriority <- B.runInReplica $ QSpecialLocationPriority.findAllByMerchantIdAndCategory merchantId.getId pickupSpecialLocation.category
  return (pickupSpecialLocation, maybe 999 (.pickupPriority) pickupSpecialLocationPriority)

getDropSpecialLocation ::
  (KvDbFlow m r, EsqDBReplicaFlow m r) =>
  Id Merchant ->
  DSpecialLocation.SpecialLocation ->
  m (DSpecialLocation.SpecialLocation, Int)
getDropSpecialLocation merchantId dropSpecialLocation = do
  dropSpecialLocationPriority <- B.runInReplica $ QSpecialLocationPriority.findAllByMerchantIdAndCategory merchantId.getId dropSpecialLocation.category
  return (dropSpecialLocation, maybe 999 (.dropPriority) dropSpecialLocationPriority)

getAllFareProducts :: (KvDbFlow m r, EsqDBReplicaFlow m r) => Id Merchant -> Id DMOC.MerchantOperatingCity -> LatLong -> Maybe LatLong -> DTC.TripCategory -> m FareProducts
getAllFareProducts merchantId merchantOpCityId fromLocationLatLong mToLocationLatLong tripCategory = do
  mbPickupSpecialLocation <- mapM (getPickupSpecialLocation merchantId) =<< QSpecialLocation.findPickupSpecialLocationByLatLong fromLocationLatLong
  mbDropSpecialLocation <- maybe (pure Nothing) (\toLoc -> mapM (getDropSpecialLocation merchantId) =<< QSpecialLocation.findSpecialLocationByLatLong' toLoc) mToLocationLatLong
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
      let area = SL.Pickup pickupSpecialLocation.id
      fareProducts <- getFareProducts area
      return $
        FareProducts
          { fareProducts,
            area = area,
            specialLocationTag = Just specialLocationTag
          }
    getDropFareProductsAndSpecialLocationTag dropSpecialLocation specialLocationTag = do
      let area = SL.Drop dropSpecialLocation.id
      fareProducts <- getFareProducts area
      return $
        FareProducts
          { fareProducts,
            area,
            specialLocationTag = Just specialLocationTag
          }

    getDefaultFareProducts = do
      defFareProducts <- QFareProduct.findAllUnboundedFareProductForVariants merchantOpCityId tripCategory SL.Default
      fareProducts <- mapM getBoundedOrDefaultFareProduct defFareProducts
      return $
        FareProducts
          { fareProducts,
            area = SL.Default,
            specialLocationTag = Nothing
          }

    mkSpecialLocationTag pickupSpecialLocationCategory dropSpecialLocationCategory priority = pickupSpecialLocationCategory <> "_" <> dropSpecialLocationCategory <> "_" <> "Priority" <> priority

    getFareProducts area = do
      fareProducts <- QFareProduct.findAllUnboundedFareProductForVariants merchantOpCityId tripCategory area
      if null fareProducts && area /= SL.Default
        then do
          defFareProducts <- QFareProduct.findAllUnboundedFareProductForVariants merchantOpCityId tripCategory SL.Default
          fareProducts' <- mapM getBoundedOrDefaultFareProduct defFareProducts
          return fareProducts'
        else do
          fareProducts' <- mapM getBoundedOrDefaultFareProduct fareProducts
          return fareProducts'

    getBoundedOrDefaultFareProduct fareProduct = do
      boundedFareProduct <- getBoundedFareProduct fareProduct.merchantOperatingCityId fareProduct.tripCategory fareProduct.vehicleServiceTier fareProduct.area
      return $ fromMaybe fareProduct boundedFareProduct

getBoundedFareProduct :: (KvDbFlow m r, EsqDBReplicaFlow m r) => Id DMOC.MerchantOperatingCity -> DTC.TripCategory -> DVST.ServiceTierType -> SL.Area -> m (Maybe DFareProduct.FareProduct)
getBoundedFareProduct merchantOpCityId tripCategory serviceTier area = do
  fareProducts <- QFareProduct.findAllBoundedByMerchantVariantArea merchantOpCityId tripCategory serviceTier area
  currentIstTime <- getLocalCurrentTime 19800
  let currTimeOfDay = utcTimeToDiffTime currentIstTime
      currentDay = utctDay currentIstTime
      (_, _, currentDayOfWeek) = toWeekDate currentDay
  let (fareProductBoundedByWeekday, fareProductBoundedByDay) =
        foldl
          ( \acc@(fpBoundedByWeekday, fpBoundedByDay) fp ->
              case fp.timeBounds of
                UFP.BoundedByWeekday timeBounds ->
                  if isWithin currTimeOfDay (getPeaksForCurrentDay currentDayOfWeek timeBounds)
                    then (Just fp, fpBoundedByDay)
                    else acc
                UFP.BoundedByDay days ->
                  if maybe False (isWithin currTimeOfDay) (snd <$> find (\(day, _) -> day == currentDay) days)
                    then (fpBoundedByWeekday, Just fp)
                    else acc
                UFP.Unbounded -> acc
          )
          (Nothing, Nothing)
          fareProducts
  return $ fareProductBoundedByDay <|> fareProductBoundedByWeekday
  where
    isWithin _ [] = False
    isWithin currTime [(startTime, endTime)] = currTime > (timeOfDayToTime startTime) && currTime < (timeOfDayToTime endTime)
    isWithin currTime ((startTime, endTime) : xs) = (currTime > (timeOfDayToTime startTime) && currTime < (timeOfDayToTime endTime)) || isWithin currTime xs

    getPeaksForCurrentDay currentDayOfWeek peaks =
      case currentDayOfWeek of
        1 -> peaks.monday
        2 -> peaks.tuesday
        3 -> peaks.wednesday
        4 -> peaks.thursday
        5 -> peaks.friday
        6 -> peaks.saturday
        7 -> peaks.sunday
        _ -> peaks.monday -- This case should never come.
