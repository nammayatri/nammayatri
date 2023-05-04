{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.FarePolicy
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.FarePolicy
import Domain.Types.Merchant
import Domain.Types.Vehicle.Variant (Variant)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FarePolicy.DriverExtraFeeBounds as QFPDriverExtraFeeBounds
import qualified Storage.Queries.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab as QFPSlabDetSlabs
import Storage.Queries.FullEntityBuilders (buildFullFarePolicy)
import Storage.Tabular.FarePolicy
import Storage.Tabular.FarePolicy.FarePolicyProgressiveDetails (EntityField (FarePolicyProgressiveDetailsBaseDistance, FarePolicyProgressiveDetailsBaseFare, FarePolicyProgressiveDetailsDeadKmFare, FarePolicyProgressiveDetailsNightShiftCharge, FarePolicyProgressiveDetailsPerExtraKmFare, FarePolicyProgressiveDetailsTId), FarePolicyProgressiveDetailsT (..))
import Storage.Tabular.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab ()
import Storage.Tabular.FarePolicy.Instances

findAllByMerchantId ::
  Transactionable m =>
  Id Merchant ->
  m [FarePolicy]
findAllByMerchantId merchantId = buildDType $ do
  res <- Esq.findAll' $ do
    farePolicy <- from $ table @FarePolicyT
    where_ $
      farePolicy ^. FarePolicyMerchantId ==. val (toKey merchantId)
    return farePolicy
  catMaybes <$> mapM buildFullFarePolicy res

findByMerchantIdAndVariant ::
  Transactionable m =>
  Id Merchant ->
  Variant ->
  m (Maybe FarePolicy)
findByMerchantIdAndVariant merchantId variant = buildDType $ do
  res <- Esq.findOne' $ do
    farePolicy <- from $ table @FarePolicyT
    where_ $
      farePolicy ^. FarePolicyMerchantId ==. val (toKey merchantId)
        &&. farePolicy ^. FarePolicyVehicleVariant ==. val variant
    return farePolicy
  join <$> mapM buildFullFarePolicy res

findById :: Transactionable m => Id FarePolicy -> m (Maybe FarePolicy)
findById farePolicyId = buildDType $ do
  res <- Esq.findById' farePolicyId
  join <$> mapM buildFullFarePolicy res

update :: FarePolicy -> SqlDB ()
update farePolicy = do
  now <- getCurrentTime
  withFullEntity farePolicy $ \(FarePolicyT {..}, driverExtraFeeBoundsT, fpDetailsT) -> do
    Esq.update' $ \tbl -> do
      set
        tbl
        [ FarePolicyNightShiftStart =. val nightShiftStart,
          FarePolicyNightShiftEnd =. val nightShiftEnd,
          FarePolicyUpdatedAt =. val now
        ]
      where_ $ tbl ^. FarePolicyTId ==. val (toKey farePolicy.id)

    updateDriverExtraFeeBounds driverExtraFeeBoundsT

    case fpDetailsT of
      ProgressiveDetailsT fpdd -> updateProgressiveDetails fpdd
      SlabsDetailsT fsdd -> updateSlabsDetails fsdd
  where
    updateDriverExtraFeeBounds driverExtraFeeBoundsT = do
      QFPDriverExtraFeeBounds.deleteAll' farePolicy.id
      Esq.createMany' driverExtraFeeBoundsT

    updateProgressiveDetails FarePolicyProgressiveDetailsT {..} = do
      void $
        Esq.update' $ \tbl -> do
          set
            tbl
            [ FarePolicyProgressiveDetailsBaseFare =. val baseFare,
              FarePolicyProgressiveDetailsBaseDistance =. val baseDistance,
              FarePolicyProgressiveDetailsPerExtraKmFare =. val perExtraKmFare,
              FarePolicyProgressiveDetailsDeadKmFare =. val deadKmFare,
              FarePolicyProgressiveDetailsNightShiftCharge =. val nightShiftCharge
            ]
          where_ $ tbl ^. FarePolicyProgressiveDetailsTId ==. val (toKey farePolicy.id)
    updateSlabsDetails dets = do
      QFPSlabDetSlabs.deleteAll' farePolicy.id
      Esq.createMany' dets
