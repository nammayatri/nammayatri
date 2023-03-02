{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.FarePolicy.OneWayFarePolicy
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.FarePolicy.OneWayFarePolicy
import Domain.Types.Merchant
import Domain.Types.Vehicle as Vehicle
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FarePolicy.OneWayFarePolicy.PerExtraKmRate as QExtraKmRate
import Storage.Queries.FullEntityBuilders (buildFullOneWayFarePolicy)
import Storage.Tabular.FarePolicy.OneWayFarePolicy

findByMerchantIdAndVariant ::
  forall m ma.
  Transactionable ma m =>
  Id Merchant ->
  Vehicle.Variant ->
  Proxy ma ->
  m (Maybe OneWayFarePolicy)
findByMerchantIdAndVariant merchantId vehicleVariant_ proxy =
  Esq.buildDType $ do
    mbFarePolicy <- Esq.findOne' @m @ma $ do
      farePolicy <- from $ table @OneWayFarePolicyT
      where_ $
        farePolicy ^. OneWayFarePolicyMerchantId ==. val (toKey merchantId)
          &&. farePolicy ^. OneWayFarePolicyVehicleVariant ==. val vehicleVariant_
      return farePolicy
    mapM (`buildFullOneWayFarePolicy` proxy) mbFarePolicy

findAllByMerchantId :: forall m ma. Transactionable ma m => Id Merchant -> Proxy ma -> m [OneWayFarePolicy]
findAllByMerchantId merchantId proxy =
  Esq.buildDType $ do
    farePolicy <- Esq.findAll' @m @ma $ do
      farePolicy <- from $ table @OneWayFarePolicyT
      where_ $ farePolicy ^. OneWayFarePolicyMerchantId ==. val (toKey merchantId)
      orderBy [asc $ farePolicy ^. OneWayFarePolicyVehicleVariant]
      return farePolicy
    mapM (`buildFullOneWayFarePolicy` proxy) farePolicy

findById :: forall m ma. Transactionable ma m => Id OneWayFarePolicy -> Proxy ma -> m (Maybe OneWayFarePolicy)
findById fpId proxy =
  Esq.buildDType $ do
    mbfarePolicy <- Esq.findById' @_ @m @ma fpId
    mapM (`buildFullOneWayFarePolicy` proxy) mbfarePolicy

update :: OneWayFarePolicy -> SqlDB m ()
update farePolicy = do
  now <- getCurrentTime
  withFullEntity farePolicy $ \(farePolicyT, perExtraKmRateList, _) -> do
    upsert'
      farePolicyT
      [ OneWayFarePolicyBaseFare =. val (fromIntegral <$> farePolicy.baseFare),
        OneWayFarePolicyNightShiftStart =. val (farePolicy.nightShiftStart),
        OneWayFarePolicyNightShiftEnd =. val (farePolicy.nightShiftEnd),
        OneWayFarePolicyNightShiftRate =. val farePolicy.nightShiftRate,
        OneWayFarePolicyUpdatedAt =. val now
      ]

    QExtraKmRate.deleteAll' farePolicy.merchantId farePolicy.vehicleVariant
    Esq.createMany' perExtraKmRateList
