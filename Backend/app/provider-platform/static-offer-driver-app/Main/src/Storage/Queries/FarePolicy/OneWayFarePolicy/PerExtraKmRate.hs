{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.FarePolicy.OneWayFarePolicy.PerExtraKmRate
  ( Storage.Queries.FarePolicy.OneWayFarePolicy.PerExtraKmRate.findAll',
    deleteAll',
  )
where

import Domain.Types.Merchant (Merchant)
import Domain.Types.Vehicle as Vehicle
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.FarePolicy.OneWayFarePolicy.PerExtraKmRate

findAll' ::
  forall m ma.
  ( Transactionable ma m,
    Monad m,
    MonadThrow m,
    Log m
  ) =>
  Id Merchant ->
  Vehicle.Variant ->
  Proxy ma ->
  DTypeBuilder m [PerExtraKmRateT]
findAll' merchantId vehicleVariant _ = do
  Esq.findAll' @m @ma $ do
    perExtraKmRate <- from $ table @PerExtraKmRateT
    where_ $
      perExtraKmRate ^. PerExtraKmRateMerchantId ==. val (toKey merchantId)
        &&. perExtraKmRate ^. PerExtraKmRateVehicleVariant ==. val vehicleVariant
    orderBy [asc $ perExtraKmRate ^. PerExtraKmRateDistanceRangeStart]
    return perExtraKmRate

deleteAll' :: Id Merchant -> Vehicle.Variant -> FullEntitySqlDB m ()
deleteAll' merchantId var =
  Esq.delete' $ do
    perExtraKmRate <- from $ table @PerExtraKmRateT
    where_ $
      perExtraKmRate ^. PerExtraKmRateMerchantId ==. val (toKey merchantId)
        &&. perExtraKmRate ^. PerExtraKmRateVehicleVariant ==. val var
