{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverFee where

import Domain.Types.DriverFee
import Domain.Types.Person
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common (Money)
import Kernel.Types.Id
import Kernel.Types.Time
import Storage.Tabular.DriverFee

create :: DriverFee -> SqlDB ()
create = Esq.create

findLatestFeeByDriverId :: Transactionable m => Id Driver -> m (Maybe DriverFee)
findLatestFeeByDriverId driverId = do
  findOne $ do
    driverFee <- from $ table @DriverFeeT
    where_ $
      driverFee ^. DriverFeeDriverId ==. val (toKey $ cast driverId)
    orderBy [desc $ driverFee ^. DriverFeeCreatedAt]
    limit 1
    return driverFee

findOldestFeeByStatus :: Transactionable m => Id Driver -> DriverFeeStatus -> m (Maybe DriverFee)
findOldestFeeByStatus driverId status = do
  findOne $ do
    driverFee <- from $ table @DriverFeeT
    where_ $
      driverFee ^. DriverFeeDriverId ==. val (toKey $ cast driverId)
        &&. driverFee ^. DriverFeeStatus ==. val status
    orderBy [asc $ driverFee ^. DriverFeeCreatedAt]
    limit 1
    return driverFee

findFeesInRangeWithStatus :: Transactionable m => UTCTime -> UTCTime -> DriverFeeStatus -> m [DriverFee]
findFeesInRangeWithStatus startTime endTime status = do
  findAll $ do
    driverFee <- from $ table @DriverFeeT
    where_ $
      driverFee ^. DriverFeeStartTime <=. val startTime
        &&. driverFee ^. DriverFeeEndTime >=. val endTime
        &&. driverFee ^. DriverFeeStatus ==. val status
    return driverFee

updateFee :: Id DriverFee -> Money -> SqlDB ()
updateFee driverFeeId totalAmount = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverFeeTotalAmount =. val totalAmount,
        DriverFeeStatus =. val ONGOING,
        DriverFeeUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverFeeId ==. val (getId driverFeeId)

updateStatus :: DriverFeeStatus -> Id DriverFee -> SqlDB ()
updateStatus status driverFeeId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverFeeStatus =. val status,
        DriverFeeUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverFeeId ==. val (getId driverFeeId)
