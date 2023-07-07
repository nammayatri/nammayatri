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
import Kernel.Types.Common (HighPrecMoney, Money)
import Kernel.Types.Id
import Storage.Tabular.DriverFee

create :: DriverFee -> SqlDB ()
create = Esq.create

findById :: Transactionable m => Id DriverFee -> m (Maybe DriverFee)
findById = Esq.findById

findByShortId :: Transactionable m => ShortId DriverFee -> m (Maybe DriverFee)
findByShortId shortId = do
  findOne $ do
    driverFee <- from $ table @DriverFeeT
    where_ $ driverFee ^. DriverFeeShortId ==. val (getShortId shortId)
    return driverFee

findPendingFeesByDriverFeeId :: Transactionable m => Id DriverFee -> m (Maybe DriverFee)
findPendingFeesByDriverFeeId driverFeeId = do
  findOne $ do
    driverFee <- from $ table @DriverFeeT
    where_ $
      driverFee ^. DriverFeeId ==. val (getId driverFeeId)
        &&. driverFee ^. DriverFeeStatus `in_` valList [PAYMENT_PENDING, PAYMENT_OVERDUE]
    return driverFee

findPendingFeesByDriverId :: Transactionable m => Id Driver -> m (Maybe DriverFee)
findPendingFeesByDriverId driverId = do
  findOne $ do
    driverFee <- from $ table @DriverFeeT
    where_ $
      driverFee ^. DriverFeeDriverId ==. val (toKey (cast driverId))
        &&. driverFee ^. DriverFeeStatus `in_` valList [PAYMENT_PENDING, PAYMENT_OVERDUE]
    return driverFee

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
      driverFee ^. DriverFeeStartTime >=. val startTime
        &&. driverFee ^. DriverFeeEndTime <=. val endTime
        &&. driverFee ^. DriverFeeStatus ==. val status
    return driverFee

findWindowsWithStatus :: Transactionable m => Id Person -> UTCTime -> UTCTime -> Maybe DriverFeeStatus -> Int -> Int -> m [DriverFee]
findWindowsWithStatus driverId startTime endTime mbStatus limitVal offsetVal = do
  findAll $ do
    driverFee <- from $ table @DriverFeeT
    where_ $
      driverFee ^. DriverFeeDriverId ==. val (toKey driverId)
        &&. driverFee ^. DriverFeeStartTime >=. val startTime
        &&. driverFee ^. DriverFeeEndTime <=. val endTime
        &&. whenJust_ mbStatus (\status -> driverFee ^. DriverFeeStatus ==. val status)
    orderBy [desc $ driverFee ^. DriverFeeCreatedAt]
    limit $ fromIntegral limitVal
    offset $ fromIntegral offsetVal
    return driverFee

findOngoingAfterEndTime :: Transactionable m => Id Person -> UTCTime -> m (Maybe DriverFee)
findOngoingAfterEndTime driverId now = do
  findOne $ do
    -- assuming one such entry only
    driverFee <- from $ table @DriverFeeT
    where_ $
      driverFee ^. DriverFeeDriverId ==. val (toKey driverId)
        &&. driverFee ^. DriverFeeStatus ==. val ONGOING
        &&. driverFee ^. DriverFeeEndTime <=. val now
    return driverFee

findUnpaidAfterPayBy :: Transactionable m => Id Person -> UTCTime -> m (Maybe DriverFee)
findUnpaidAfterPayBy driverId now = do
  findOne $ do
    -- assuming only one such entry only
    driverFee <- from $ table @DriverFeeT
    where_ $
      driverFee ^. DriverFeeDriverId ==. val (toKey driverId)
        &&. driverFee ^. DriverFeeStatus `in_` valList [PAYMENT_PENDING, PAYMENT_OVERDUE]
        &&. driverFee ^. DriverFeePayBy <=. val now
    return driverFee

updateFee :: Id DriverFee -> Maybe Money -> Money -> Money -> HighPrecMoney -> HighPrecMoney -> UTCTime -> SqlDB ()
updateFee driverFeeId mbFare govtCharges platformFee cgst sgst now = do
  let fare = fromMaybe 0 mbFare
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverFeeGovtCharges +=. val govtCharges,
        DriverFeePlatformFee +=. val platformFee,
        DriverFeeCgst +=. val cgst,
        DriverFeeSgst +=. val sgst,
        DriverFeeStatus =. val ONGOING,
        DriverFeeTotalEarnings +=. val fare,
        DriverFeeNumRides +=. val 1, -- in the api, num_rides needed without cost contribution?
        DriverFeeUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverFeeId ==. val (getId driverFeeId)

updateStatus :: DriverFeeStatus -> Id DriverFee -> UTCTime -> SqlDB ()
updateStatus status driverFeeId now = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverFeeStatus =. val status,
        DriverFeeUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverFeeId ==. val (getId driverFeeId)
