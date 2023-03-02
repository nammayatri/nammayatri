{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Consumer.AvailabilityTime.Storage.Queries where

import Consumer.AvailabilityTime.Storage.Tables
import qualified Consumer.AvailabilityTime.Types as Domain
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq hiding (putMany)

create :: Domain.DriverAvailability -> Esq.SqlDB Flow ()
create = Esq.create

findLatestByDriverIdAndMerchantId :: forall m. (Esq.Transactionable Flow m) => Domain.DriverId -> Domain.MerchantId -> m (Maybe Domain.DriverAvailability)
findLatestByDriverIdAndMerchantId driverId merchantId = buildDType $
  fmap (fmap $ extractSolidType @Domain.DriverAvailability) $
    Esq.findOne' @m @Flow $ do
      dDriverAvailability <-
        from $ table @DriverAvailabilityT
      where_ $
        dDriverAvailability ^. DriverAvailabilityDriverId ==. val driverId
          &&. dDriverAvailability ^. DriverAvailabilityMerchantId ==. val merchantId
      orderBy [desc $ dDriverAvailability ^. DriverAvailabilityLastAvailableTime]
      limit 1
      pure dDriverAvailability

findAvailableTimeInBucketByDriverIdAndMerchantId :: forall m. (Esq.Transactionable Flow m) => Domain.DriverId -> Domain.MerchantId -> UTCTime -> UTCTime -> m (Maybe Domain.DriverAvailability)
findAvailableTimeInBucketByDriverIdAndMerchantId driverId merchantId bucketStartTime bucketEndTime = buildDType $
  fmap (fmap $ extractSolidType @Domain.DriverAvailability) $
    Esq.findOne' @m @Flow $ do
      dDriverAvailability <-
        from $ table @DriverAvailabilityT
      where_ $
        dDriverAvailability ^. DriverAvailabilityDriverId ==. val driverId
          &&. dDriverAvailability ^. DriverAvailabilityMerchantId ==. val merchantId
          &&. dDriverAvailability ^. DriverAvailabilityBucketStartTime ==. val bucketStartTime
          &&. dDriverAvailability ^. DriverAvailabilityBucketEndTime ==. val bucketEndTime
      orderBy [desc $ dDriverAvailability ^. DriverAvailabilityUpdatedAt]
      limit 1
      pure dDriverAvailability

createOrUpdateDriverAvailability :: Domain.DriverAvailability -> SqlDB Flow ()
createOrUpdateDriverAvailability d@Domain.DriverAvailability {..} = do
  mbOldBucketAvailableTime <- findAvailableTimeInBucketByDriverIdAndMerchantId driverId merchantId bucketStartTime bucketEndTime
  case mbOldBucketAvailableTime of
    Nothing -> Consumer.AvailabilityTime.Storage.Queries.create d
    Just lastVal ->
      Esq.update $ \tbl -> do
        set
          tbl
          [ DriverAvailabilityTotalAvailableTime =. val (lastVal.totalAvailableTime + totalAvailableTime),
            DriverAvailabilityUpdatedAt =. val updatedAt,
            DriverAvailabilityLastAvailableTime =. val (ifAGBTA lastVal.lastAvailableTime lastAvailableTime)
          ]
        where_ $
          tbl ^. DriverAvailabilityDriverId ==. val driverId
            &&. tbl ^. DriverAvailabilityBucketStartTime ==. val bucketStartTime
            &&. tbl ^. DriverAvailabilityBucketEndTime ==. val bucketEndTime
  where
    ifAGBTA a b = if a > b then a else b
