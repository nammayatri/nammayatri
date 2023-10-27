{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Consumer.AvailabilityTime.Storage.Queries where

import qualified Consumer.AvailabilityTime.Storage.Beam.Tables as BeamDA
import qualified Consumer.AvailabilityTime.Types as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import qualified Sequelize as Se

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.DriverAvailability -> m ()
create = createWithKV

findLatestByDriverIdAndMerchantId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.DriverId -> Domain.MerchantId -> m (Maybe Domain.DriverAvailability)
findLatestByDriverIdAndMerchantId driverId merchantId =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDA.driverId $ Se.Eq driverId,
          Se.Is BeamDA.merchantId $ Se.Eq merchantId
        ]
    ]
    (Se.Desc BeamDA.lastAvailableTime)
    (Just 1)
    Nothing
    <&> listToMaybe

findAvailableTimeInBucketByDriverIdAndMerchantId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.DriverId -> Domain.MerchantId -> UTCTime -> UTCTime -> m (Maybe Domain.DriverAvailability)
findAvailableTimeInBucketByDriverIdAndMerchantId driverId merchantId bucketStartTime bucketEndTime =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDA.driverId $ Se.Eq driverId,
          Se.Is BeamDA.merchantId $ Se.Eq merchantId,
          Se.Is BeamDA.bucketStartTime $ Se.Eq bucketStartTime,
          Se.Is BeamDA.bucketEndTime $ Se.Eq bucketEndTime
        ]
    ]
    (Se.Desc BeamDA.lastAvailableTime)
    (Just 1)
    Nothing
    <&> listToMaybe

createOrUpdateDriverAvailability :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.DriverAvailability -> m ()
createOrUpdateDriverAvailability d@Domain.DriverAvailability {..} = do
  mbOldBucketAvailableTime <- findAvailableTimeInBucketByDriverIdAndMerchantId driverId merchantId bucketStartTime bucketEndTime
  case mbOldBucketAvailableTime of
    Nothing -> Consumer.AvailabilityTime.Storage.Queries.create d
    Just lastVal ->
      updateWithKV
        [ Se.Set BeamDA.totalAvailableTime (lastVal.totalAvailableTime + totalAvailableTime),
          Se.Set BeamDA.updatedAt updatedAt,
          Se.Set BeamDA.lastAvailableTime (max lastVal.lastAvailableTime lastAvailableTime)
        ]
        [ Se.And
            [ Se.Is BeamDA.driverId $ Se.Eq driverId,
              Se.Is BeamDA.bucketStartTime $ Se.Eq bucketStartTime,
              Se.Is BeamDA.bucketEndTime $ Se.Eq bucketEndTime
            ]
        ]

instance FromTType' BeamDA.DriverAvailability Domain.DriverAvailability where
  fromTType' BeamDA.DriverAvailabilityT {..} = do
    pure $
      Just
        Domain.DriverAvailability
          { id = Id id,
            ..
          }

instance ToTType' BeamDA.DriverAvailability Domain.DriverAvailability where
  toTType' Domain.DriverAvailability {..} =
    BeamDA.DriverAvailabilityT
      { id = getId id,
        ..
      }
