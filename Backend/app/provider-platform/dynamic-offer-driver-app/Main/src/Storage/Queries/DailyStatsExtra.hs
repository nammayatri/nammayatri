module Storage.Queries.DailyStatsExtra where

import Data.Time (Day)
import Domain.Types.DailyStats
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.DailyStats as Beam
import Storage.Queries.OrphanInstances.DailyStats ()

-- Extra code goes here --

findAllInRangeByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id SP.Person -> Day -> Day -> m [DailyStats]
findAllInRangeByDriverId (Id driverId) from to = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq driverId,
          Se.Is Beam.merchantLocalDate $ Se.GreaterThanOrEq from,
          Se.Is Beam.merchantLocalDate $ Se.LessThan to
        ]
    ]

findAllByDatesInAndPayoutStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Day] -> PayoutStatus -> m [DailyStats]
findAllByDatesInAndPayoutStatus dates payoutStatus = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantLocalDate $ Se.In dates,
          Se.Is Beam.payoutStatus $ Se.Eq (Just payoutStatus)
        ]
    ]

findAllByPayoutStatusAndReferralEarningsAndDriver :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => PayoutStatus -> Id SP.Person -> m [DailyStats]
findAllByPayoutStatusAndReferralEarningsAndDriver status (Id driverId) = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.payoutStatus $ Se.Eq (Just status),
          Se.Is Beam.driverId $ Se.Eq driverId,
          Se.Is Beam.referralEarnings $ Se.GreaterThan (Just 0.0)
        ]
    ]

findAllByDateAndPayoutStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Maybe Int -> Maybe Int -> Day -> PayoutStatus -> Id DMOC.MerchantOperatingCity -> m [DailyStats]
findAllByDateAndPayoutStatus limit offset merchantLocalDate payoutStatus merchantOpCityId = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate,
          Se.Is Beam.payoutStatus $ Se.Eq (Just payoutStatus),
          Se.Is Beam.referralEarnings $ Se.GreaterThan (Just 0.0),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Just $ getId merchantOpCityId)
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

findAllInRangeByDriverId_ :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id SP.Person -> Day -> Day -> m [DailyStats]
findAllInRangeByDriverId_ (Id driverId) from to = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq driverId,
          Se.Is Beam.merchantLocalDate $ Se.GreaterThanOrEq from,
          Se.Is Beam.merchantLocalDate $ Se.LessThanOrEq to
        ]
    ]

updateOnlineDurationByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id SP.Person ->
  Day ->
  Seconds ->
  m ()
updateOnlineDurationByDriverId driverId merchantLocalDate onlineDuration = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.updatedAt _now,
      Se.Set Beam.onlineDuration $ Just onlineDuration
    ]
    [Se.And [Se.Is Beam.driverId $ Se.Eq (getId driverId), Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate]]

deleteAllByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id SP.Person ->
  m ()
deleteAllByDriverId driverId = do
  deleteWithKV [Se.Is Beam.driverId $ Se.Eq (getId driverId)]
