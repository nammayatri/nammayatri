{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DailyStats (module Storage.Queries.DailyStats, module ReExport) where

import qualified Data.Text
import qualified Data.Time.Calendar
import qualified Domain.Types.DailyStats
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DailyStats as Beam
import Storage.Queries.DailyStatsExtra as ReExport
import Storage.Queries.Transformers.DailyStats

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DailyStats.DailyStats -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DailyStats.DailyStats] -> m ())
createMany = traverse_ create

findByDriverIdAndDate :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> Data.Time.Calendar.Day -> m (Maybe Domain.Types.DailyStats.DailyStats))
findByDriverIdAndDate driverId merchantLocalDate = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate]]

updateByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Int -> Kernel.Types.Common.Meters -> Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Common.Seconds -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Data.Time.Calendar.Day -> m ())
updateByDriverId totalEarnings numRides totalDistance tollCharges bonusEarnings totalRideTime driverId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.totalEarnings (Kernel.Prelude.roundToIntegral totalEarnings),
      Se.Set Beam.totalEarningsAmount (Kernel.Prelude.Just totalEarnings),
      Se.Set Beam.numRides numRides,
      Se.Set Beam.totalDistance totalDistance,
      Se.Set Beam.tollCharges (Kernel.Prelude.Just tollCharges),
      Se.Set Beam.bonusEarnings (Kernel.Prelude.Just bonusEarnings),
      Se.Set Beam.totalRideTime (Kernel.Prelude.Just totalRideTime),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate]]

updateMerchantIdAndCityIdByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateMerchantIdAndCityIdByDriverId merchantId merchantOperatingCityId driverId = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateNumDriversOnboardedByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Data.Time.Calendar.Day -> m ())
updateNumDriversOnboardedByDriverId numDriversOnboarded driverId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.numDriversOnboarded (Kernel.Prelude.Just numDriversOnboarded), Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updateNumFleetsOnboardedByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Data.Time.Calendar.Day -> m ())
updateNumFleetsOnboardedByDriverId numFleetsOnboarded driverId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.numFleetsOnboarded (Kernel.Prelude.Just numFleetsOnboarded), Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updatePayoutOrderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Data.Text.Text -> Data.Text.Text -> m ())
updatePayoutOrderId payoutOrderId id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.payoutOrderId payoutOrderId, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq id]

updatePayoutOrderIdAndStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Data.Text.Text -> Domain.Types.DailyStats.PayoutStatus -> Data.Text.Text -> m ())
updatePayoutOrderIdAndStatus payoutOrderId payoutStatus id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.payoutOrderId payoutOrderId, Se.Set Beam.payoutStatus (Kernel.Prelude.Just payoutStatus), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq id]

updatePayoutStatusById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DailyStats.PayoutStatus -> Data.Text.Text -> m ())
updatePayoutStatusById payoutStatus id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.payoutStatus (Kernel.Prelude.Just payoutStatus), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq id]

updateReferralCount :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Data.Time.Calendar.Day -> m ())
updateReferralCount referralCounts driverId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.referralCounts (Kernel.Prelude.Just referralCounts), Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updateReferralStatsByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Int -> Kernel.Types.Common.HighPrecMoney -> Domain.Types.DailyStats.PayoutStatus -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Data.Time.Calendar.Day -> m ())
updateReferralStatsByDriverId activatedValidRides referralEarnings payoutStatus driverId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.activatedValidRides (Kernel.Prelude.Just activatedValidRides),
      Se.Set Beam.referralEarnings (Kernel.Prelude.Just referralEarnings),
      Se.Set Beam.payoutStatus (Kernel.Prelude.Just payoutStatus),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate]]

updateTipAmountByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Data.Time.Calendar.Day -> m ())
updateTipAmountByDriverId tipAmount driverId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.tipAmount (Kernel.Prelude.Just tipAmount), Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> m (Maybe Domain.Types.DailyStats.DailyStats))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DailyStats.DailyStats -> m ())
updateByPrimaryKey (Domain.Types.DailyStats.DailyStats {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.activatedValidRides (Kernel.Prelude.Just activatedValidRides),
      Se.Set Beam.bonusEarnings (Kernel.Prelude.Just bonusEarnings),
      Se.Set Beam.cancellationCharges (Kernel.Prelude.Just cancellationCharges),
      Se.Set Beam.currency (Kernel.Prelude.Just currency),
      Se.Set Beam.distanceUnit (Kernel.Prelude.Just distanceUnit),
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantLocalDate merchantLocalDate,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.numDriversOnboarded (Kernel.Prelude.Just numDriversOnboarded),
      Se.Set Beam.numFleetsOnboarded (Kernel.Prelude.Just numFleetsOnboarded),
      Se.Set Beam.numRides numRides,
      Se.Set Beam.onlineDuration onlineDuration,
      Se.Set Beam.payoutOrderId payoutOrderId,
      Se.Set Beam.payoutOrderStatus payoutOrderStatus,
      Se.Set Beam.payoutStatus (Kernel.Prelude.Just payoutStatus),
      Se.Set Beam.referralCounts (Kernel.Prelude.Just referralCounts),
      Se.Set Beam.referralEarnings (Kernel.Prelude.Just referralEarnings),
      Se.Set Beam.tipAmount (Kernel.Prelude.Just tipAmount),
      Se.Set Beam.tollCharges (Kernel.Prelude.Just tollCharges),
      Se.Set Beam.totalDistance totalDistance,
      Se.Set Beam.totalEarnings (Kernel.Prelude.roundToIntegral totalEarnings),
      Se.Set Beam.totalEarningsAmount (Kernel.Prelude.Just totalEarnings),
      Se.Set Beam.totalRideTime (Kernel.Prelude.Just totalRideTime),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq id]]
