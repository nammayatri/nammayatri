{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PersonStats (module Storage.Queries.PersonStats, module ReExport) where

import qualified Domain.Types.Person
import qualified Domain.Types.PersonStats
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PersonStats as Beam
import Storage.Queries.PersonStatsExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PersonStats.PersonStats -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PersonStats.PersonStats] -> m ())
createMany = traverse_ create

findByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.PersonStats.PersonStats))
findByPersonId personId = do findOneWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

updateBacklogAndReferredByPayoutStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Domain.Types.PersonStats.PayoutStatus -> Kernel.Prelude.Maybe Domain.Types.PersonStats.PayoutStatus -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateBacklogAndReferredByPayoutStatus backlogPayoutStatus referredByEarningsPayoutStatus personId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.backlogPayoutStatus backlogPayoutStatus,
      Se.Set Beam.referredByEarningsPayoutStatus referredByEarningsPayoutStatus,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

updateBacklogAndReferredByPayoutStatusAndAmountPaid ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Domain.Types.PersonStats.PayoutStatus -> Kernel.Prelude.Maybe Domain.Types.PersonStats.PayoutStatus -> Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateBacklogAndReferredByPayoutStatusAndAmountPaid backlogPayoutStatus referredByEarningsPayoutStatus referralAmountPaid personId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.backlogPayoutStatus backlogPayoutStatus,
      Se.Set Beam.referredByEarningsPayoutStatus referredByEarningsPayoutStatus,
      Se.Set Beam.referralAmountPaid (Kernel.Prelude.Just referralAmountPaid),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

updateBacklogPayoutStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Domain.Types.PersonStats.PayoutStatus -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateBacklogPayoutStatus backlogPayoutStatus personId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.backlogPayoutStatus backlogPayoutStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

updateBacklogStatusAndAmountPaid ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Domain.Types.PersonStats.PayoutStatus -> Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateBacklogStatusAndAmountPaid backlogPayoutStatus referralAmountPaid personId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.backlogPayoutStatus backlogPayoutStatus,
      Se.Set Beam.referralAmountPaid (Kernel.Prelude.Just referralAmountPaid),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

updateEarningsAndActivations ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateEarningsAndActivations referralEarnings backlogPayoutAmount validActivations personId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.referralEarnings (Kernel.Prelude.Just referralEarnings),
      Se.Set Beam.backlogPayoutAmount (Kernel.Prelude.Just backlogPayoutAmount),
      Se.Set Beam.validActivations (Kernel.Prelude.Just validActivations),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

updateReferralAmountPaid :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateReferralAmountPaid referralAmountPaid personId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.referralAmountPaid (Kernel.Prelude.Just referralAmountPaid), Se.Set Beam.updatedAt _now] [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

updateReferralCount :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateReferralCount referralCount personId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.referralCount referralCount, Se.Set Beam.updatedAt _now] [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

updateReferralEarningsAndValidActivations ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateReferralEarningsAndValidActivations referralEarnings validActivations personId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.referralEarnings (Kernel.Prelude.Just referralEarnings),
      Se.Set Beam.validActivations (Kernel.Prelude.Just validActivations),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

updateReferredByEarning :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateReferredByEarning referredByEarnings personId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.referredByEarnings (Kernel.Prelude.Just referredByEarnings), Se.Set Beam.updatedAt _now] [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

updateReferredByEarningsPayoutStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Domain.Types.PersonStats.PayoutStatus -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateReferredByEarningsPayoutStatus referredByEarningsPayoutStatus personId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.referredByEarningsPayoutStatus referredByEarningsPayoutStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

updateReferredByEarningsPayoutStatusAndAmountPaid ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Domain.Types.PersonStats.PayoutStatus -> Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateReferredByEarningsPayoutStatusAndAmountPaid referredByEarningsPayoutStatus referralAmountPaid personId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.referredByEarningsPayoutStatus referredByEarningsPayoutStatus,
      Se.Set Beam.referralAmountPaid (Kernel.Prelude.Just referralAmountPaid),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.PersonStats.PersonStats))
findByPrimaryKey personId = do findOneWithKV [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PersonStats.PersonStats -> m ())
updateByPrimaryKey (Domain.Types.PersonStats.PersonStats {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.backlogPayoutAmount (Kernel.Prelude.Just backlogPayoutAmount),
      Se.Set Beam.backlogPayoutStatus backlogPayoutStatus,
      Se.Set Beam.completedRides completedRides,
      Se.Set Beam.driverCancelledRides driverCancelledRides,
      Se.Set Beam.eveningPeakRides eveningPeakRides,
      Se.Set Beam.isBackfilled isBackfilled,
      Se.Set Beam.morningPeakRides morningPeakRides,
      Se.Set Beam.offPeakRides offPeakRides,
      Se.Set Beam.referralAmountPaid (Kernel.Prelude.Just referralAmountPaid),
      Se.Set Beam.referralCount referralCount,
      Se.Set Beam.referralEarnings (Kernel.Prelude.Just referralEarnings),
      Se.Set Beam.referredByEarnings (Kernel.Prelude.Just referredByEarnings),
      Se.Set Beam.referredByEarningsPayoutStatus referredByEarningsPayoutStatus,
      Se.Set Beam.ticketsBookedInEvent ticketsBookedInEvent,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.userCancelledRides userCancelledRides,
      Se.Set Beam.validActivations (Kernel.Prelude.Just validActivations),
      Se.Set Beam.weekdayRides weekdayRides,
      Se.Set Beam.weekendPeakRides weekendPeakRides,
      Se.Set Beam.weekendRides weekendRides
    ]
    [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]]
