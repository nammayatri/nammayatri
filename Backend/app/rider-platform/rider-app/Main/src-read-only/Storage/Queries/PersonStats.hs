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
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PersonStats as Beam
import Storage.Queries.PersonStatsExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.PersonStats.PersonStats -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.PersonStats.PersonStats] -> m ())
createMany = traverse_ create

findByPersonId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.PersonStats.PersonStats))
findByPersonId (Kernel.Types.Id.Id personId) = do findOneWithKV [Se.Is Beam.personId $ Se.Eq personId]

updateReferralCount :: KvDbFlow m r => (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateReferralCount referralCount (Kernel.Types.Id.Id personId) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.referralCount referralCount, Se.Set Beam.updatedAt _now] [Se.Is Beam.personId $ Se.Eq personId]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.PersonStats.PersonStats))
findByPrimaryKey (Kernel.Types.Id.Id personId) = do findOneWithKV [Se.And [Se.Is Beam.personId $ Se.Eq personId]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.PersonStats.PersonStats -> m ())
updateByPrimaryKey (Domain.Types.PersonStats.PersonStats {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.completedRides completedRides,
      Se.Set Beam.createdAt (Kernel.Prelude.Just createdAt),
      Se.Set Beam.driverCancelledRides driverCancelledRides,
      Se.Set Beam.eveningPeakRides eveningPeakRides,
      Se.Set Beam.morningPeakRides morningPeakRides,
      Se.Set Beam.offPeakRides offPeakRides,
      Se.Set Beam.referralCount referralCount,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.userCancelledRides userCancelledRides,
      Se.Set Beam.weekdayRides weekdayRides,
      Se.Set Beam.weekendPeakRides weekendPeakRides,
      Se.Set Beam.weekendRides weekendRides
    ]
    [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]]
