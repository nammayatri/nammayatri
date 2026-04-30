{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SvpJourney (module Storage.Queries.SvpJourney, module ReExport) where

import qualified Domain.Types.Person
import qualified Domain.Types.SvpJourney
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SvpJourney as Beam
import Storage.Queries.SvpJourneyExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SvpJourney.SvpJourney -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SvpJourney.SvpJourney] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SvpJourney.SvpJourney -> m (Maybe Domain.Types.SvpJourney.SvpJourney))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByRiderIdAndStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.SvpJourney.SvpJourneyStatus -> m (Maybe Domain.Types.SvpJourney.SvpJourney))
findByRiderIdAndStatus riderId status = do findOneWithKV [Se.And [Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId), Se.Is Beam.status $ Se.Eq status]]

updateStatusAndExitDetailsById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.SvpJourney.SvpJourneyStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.Currency -> Kernel.Types.Id.Id Domain.Types.SvpJourney.SvpJourney -> m ())
updateStatusAndExitDetailsById status exitStationCode exitTime fareCharged currency id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.exitStationCode exitStationCode,
      Se.Set Beam.exitTime exitTime,
      Se.Set Beam.fareCharged fareCharged,
      Se.Set Beam.currency currency,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SvpJourney.SvpJourney -> m (Maybe Domain.Types.SvpJourney.SvpJourney))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SvpJourney.SvpJourney -> m ())
updateByPrimaryKey (Domain.Types.SvpJourney.SvpJourney {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.currency currency,
      Se.Set Beam.entryStationCode entryStationCode,
      Se.Set Beam.entryTime entryTime,
      Se.Set Beam.exitStationCode exitStationCode,
      Se.Set Beam.exitTime exitTime,
      Se.Set Beam.fareCharged fareCharged,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.status status,
      Se.Set Beam.tktSlNo tktSlNo,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
