{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RiderPreferences (module Storage.Queries.RiderPreferences, module ReExport) where

import qualified Data.Aeson
import qualified Domain.Types.Extra.RiderPreferences
import qualified Domain.Types.Person
import qualified Domain.Types.RiderPreferences
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RiderPreferences as Beam
import Storage.Queries.RiderPreferencesExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RiderPreferences.RiderPreferences -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RiderPreferences.RiderPreferences] -> m ())
createMany = traverse_ create

deleteByRiderIdAndId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.RiderPreferences.RiderPreferences -> m ())
deleteByRiderIdAndId riderId id = do deleteWithKV [Se.And [Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId), Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

findAllByRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.RiderPreferences.RiderPreferences]))
findAllByRiderId riderId = do findAllWithKV [Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId)]

findByRiderIdAndType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.Extra.RiderPreferences.PreferenceType -> m ([Domain.Types.RiderPreferences.RiderPreferences]))
findByRiderIdAndType riderId preferenceType = do findAllWithKV [Se.And [Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId), Se.Is Beam.preferenceType $ Se.Eq preferenceType]]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RiderPreferences.RiderPreferences -> m (Maybe Domain.Types.RiderPreferences.RiderPreferences))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RiderPreferences.RiderPreferences -> m ())
updateByPrimaryKey (Domain.Types.RiderPreferences.RiderPreferences {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.preferenceData (Data.Aeson.toJSON preferenceData),
      Se.Set Beam.preferenceType preferenceType,
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
