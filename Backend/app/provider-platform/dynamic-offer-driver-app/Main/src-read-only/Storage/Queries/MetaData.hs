{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MetaData where

import qualified Data.Text
import qualified Domain.Types.MetaData
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MetaData as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MetaData.MetaData -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MetaData.MetaData] -> m ())
createMany = traverse_ create

updateMetaData ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateMetaData device deviceOS deviceDateTime appPermissions driverId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.device device,
      Se.Set Beam.deviceOS deviceOS,
      Se.Set Beam.deviceDateTime deviceDateTime,
      Se.Set Beam.appPermissions appPermissions,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.MetaData.MetaData))
findByPrimaryKey driverId = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MetaData.MetaData -> m ())
updateByPrimaryKey (Domain.Types.MetaData.MetaData {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.appPermissions appPermissions,
      Se.Set Beam.device device,
      Se.Set Beam.deviceDateTime deviceDateTime,
      Se.Set Beam.deviceOS deviceOS,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]

instance FromTType' Beam.MetaData Domain.Types.MetaData.MetaData where
  fromTType' (Beam.MetaDataT {..}) = do
    pure $
      Just
        Domain.Types.MetaData.MetaData
          { appPermissions = appPermissions,
            createdAt = createdAt,
            device = device,
            deviceDateTime = deviceDateTime,
            deviceOS = deviceOS,
            driverId = Kernel.Types.Id.Id driverId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MetaData Domain.Types.MetaData.MetaData where
  toTType' (Domain.Types.MetaData.MetaData {..}) = do
    Beam.MetaDataT
      { Beam.appPermissions = appPermissions,
        Beam.createdAt = createdAt,
        Beam.device = device,
        Beam.deviceDateTime = deviceDateTime,
        Beam.deviceOS = deviceOS,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.updatedAt = updatedAt
      }
