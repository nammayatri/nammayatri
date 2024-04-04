{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PortalConfigs where

import qualified Domain.Types.PortalConfigs
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PortalConfigs as Beam

create :: KvDbFlow m r => (Domain.Types.PortalConfigs.PortalConfigs -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.PortalConfigs.PortalConfigs] -> m ())
createMany = traverse_ create

findByConfigName :: KvDbFlow m r => (Kernel.Prelude.Text -> m (Maybe Domain.Types.PortalConfigs.PortalConfigs))
findByConfigName configName = do findOneWithKV [Se.Is Beam.configName $ Se.Eq configName]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.PortalConfigs.PortalConfigs -> m (Maybe Domain.Types.PortalConfigs.PortalConfigs))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.PortalConfigs.PortalConfigs -> m ())
updateByPrimaryKey (Domain.Types.PortalConfigs.PortalConfigs {..}) = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.configName configName, Se.Set Beam.createdAt createdAt, Se.Set Beam.updatedAt _now, Se.Set Beam.value value] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.PortalConfigs Domain.Types.PortalConfigs.PortalConfigs where
  fromTType' (Beam.PortalConfigsT {..}) = do
    pure $
      Just
        Domain.Types.PortalConfigs.PortalConfigs
          { configName = configName,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            updatedAt = updatedAt,
            value = value
          }

instance ToTType' Beam.PortalConfigs Domain.Types.PortalConfigs.PortalConfigs where
  toTType' (Domain.Types.PortalConfigs.PortalConfigs {..}) = do
    Beam.PortalConfigsT
      { Beam.configName = configName,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.updatedAt = updatedAt,
        Beam.value = value
      }
