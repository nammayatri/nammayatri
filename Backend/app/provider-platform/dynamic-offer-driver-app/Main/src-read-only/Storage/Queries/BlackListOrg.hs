{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BlackListOrg where

import qualified Domain.Types.BlackListOrg
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Domain
import Kernel.Types.Error
import qualified Kernel.Types.Id
import qualified Kernel.Types.Registry
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BlackListOrg as Beam

create :: KvDbFlow m r => (Domain.Types.BlackListOrg.BlackListOrg -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.BlackListOrg.BlackListOrg] -> m ())
createMany = traverse_ create

findBySubscriberIdAndDomain :: KvDbFlow m r => (Kernel.Types.Id.ShortId Kernel.Types.Registry.Subscriber -> Kernel.Types.Beckn.Domain.Domain -> m (Maybe Domain.Types.BlackListOrg.BlackListOrg))
findBySubscriberIdAndDomain (Kernel.Types.Id.ShortId subscriberId) domain = do findOneWithKV [Se.And [Se.Is Beam.subscriberId $ Se.Eq subscriberId, Se.Is Beam.domain $ Se.Eq domain]]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.BlackListOrg.BlackListOrg -> m (Maybe Domain.Types.BlackListOrg.BlackListOrg))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.BlackListOrg.BlackListOrg -> m ())
updateByPrimaryKey (Domain.Types.BlackListOrg.BlackListOrg {..}) = do
  updateWithKV
    [ Se.Set Beam.domain domain,
      Se.Set Beam.subscriberId (Kernel.Types.Id.getShortId subscriberId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.BlackListOrg Domain.Types.BlackListOrg.BlackListOrg where
  fromTType' (Beam.BlackListOrgT {..}) = do pure $ Just Domain.Types.BlackListOrg.BlackListOrg {domain = domain, id = Kernel.Types.Id.Id id, subscriberId = Kernel.Types.Id.ShortId subscriberId}

instance ToTType' Beam.BlackListOrg Domain.Types.BlackListOrg.BlackListOrg where
  toTType' (Domain.Types.BlackListOrg.BlackListOrg {..}) = do
    Beam.BlackListOrgT
      { Beam.domain = domain,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.subscriberId = Kernel.Types.Id.getShortId subscriberId
      }
