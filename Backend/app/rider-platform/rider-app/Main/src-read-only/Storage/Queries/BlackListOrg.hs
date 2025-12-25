{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BlackListOrg where

import qualified Domain.Types.BlackListOrg
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Domain
import Kernel.Types.Error
import qualified Kernel.Types.Id
import qualified Kernel.Types.Registry
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BlackListOrg as Beam
import Storage.Queries.Transformers.BlackListOrg

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BlackListOrg.BlackListOrg -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.BlackListOrg.BlackListOrg] -> m ())
createMany = traverse_ create

findBySubscriberIdAndDomain ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.ShortId Kernel.Types.Registry.Subscriber -> Kernel.Types.Beckn.Domain.Domain -> m (Maybe Domain.Types.BlackListOrg.BlackListOrg))
findBySubscriberIdAndDomain subscriberId domain = do findOneWithKV [Se.And [Se.Is Beam.subscriberId $ Se.Eq (Kernel.Types.Id.getShortId subscriberId), Se.Is Beam.domain $ Se.Eq domain]]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BlackListOrg.BlackListOrg -> m (Maybe Domain.Types.BlackListOrg.BlackListOrg))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BlackListOrg.BlackListOrg -> m ())
updateByPrimaryKey (Domain.Types.BlackListOrg.BlackListOrg {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.domain domain,
      Se.Set Beam.subscriberId (Kernel.Types.Id.getShortId subscriberId),
      Se.Set Beam.updatedAt (Just _now)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.BlackListOrg Domain.Types.BlackListOrg.BlackListOrg where
  fromTType' (Beam.BlackListOrgT {..}) = do
    createdAt' <- getCreatedAt createdAt
    updatedAt' <- getUpdatedAt updatedAt
    pure $ Just Domain.Types.BlackListOrg.BlackListOrg {createdAt = createdAt', domain = domain, id = Kernel.Types.Id.Id id, subscriberId = Kernel.Types.Id.ShortId subscriberId, updatedAt = updatedAt'}

instance ToTType' Beam.BlackListOrg Domain.Types.BlackListOrg.BlackListOrg where
  toTType' (Domain.Types.BlackListOrg.BlackListOrg {..}) = do
    Beam.BlackListOrgT
      { Beam.createdAt = Kernel.Prelude.Just createdAt,
        Beam.domain = domain,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.subscriberId = Kernel.Types.Id.getShortId subscriberId,
        Beam.updatedAt = Kernel.Prelude.Just updatedAt
      }
