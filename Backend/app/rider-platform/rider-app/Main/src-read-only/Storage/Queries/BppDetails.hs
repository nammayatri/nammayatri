{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BppDetails where

import qualified Domain.Types.BppDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BppDetails as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BppDetails.BppDetails -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.BppDetails.BppDetails] -> m ())
createMany = traverse_ create

findBySubscriberIdAndDomain :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Domain.Types.BppDetails.BppDetails))
findBySubscriberIdAndDomain subscriberId domain = do findOneWithKV [Se.And [Se.Is Beam.subscriberId $ Se.Eq subscriberId, Se.Is Beam.domain $ Se.Eq domain]]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BppDetails.BppDetails -> m (Maybe Domain.Types.BppDetails.BppDetails))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BppDetails.BppDetails -> m ())
updateByPrimaryKey (Domain.Types.BppDetails.BppDetails {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.description description,
      Se.Set Beam.domain domain,
      Se.Set Beam.logoUrl logoUrl,
      Se.Set Beam.name name,
      Se.Set Beam.subscriberId subscriberId,
      Se.Set Beam.supportNumber supportNumber,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.BppDetails Domain.Types.BppDetails.BppDetails where
  fromTType' (Beam.BppDetailsT {..}) = do
    pure $
      Just
        Domain.Types.BppDetails.BppDetails
          { description = description,
            domain = domain,
            id = Kernel.Types.Id.Id id,
            logoUrl = logoUrl,
            name = name,
            subscriberId = subscriberId,
            supportNumber = supportNumber,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.BppDetails Domain.Types.BppDetails.BppDetails where
  toTType' (Domain.Types.BppDetails.BppDetails {..}) = do
    Beam.BppDetailsT
      { Beam.description = description,
        Beam.domain = domain,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.logoUrl = logoUrl,
        Beam.name = name,
        Beam.subscriberId = subscriberId,
        Beam.supportNumber = supportNumber,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
