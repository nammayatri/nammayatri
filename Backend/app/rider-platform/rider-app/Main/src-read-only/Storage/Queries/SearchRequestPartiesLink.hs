{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SearchRequestPartiesLink where

import qualified Domain.Types.SearchRequest
import qualified Domain.Types.SearchRequestPartiesLink
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRequestPartiesLink as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SearchRequestPartiesLink.SearchRequestPartiesLink -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SearchRequestPartiesLink.SearchRequestPartiesLink] -> m ())
createMany = traverse_ create

findAllBySearchRequestId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m ([Domain.Types.SearchRequestPartiesLink.SearchRequestPartiesLink]))
findAllBySearchRequestId searchRequestId = do findAllWithKV [Se.Is Beam.searchRequestId $ Se.Eq (Kernel.Types.Id.getId searchRequestId)]

instance FromTType' Beam.SearchRequestPartiesLink Domain.Types.SearchRequestPartiesLink.SearchRequestPartiesLink where
  fromTType' (Beam.SearchRequestPartiesLinkT {..}) = do
    pure $
      Just
        Domain.Types.SearchRequestPartiesLink.SearchRequestPartiesLink
          { id = Kernel.Types.Id.Id id,
            partyId = Kernel.Types.Id.Id partyId,
            partyName = partyName,
            partyType = partyType,
            searchRequestId = Kernel.Types.Id.Id searchRequestId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.SearchRequestPartiesLink Domain.Types.SearchRequestPartiesLink.SearchRequestPartiesLink where
  toTType' (Domain.Types.SearchRequestPartiesLink.SearchRequestPartiesLink {..}) = do
    Beam.SearchRequestPartiesLinkT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.partyId = Kernel.Types.Id.getId partyId,
        Beam.partyName = partyName,
        Beam.partyType = partyType,
        Beam.searchRequestId = Kernel.Types.Id.getId searchRequestId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
