module Storage.Queries.SearchTryExtra where

import qualified Database.Beam.Query ()
import Domain.Types.SearchRequest (SearchRequest)
import Domain.Types.SearchTry as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.SearchTry as BeamST
import Storage.Queries.OrphanInstances.SearchTry ()

-- Extra code goes here --

findLastByRequestId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id SearchRequest ->
  m (Maybe SearchTry)
findLastByRequestId (Id searchRequest) = findAllWithOptionsKV [Se.Is BeamST.requestId $ Se.Eq searchRequest] (Se.Desc BeamST.searchRepeatCounter) (Just 1) Nothing <&> listToMaybe

findTryByRequestId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id SearchRequest ->
  m (Maybe SearchTry)
findTryByRequestId (Id searchRequest) =
  findAllWithOptionsKV
    [Se.Is BeamST.requestId $ Se.Eq searchRequest]
    (Se.Desc BeamST.searchRepeatCounter)
    (Just 1)
    Nothing
    <&> listToMaybe

findActiveTryByQuoteId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Text ->
  m (Maybe SearchTry)
findActiveTryByQuoteId quoteId =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamST.estimateId $ Se.Eq quoteId,
          Se.Is BeamST.status $ Se.Eq ACTIVE
        ]
    ]
    (Se.Desc BeamST.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe

getSearchTryStatusAndValidTill ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id SearchTry ->
  m (Maybe (UTCTime, SearchTryStatus))
getSearchTryStatusAndValidTill (Id searchTryId) = findOneWithKV [Se.Is BeamST.id $ Se.Eq searchTryId] <&> fmap (\st -> (Domain.validTill st, Domain.status st))

cancelActiveTriesByRequestId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id SearchRequest ->
  m ()
cancelActiveTriesByRequestId (Id searchId) = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamST.status CANCELLED,
      Se.Set BeamST.updatedAt now
    ]
    [ Se.And
        [ Se.Is BeamST.requestId $ Se.Eq searchId,
          Se.Is BeamST.status $ Se.Eq ACTIVE
        ]
    ]
