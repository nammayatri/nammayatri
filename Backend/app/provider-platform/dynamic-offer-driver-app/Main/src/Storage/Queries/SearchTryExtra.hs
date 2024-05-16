{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SearchTryExtra where

import qualified Database.Beam.Query ()
import Domain.Types.Common
import Domain.Types.SearchRequest (SearchRequest)
import Domain.Types.SearchTry as Domain
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SearchTry as BeamST
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.Queries.OrphanInstances.SearchTry
import qualified Storage.Queries.SearchRequest as QR

-- Extra code goes here --

findLastByRequestId ::
  KvDbFlow m r =>
  Id SearchRequest ->
  m (Maybe SearchTry)
findLastByRequestId (Id searchRequest) = findAllWithOptionsKV [Se.Is BeamST.requestId $ Se.Eq searchRequest] (Se.Desc BeamST.searchRepeatCounter) (Just 1) Nothing <&> listToMaybe

findTryByRequestId ::
  KvDbFlow m r =>
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
  KvDbFlow m r =>
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
  KvDbFlow m r =>
  Id SearchTry ->
  m (Maybe (UTCTime, SearchTryStatus))
getSearchTryStatusAndValidTill (Id searchTryId) = findOneWithKV [Se.Is BeamST.id $ Se.Eq searchTryId] <&> fmap (\st -> (Domain.validTill st, Domain.status st))

cancelActiveTriesByRequestId ::
  KvDbFlow m r =>
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
