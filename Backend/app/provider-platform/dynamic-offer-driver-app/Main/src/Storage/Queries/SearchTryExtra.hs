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
findLastByRequestId (Id searchRequest) = findAllWithKVAndConditionalDB [Se.Is BeamST.requestId $ Se.Eq searchRequest] (Just (Se.Desc BeamST.searchRepeatCounter)) <&> listToMaybe

findAllByRequestId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id SearchRequest ->
  m [SearchTry]
findAllByRequestId (Id searchRequest) = findAllWithKVAndConditionalDB [Se.Is BeamST.requestId $ Se.Eq searchRequest] (Just (Se.Desc BeamST.searchRepeatCounter))

findRecentByRequestIds ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [Id SearchRequest] ->
  Int ->
  m [SearchTry]
findRecentByRequestIds requestIds limit =
  findAllWithOptionsKV
    [Se.Is BeamST.requestId $ Se.In (getId <$> requestIds)]
    (Se.Desc BeamST.createdAt)
    (Just limit)
    Nothing

findTryByRequestId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id SearchRequest ->
  m (Maybe SearchTry)
findTryByRequestId (Id searchRequest) =
  findAllWithKVAndConditionalDB
    [Se.Is BeamST.requestId $ Se.Eq searchRequest]
    (Just (Se.Desc BeamST.searchRepeatCounter))
    <&> listToMaybe

findActiveTryByQuoteId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Text ->
  m (Maybe SearchTry)
findActiveTryByQuoteId quoteId =
  findAllWithKVAndConditionalDB
    [ Se.And
        [ Se.Is BeamST.estimateId $ Se.Eq quoteId,
          Se.Is BeamST.status $ Se.Eq ACTIVE
        ]
    ]
    (Just (Se.Desc BeamST.createdAt))
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

-- | No-op if there's no live "find a better driver" stand-by search for this booking -
-- the common case on almost every GPS ping. Used by the proximity-abort hook: cancel
-- the stand-by search the moment the real, currently-assigned driver gets close enough
-- to pickup that searching for someone better no longer makes sense.
cancelBetterDriverSearchByBookingId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Text ->
  m ()
cancelBetterDriverSearchByBookingId bookingId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamST.status CANCELLED,
      Se.Set BeamST.updatedAt now
    ]
    [ Se.And
        [ Se.Is BeamST.standInForBookingId $ Se.Eq (Just bookingId),
          Se.Is BeamST.status $ Se.Eq ACTIVE,
          Se.Is BeamST.searchRepeatType $ Se.Eq BETTER_DRIVER_SEARCH
        ]
    ]

-- | Used at driver-cancellation time to decide promote-vs-cancel for any live
-- "find a better driver" stand-by search on this booking - see
-- SharedLogic.Cancel.reAllocateBookingIfPossible.
findActiveBetterDriverSearchByBookingId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Text ->
  m (Maybe SearchTry)
findActiveBetterDriverSearchByBookingId bookingId =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamST.standInForBookingId $ Se.Eq (Just bookingId),
          Se.Is BeamST.status $ Se.Eq ACTIVE,
          Se.Is BeamST.searchRepeatType $ Se.Eq BETTER_DRIVER_SEARCH
        ]
    ]

-- | The booking's own driver-cancel reallocation has decided a replacement IS allowed:
-- let this already-running stand-by search serve as that replacement instead of
-- starting a second, duplicate one. Re-tagging away from BETTER_DRIVER_SEARCH also
-- restores normal on-expiry rider notification (see notifyBapOnExpiry) and stops
-- SharedLogic.Allocator.Jobs.CheckBetterDriverSearchProximity from ticking further.
promoteBetterDriverSearchToReallocation ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id SearchTry ->
  m ()
promoteBetterDriverSearchToReallocation (Id searchTryId) = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamST.searchRepeatType REALLOCATION,
      Se.Set BeamST.updatedAt now
    ]
    [Se.Is BeamST.id $ Se.Eq searchTryId]
