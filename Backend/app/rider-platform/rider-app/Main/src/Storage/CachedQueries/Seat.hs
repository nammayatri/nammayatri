module Storage.CachedQueries.Seat where

import qualified Domain.Types.Seat as Domain
import qualified Domain.Types.SeatLayout as DomainLayout
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id (Id, getId)
import Kernel.Utils.Common
import qualified Storage.Queries.Seat as Queries

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id Domain.Seat ->
  m (Maybe Domain.Seat)
findById seatId = do
  let cacheKey = makeSeatIdKey seatId
  Hedis.safeGet cacheKey >>= \case
    Just val -> return val
    Nothing -> do
      val <- Queries.findById seatId
      whenJust val $ \v -> do
        expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
        Hedis.setExp cacheKey v expTime
      return val

makeSeatIdKey :: Id Domain.Seat -> Text
makeSeatIdKey seatId = "CachedQueries:Seat:Id-" <> getId seatId

findAllByLayoutId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DomainLayout.SeatLayout ->
  m [Domain.Seat]
findAllByLayoutId layoutId = do
  let cacheKey = makeLayoutIdKey layoutId
  Hedis.safeGet cacheKey >>= \case
    Just val -> return val
    Nothing -> do
      val <- Queries.findAllByLayoutId layoutId
      unless (null val) $ do
        expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
        Hedis.setExp cacheKey val expTime
      return val

makeLayoutIdKey :: Id DomainLayout.SeatLayout -> Text
makeLayoutIdKey layoutId = "CachedQueries:Seat:LayoutId-" <> getId layoutId

invalidateCacheByLayoutId :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id DomainLayout.SeatLayout -> m ()
invalidateCacheByLayoutId layoutId = do
  let cacheKey = makeLayoutIdKey layoutId
  void $ Hedis.del cacheKey

invalidateCacheBySeatId :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id Domain.Seat -> m ()
invalidateCacheBySeatId seatId = void $ Hedis.del (makeSeatIdKey seatId)
