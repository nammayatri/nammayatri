{-# OPTIONS_GHC -Wno-deprecations #-}

module Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourney
  ( findById,
    findAll,
    findByIds,
    findByJourneyType,
    clearCache,
    clearAllCache,
  )
where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourney as Queries
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyExtra as QueriesExtra
import Lib.IncentiveJourney.Types.Actor (JourneyActor, actorCachePrefix)

findById ::
  (BeamFlow m r) =>
  JourneyActor ->
  Id DIJ.IncentiveJourney ->
  m (Maybe DIJ.IncentiveJourney)
findById actor journeyId =
  Hedis.withCrossAppRedis (Hedis.safeGet (makeByIdKey actor journeyId)) >>= \case
    Just journey -> pure journey
    Nothing -> do
      mbJourney <- Queries.findById journeyId
      whenJust mbJourney $ cacheJourney actor
      pure mbJourney

findAll ::
  (BeamFlow m r) =>
  JourneyActor ->
  Maybe Int ->
  Maybe Int ->
  m [DIJ.IncentiveJourney]
findAll _actor = Queries.findAll

findByJourneyType ::
  (BeamFlow m r) =>
  JourneyActor ->
  Maybe Int ->
  Maybe Int ->
  DIJ.IncentiveJourneyType ->
  m [DIJ.IncentiveJourney]
findByJourneyType _actor = QueriesExtra.findByJourneyType

findByIds ::
  (BeamFlow m r) =>
  JourneyActor ->
  [Id DIJ.IncentiveJourney] ->
  m [DIJ.IncentiveJourney]
findByIds actor rawIds = do
  let ids = nub rawIds
  cachedPairs <- forM ids $ \jid -> do
    mbCached <- Hedis.withCrossAppRedis (Hedis.safeGet (makeByIdKey actor jid))
    pure (jid, mbCached :: Maybe DIJ.IncentiveJourney)
  let hits = mapMaybe snd cachedPairs
      missIds = [jid | (jid, Nothing) <- cachedPairs]
  misses <-
    if null missIds
      then pure []
      else do
        fetched <- QueriesExtra.findByIds missIds
        forM_ fetched (cacheJourney actor)
        pure fetched
  let byId = Map.fromList [(j.id, j) | j <- hits <> misses]
  pure $ mapMaybe (`Map.lookup` byId) ids

cacheJourney :: (MonadFlow m, CacheFlow m r) => JourneyActor -> DIJ.IncentiveJourney -> m ()
cacheJourney actor journey = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeByIdKey actor journey.id) journey expTime

clearCache :: (CacheFlow m r) => JourneyActor -> DIJ.IncentiveJourney -> m ()
clearCache actor journey =
  Hedis.runInMultiCloudRedisWrite $
    Hedis.withCrossAppRedis $
      void $
        Hedis.del (makeByIdKey actor journey.id)

clearAllCache :: (CacheFlow m r) => JourneyActor -> m ()
clearAllCache _actor = pure ()

makeByIdKey :: JourneyActor -> Id DIJ.IncentiveJourney -> Text
makeByIdKey actor journeyId =
  actorCachePrefix actor <> ":CachedQueries:IncentiveJourney:Id-" <> journeyId.getId
