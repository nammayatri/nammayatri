module SharedLogic.GetRatingCategories where

import Beckn.Types.Core.Taxi.API.Rating
import Beckn.Types.Core.Taxi.Rating.Category
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Utils.Common
import qualified SharedLogic.CallBPP as CallBPP
import Storage.CachedQueries.CacheConfig

type RatingCategoriesResp = BecknReq RatingCategories

getRatingCategories ::
  ( HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl],
    HasCacheConfig r,
    Hedis.HedisFlow m r,
    CoreMetrics m,
    HasBapInfo r m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  m RatingCategories
getRatingCategories providerUrl bppId city messageId =
  Hedis.safeGet (makeRatingCategoriesId bppId) >>= \case
    Just a ->
      return a
    Nothing ->
      findAndCache providerUrl bppId city messageId

makeRatingCategoriesId :: Text -> Text
makeRatingCategoriesId bppId = "RatingCategories:BppId:" <> bppId

cacheRatingCategories :: (CacheFlow m r) => Text -> RatingCategories -> m ()
cacheRatingCategories bppId listOfCategories = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeRatingCategoriesId bppId) listOfCategories expTime

findAndCache ::
  ( HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl],
    HasCacheConfig r,
    Hedis.HedisFlow m r,
    CoreMetrics m,
    HasBapInfo r m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  m RatingCategories
findAndCache providerUrl bppId city messageId = do
  req <- buildGetRatingCategoriesReq providerUrl bppId city messageId
  listOfCategories <- CallBPP.getRatingCategories providerUrl req
  cacheRatingCategories bppId listOfCategories
  return listOfCategories

buildGetRatingCategoriesReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  m GetRatingCategoriesReq
buildGetRatingCategoriesReq bppUrl bppId city messageId = do
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  context <- buildTaxiContext Context.GET_RATING_CATEGORIES messageId (Just messageId) bapIDs.cabs bapURIs.cabs (Just bppId) (Just bppUrl) city
  pure $ BecknReq context Empty

buildGetRatingCategoriesRes ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  RatingCategories ->
  m RatingCategoriesResp
buildGetRatingCategoriesRes bppUrl bppId city messageId listOfCategories = do
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  context <- buildTaxiContext Context.RATING_CATEGORIES messageId (Just messageId) bapIDs.cabs bapURIs.cabs (Just bppId) (Just bppUrl) city
  pure $ BecknReq context listOfCategories
