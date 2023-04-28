module SharedLogic.GetFeedbackForm where

import Beckn.Types.Core.Taxi.API.Rating
import Beckn.Types.Core.Taxi.Rating
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

type FeedbackFormResp = BecknReq FeedbackFormAPIEntity

getFeedbackForm ::
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
  Int ->
  CategoryName ->
  m GetFeedbackFormResp
getFeedbackForm providerUrl bppId city messageId ratingValue categoryName =
  Hedis.safeGet (makeFeedbackFormId bppId ratingValue) >>= \case
    Just a ->
      return a
    Nothing ->
      findAndCache providerUrl bppId city messageId ratingValue categoryName

makeFeedbackFormId :: Text -> Int -> Text
makeFeedbackFormId bppId ratingValue = "RatingCategories:BppId:" <> bppId <> "RatingValue:" <> show ratingValue

cacheFeedbackForm :: (CacheFlow m r) => Text -> FeedbackFormAPIEntity -> m ()
cacheFeedbackForm bppId feedbackForm = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeFeedbackFormId bppId feedbackForm.id) feedbackForm expTime

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
  Int ->
  CategoryName ->
  m GetFeedbackFormResp
findAndCache providerUrl bppId city messageId ratingValue categoryName = do
  req <- buildGetFeedbackFormReq providerUrl bppId city messageId ratingValue categoryName
  feedBackForm <- CallBPP.getFeedbackForm providerUrl req
  cacheFeedbackForm bppId feedBackForm
  return feedBackForm

buildGetFeedbackFormReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Int ->
  CategoryName ->
  m GetFeedbackFormReq
buildGetFeedbackFormReq bppUrl bppId city messageId rating_value rating_category = do
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  context <- buildTaxiContext Context.GET_FEEDBACK_FORM messageId (Just messageId) bapIDs.cabs bapURIs.cabs (Just bppId) (Just bppUrl) city
  pure $ BecknReq context GetFeedbackFormMes {..}

buildGetFeedbackFormRes ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  FeedbackFormAPIEntity ->
  m FeedbackFormResp
buildGetFeedbackFormRes bppUrl bppId city messageId feedbackForm = do
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  context <- buildTaxiContext Context.FEEDBACK_FORM messageId (Just messageId) bapIDs.cabs bapURIs.cabs (Just bppId) (Just bppUrl) city
  pure $ BecknReq context feedbackForm
