module Domain.Action.Dashboard.FeedbackForm
  ( getFeedbackFormList,
    postFeedbackFormCreate,
    putFeedbackFormUpdate,
    deleteFeedbackFormDelete,
    getFeedbackForm,
  )
where

import qualified API.Types.RiderPlatform.Management.FeedbackForm as API
import qualified Data.List as DL
import qualified Data.Text as T
import qualified Domain.Types.FeedbackForm as DTF
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.FeedbackForm as CQFF
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.FeedbackForm as QFF

getFeedbackFormList ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Environment.Flow [API.FeedbackFormRes]
getFeedbackFormList merchantShortId opCity mbLimit mbOffset = do
  (_, merchantOpCityId) <- getMerchantAndCity merchantShortId opCity
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  feedbackForms <- QFF.findAllFeedbackByMerchantOpCityIdWithLimitOffset merchantOpCityId (Just limit) (Just offset)
  pure $ map toAPIFeedbackFormRes feedbackForms

postFeedbackFormCreate ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  API.CreateFeedbackFormReq ->
  Environment.Flow API.CreateFeedbackFormRes
postFeedbackFormCreate merchantShortId opCity req = do
  (merchant, merchantOpCityId) <- getMerchantAndCity merchantShortId opCity
  validateBadges req.badges
  questionKey <- validateQuestionKey req.questionKey
  id <- generateGUID
  ensureQuestionKeyUnique merchantOpCityId req.rating questionKey id
  let feedbackForm =
        DTF.FeedbackForm
          { DTF.id = id,
            DTF.categoryName = toDomainCategory req.categoryName,
            DTF.rating = req.rating,
            DTF.questionKey = Just questionKey,
            DTF.shouldDefault = Just req.shouldDefault,
            DTF.question = req.question,
            DTF.questionTranslations = req.questionTranslations,
            -- Legacy text-answer columns: not read by the app (only `badges` is), kept neutral.
            DTF.answer = [],
            DTF.answerType = DTF.Checkbox,
            DTF.badges = map toDomainBadgeDetail <$> req.badges,
            DTF.merchantId = Just merchant.id,
            DTF.merchantOperatingCityId = Just merchantOpCityId
          }
  QFF.create feedbackForm
  clearFeedbackFormCache merchantOpCityId
  pure $ API.CreateFeedbackFormRes {API.feedbackFormId = id.getId}

putFeedbackFormUpdate ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  API.UpdateFeedbackFormReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
putFeedbackFormUpdate merchantShortId opCity feedbackFormId req = do
  (_, merchantOpCityId) <- getMerchantAndCity merchantShortId opCity
  existing <- findInCity merchantOpCityId feedbackFormId
  validateBadges req.badges
  mbQuestionKey <- mapM validateQuestionKey req.questionKey
  let newRating = req.rating <|> existing.rating
      newQuestionKey = mbQuestionKey <|> existing.questionKey
  -- A key change or a move to another rating can collide with an existing question there.
  whenJust newQuestionKey $ \questionKey ->
    ensureQuestionKeyUnique merchantOpCityId newRating questionKey existing.id
  let updated =
        existing
          { DTF.categoryName = maybe existing.categoryName toDomainCategory req.categoryName,
            DTF.rating = newRating,
            DTF.questionKey = newQuestionKey,
            DTF.shouldDefault = req.shouldDefault <|> existing.shouldDefault,
            DTF.question = fromMaybe existing.question req.question,
            DTF.questionTranslations = req.questionTranslations <|> existing.questionTranslations,
            DTF.badges = (map toDomainBadgeDetail <$> req.badges) <|> existing.badges
          }
  QFF.updateByPrimaryKey updated
  clearFeedbackFormCache merchantOpCityId
  pure Kernel.Types.APISuccess.Success

deleteFeedbackFormDelete ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
deleteFeedbackFormDelete merchantShortId opCity feedbackFormId = do
  (_, merchantOpCityId) <- getMerchantAndCity merchantShortId opCity
  existing <- findInCity merchantOpCityId feedbackFormId
  QFF.deleteById existing.id
  clearFeedbackFormCache merchantOpCityId
  pure Kernel.Types.APISuccess.Success

getFeedbackForm ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Environment.Flow API.FeedbackFormRes
getFeedbackForm merchantShortId opCity feedbackFormId = do
  (_, merchantOpCityId) <- getMerchantAndCity merchantShortId opCity
  toAPIFeedbackFormRes <$> findInCity merchantOpCityId feedbackFormId

-- Helpers

getMerchantAndCity :: ShortId DM.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.Flow (DM.Merchant, Id DMOC.MerchantOperatingCity)
getMerchantAndCity merchantShortId opCity = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId merchant (Just opCity)
  pure (merchant, merchantOpCityId)

-- | A row is only visible to the dashboard of the city it belongs to, so one city's
-- ops can't read, edit or delete another city's questions by id.
findInCity :: Id DMOC.MerchantOperatingCity -> Kernel.Prelude.Text -> Environment.Flow DTF.FeedbackForm
findInCity merchantOpCityId feedbackFormId = do
  feedbackForm <- QFF.findByPrimaryKey (Id feedbackFormId) >>= fromMaybeM (InvalidRequest $ "Feedback form does not exist: " <> feedbackFormId)
  unless (feedbackForm.merchantOperatingCityId == Just merchantOpCityId) $
    throwError (InvalidRequest $ "Feedback form does not exist: " <> feedbackFormId)
  pure feedbackForm

validateQuestionKey :: Kernel.Prelude.Text -> Environment.Flow Kernel.Prelude.Text
validateQuestionKey rawKey = do
  let questionKey = T.strip rawKey
  when (T.null questionKey) $ throwError (InvalidRequest "Question key cannot be empty")
  pure questionKey

-- | The read path treats one row as one question and keeps only the first row per key
-- within a city + rating, so a duplicate key would silently hide a question.
ensureQuestionKeyUnique :: Id DMOC.MerchantOperatingCity -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> Id DTF.FeedbackFormItem -> Environment.Flow ()
ensureQuestionKeyUnique merchantOpCityId rating questionKey selfId = do
  cityRows <- QFF.findAllFeedbackByMerchantOpCityId (Just merchantOpCityId)
  let clash row = row.id /= selfId && row.rating == rating && row.questionKey == Just questionKey
  when (any clash cityRows) $
    throwError (InvalidRequest $ "Question key " <> questionKey <> " already exists for rating " <> maybe "none" show rating)

-- | The FEEDBACK_FORM dynamic logic and the submit path both select badges by key,
-- so keys must be present and unique within a question.
validateBadges :: Maybe [API.BadgeDetail] -> Environment.Flow ()
validateBadges mbBadges = whenJust mbBadges $ \badges -> do
  let badgeKeys = map (.key) badges
  when (any (T.null . T.strip) badgeKeys) $
    throwError (InvalidRequest "Badge key cannot be empty")
  when (length (DL.nub badgeKeys) /= length badgeKeys) $
    throwError (InvalidRequest "Badge keys must be unique within a question")

-- | Clears every cached read a write in this city can affect, on every cloud's Redis.
-- Ratings 1..5 are all cleared because rows with a NULL rating are served under every
-- per-rating key.
clearFeedbackFormCache :: Id DMOC.MerchantOperatingCity -> Environment.Flow ()
clearFeedbackFormCache merchantOpCityId =
  Hedis.runInMultiCloudRedisWrite $ do
    CQFF.clearFeedbackByMerchantOpCityIdCache merchantOpCityId
    forM_ [1 .. 5] $ \rating -> do
      CQFF.clearFeedbackByMerchantOpCityIdAndRatingCache merchantOpCityId rating
      CQFF.clearFeedbackByRatingCache rating

-- Conversions between the dashboard API types and the domain types

toAPIFeedbackFormRes :: DTF.FeedbackForm -> API.FeedbackFormRes
toAPIFeedbackFormRes feedbackForm =
  API.FeedbackFormRes
    { API.id = feedbackForm.id.getId,
      API.categoryName = toAPICategory feedbackForm.categoryName,
      API.rating = feedbackForm.rating,
      API.questionKey = feedbackForm.questionKey,
      -- A missing flag means hidden, same as the read path.
      API.shouldDefault = fromMaybe False feedbackForm.shouldDefault,
      API.question = feedbackForm.question,
      API.questionTranslations = feedbackForm.questionTranslations,
      API.badges = map toAPIBadgeDetail <$> feedbackForm.badges,
      API.merchantOperatingCityId = cast <$> feedbackForm.merchantOperatingCityId,
      API.merchantId = cast <$> feedbackForm.merchantId
    }

toDomainCategory :: API.Category -> DTF.Category
toDomainCategory = \case
  API.RIDE -> DTF.RIDE
  API.DRIVER -> DTF.DRIVER
  API.VEHICLE -> DTF.VEHICLE

toAPICategory :: DTF.Category -> API.Category
toAPICategory = \case
  DTF.RIDE -> API.RIDE
  DTF.DRIVER -> API.DRIVER
  DTF.VEHICLE -> API.VEHICLE

toDomainBadgeDetail :: API.BadgeDetail -> DTF.BadgeDetail
toDomainBadgeDetail API.BadgeDetail {..} =
  DTF.BadgeDetail
    { DTF.key = key,
      DTF.sendPN = sendPN,
      DTF.priority = priority,
      DTF.contentWithTranslations = contentWithTranslations,
      DTF.shouldDefault = Just shouldDefault
    }

toAPIBadgeDetail :: DTF.BadgeDetail -> API.BadgeDetail
toAPIBadgeDetail DTF.BadgeDetail {..} =
  API.BadgeDetail
    { API.key = key,
      API.sendPN = sendPN,
      API.priority = priority,
      API.contentWithTranslations = contentWithTranslations,
      API.shouldDefault = fromMaybe False shouldDefault
    }
