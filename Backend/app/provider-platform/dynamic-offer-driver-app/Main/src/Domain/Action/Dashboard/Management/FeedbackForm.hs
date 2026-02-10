{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Management.FeedbackForm
  ( postFeedbackFormCreate,
    putFeedbackFormUpdate,
    deleteFeedbackFormDelete,
    getFeedbackForm,
  )
where

import qualified API.Types.ProviderPlatform.Management.FeedbackForm as API
import qualified Domain.Types.FeedbackForm as DTF
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Id
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FeedbackForm as QFF
import qualified Storage.Queries.FeedbackFormExtra as QFFE
import Tools.Error

postFeedbackFormCreate ::
  Kernel.Types.Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  API.CreateFeedbackFormReq ->
  Environment.Flow API.CreateFeedbackFormRes
postFeedbackFormCreate _merchantShortId _opCity req = do
  id <- Id <$> L.generateGUID
  let feedbackForm =
        DTF.FeedbackForm
          { DTF.id = id,
            DTF.categoryName = toDomainCategory req.categoryName,
            DTF.rating = req.rating,
            DTF.question = req.question,
            DTF.questionTranslations = req.questionTranslations,
            DTF.answer = req.answer,
            DTF.answerType = toDomainAnswerType req.answerType,
            DTF.badges = fmap (map toDomainBadgeDetail) req.badges,
            DTF.merchantId = fmap (Kernel.Types.Id.Id . Kernel.Types.Id.getId) req.merchantId,
            DTF.merchantOperatingCityId = fmap (Kernel.Types.Id.Id . Kernel.Types.Id.getId) req.merchantOperatingCityId
          }
  QFF.create feedbackForm
  pure $ API.CreateFeedbackFormRes {API.feedbackFormId = Kernel.Types.Id.getId id}

putFeedbackFormUpdate ::
  Kernel.Types.Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  API.UpdateFeedbackFormReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
putFeedbackFormUpdate _merchantShortId _opCity feedbackFormId req = do
  let id = Kernel.Types.Id.Id feedbackFormId
  existing <- QFF.findByPrimaryKey id >>= fromMaybeM (InvalidRequest "Feedback form does not exist")
  let updated =
        existing
          { DTF.categoryName = fromMaybe existing.categoryName (toDomainCategory <$> req.categoryName),
            DTF.rating = req.rating <|> existing.rating,
            DTF.question = fromMaybe existing.question req.question,
            DTF.questionTranslations = req.questionTranslations <|> existing.questionTranslations,
            DTF.answer = fromMaybe existing.answer req.answer,
            DTF.answerType = fromMaybe existing.answerType (toDomainAnswerType <$> req.answerType),
            DTF.badges = (fmap (map toDomainBadgeDetail) req.badges) <|> existing.badges,
            DTF.merchantId = fmap (Kernel.Types.Id.Id . Kernel.Types.Id.getId) req.merchantId <|> existing.merchantId,
            DTF.merchantOperatingCityId = fmap (Kernel.Types.Id.Id . Kernel.Types.Id.getId) req.merchantOperatingCityId <|> existing.merchantOperatingCityId
          }
  QFF.updateByPrimaryKey updated
  pure Kernel.Types.APISuccess.Success

deleteFeedbackFormDelete ::
  Kernel.Types.Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
deleteFeedbackFormDelete _merchantShortId _opCity feedbackFormId = do
  let id = Kernel.Types.Id.Id feedbackFormId
  void $ QFF.findByPrimaryKey id >>= fromMaybeM (InvalidRequest "Feedback form does not exist")
  QFFE.deleteById id
  pure Kernel.Types.APISuccess.Success

getFeedbackForm ::
  Kernel.Types.Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Environment.Flow API.FeedbackFormRes
getFeedbackForm _merchantShortId _opCity feedbackFormId = do
  let id = Kernel.Types.Id.Id feedbackFormId
  feedbackForm <- QFF.findByPrimaryKey id >>= fromMaybeM (InvalidRequest "Feedback form does not exist")
  pure $
    API.FeedbackFormRes
      { API.id = Kernel.Types.Id.getId feedbackForm.id,
        API.categoryName = toAPICategory feedbackForm.categoryName,
        API.rating = feedbackForm.rating,
        API.question = feedbackForm.question,
        API.questionTranslations = feedbackForm.questionTranslations,
        API.answer = feedbackForm.answer,
        API.answerType = toAPIAnswerType feedbackForm.answerType,
        API.badges = fmap (map toAPIBadgeDetail) feedbackForm.badges,
        API.merchantOperatingCityId = fmap (Kernel.Types.Id.Id . Kernel.Types.Id.getId) feedbackForm.merchantOperatingCityId,
        API.merchantId = fmap (Kernel.Types.Id.Id . Kernel.Types.Id.getId) feedbackForm.merchantId
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

toDomainAnswerType :: API.AnswerType -> DTF.AnswerType
toDomainAnswerType = \case
  API.Text -> DTF.Text
  API.Checkbox -> DTF.Checkbox
  API.Radio -> DTF.Radio

toAPIAnswerType :: DTF.AnswerType -> API.AnswerType
toAPIAnswerType = \case
  DTF.Text -> API.Text
  DTF.Checkbox -> API.Checkbox
  DTF.Radio -> API.Radio

toDomainBadgeDetail :: API.BadgeDetail -> DTF.BadgeDetail
toDomainBadgeDetail API.BadgeDetail {..} =
  DTF.BadgeDetail
    { DTF.key = key,
      DTF.sendPN = sendPN,
      DTF.priority = priority,
      DTF.contentWithTranslations = contentWithTranslations
    }

toAPIBadgeDetail :: DTF.BadgeDetail -> API.BadgeDetail
toAPIBadgeDetail DTF.BadgeDetail {..} =
  API.BadgeDetail
    { API.key = key,
      API.sendPN = sendPN,
      API.priority = priority,
      API.contentWithTranslations = contentWithTranslations
    }
