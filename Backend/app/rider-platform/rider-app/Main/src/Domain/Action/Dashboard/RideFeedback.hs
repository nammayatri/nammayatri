module Domain.Action.Dashboard.RideFeedback
  ( getRideFeedbackConfigList,
    getRideFeedbackConfig,
    postRideFeedbackConfigCreate,
    postRideFeedbackConfigUpdate,
    postRideFeedbackConfigToggle,
    postRideFeedbackConfigClone,
    postRideFeedbackConfigValidate,
    getRideFeedbackRidePreview,
    getRideFeedbackRideResponses,
    postRideFeedbackResponseRetryActions,
    getRideFeedbackMeta,
  )
where

import qualified API.Types.RiderPlatform.Management.RideFeedback as Common
import qualified Dashboard.Common as DCommon
import qualified Data.Aeson as A
import Data.Default.Class (def)
import Data.Either (lefts, rights)
import Data.List (sortOn)
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideFeedbackConfig as DRFC
import qualified Domain.Types.RideFeedbackResponse as DRFR
import qualified Domain.Types.RideStatus as DRS
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.RideFeedback.Actions (ActionEnv (..), retryFailedActions)
import SharedLogic.RideFeedback.Context (RideFeedbackContext)
import SharedLogic.RideFeedback.Eligibility
import SharedLogic.RideFeedback.Rule (supportedOperatorNames)
import SharedLogic.RideFeedback.Validation (reportableIssueTypes, validateConfig)
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.RideFeedbackConfig as CQRFC
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideFeedbackConfig as QRFC
import qualified Storage.Queries.RideFeedbackResponse as QRFR
import Tools.Error

-------------------------------------------------------------------------------
-- Questions (ride_feedback_config)

getRideFeedbackConfigList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Int ->
  Maybe Int ->
  Flow Common.RideFeedbackConfigListRes
getRideFeedbackConfigList merchantShortId opCity mbQuestionKey mbEnabled mbLimit mbOffset = do
  (_, moc) <- resolveCity merchantShortId opCity
  configs <- QRFC.findAllByMerchantOperatingCityId moc.id
  let search = T.toUpper . T.strip <$> mbQuestionKey
      filtered =
        sortOn
          priorityKey
          [ c
            | c <- configs,
              maybe True (`T.isInfixOf` c.questionKey) search,
              maybe True (== c.enabled) mbEnabled
          ]
      limit = max 1 . min maxListLimit $ fromMaybe defaultListLimit mbLimit
      offset = max 0 $ fromMaybe 0 mbOffset
      page = take limit (drop offset filtered)
  pure
    Common.RideFeedbackConfigListRes
      { totalItems = length filtered,
        summary = DCommon.Summary {totalCount = length filtered, count = length page},
        configs = map toItem page
      }

getRideFeedbackConfig :: ShortId DM.Merchant -> Context.City -> Id DRFC.RideFeedbackConfig -> Flow Common.RideFeedbackConfigItem
getRideFeedbackConfig merchantShortId opCity configId = do
  (_, moc) <- resolveCity merchantShortId opCity
  toItem <$> fetchCityConfig moc configId

postRideFeedbackConfigCreate :: ShortId DM.Merchant -> Context.City -> Common.CreateRideFeedbackConfigReq -> Flow Common.RideFeedbackConfigUpsertRes
postRideFeedbackConfigCreate merchantShortId opCity req = do
  (merchant, moc) <- resolveCity merchantShortId opCity
  now <- getCurrentTime
  configId <- generateGUID
  let config = fromCreateReq merchant.id moc.id configId now req
  cityConfigs <- QRFC.findAllByMerchantOperatingCityId moc.id
  when (any ((== config.questionKey) . (.questionKey)) cityConfigs) $ throwError (RideFeedbackConfigAlreadyExists config.questionKey)
  failIfInvalid (validateConfig cityConfigs config)
  QRFC.create config
  CQRFC.clearEnabledByMerchantOpCityIdCache moc.id
  pure Common.RideFeedbackConfigUpsertRes {id = config.id, version = config.version}

postRideFeedbackConfigUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DRFC.RideFeedbackConfig ->
  Common.UpdateRideFeedbackConfigReq ->
  Flow Common.RideFeedbackConfigUpsertRes
postRideFeedbackConfigUpdate merchantShortId opCity configId req = do
  (_, moc) <- resolveCity merchantShortId opCity
  config <- fetchCityConfig moc configId
  now <- getCurrentTime
  -- Full replace of the question's content: every optional field takes the request's value, so null clears it.
  -- Only the two required fields fall back to the stored value when omitted.
  let updated =
        (config :: DRFC.RideFeedbackConfig)
          { DRFC.questionType = fromMaybe config.questionType req.questionType,
            DRFC.title = fromMaybe config.title req.title,
            DRFC.description = req.description,
            DRFC.options = req.options,
            DRFC.inputConfig = req.inputConfig,
            DRFC.acknowledgement = req.acknowledgement,
            DRFC.uiConfig = req.uiConfig,
            DRFC.isSkippable = req.isSkippable,
            DRFC.isFollowUpOnly = req.isFollowUpOnly,
            DRFC.allowedRideStatuses = req.allowedRideStatuses,
            DRFC.displayTrigger = req.displayTrigger,
            DRFC.actionRules = req.actionRules,
            DRFC.priority = req.priority,
            DRFC.maxShowsPerRide = req.maxShowsPerRide,
            DRFC.cooldownDays = req.cooldownDays,
            DRFC.startsAt = req.startsAt,
            DRFC.endsAt = req.endsAt,
            DRFC.version = config.version + 1,
            DRFC.updatedAt = now
          }
  cityConfigs <- QRFC.findAllByMerchantOperatingCityId moc.id
  failIfInvalid (validateConfig cityConfigs updated)
  QRFC.updateByPrimaryKey updated
  CQRFC.clearEnabledByMerchantOpCityIdCache moc.id
  pure Common.RideFeedbackConfigUpsertRes {id = updated.id, version = updated.version}

postRideFeedbackConfigToggle ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DRFC.RideFeedbackConfig ->
  Common.ToggleRideFeedbackConfigReq ->
  Flow APISuccess
postRideFeedbackConfigToggle merchantShortId opCity configId req = do
  (_, moc) <- resolveCity merchantShortId opCity
  config <- fetchCityConfig moc configId
  -- Re-check before switching on: a question it opens may have been removed since it was saved.
  when req.enabled $ do
    cityConfigs <- QRFC.findAllByMerchantOperatingCityId moc.id
    failIfInvalid (validateConfig cityConfigs config)
  now <- getCurrentTime
  QRFC.updateByPrimaryKey (config :: DRFC.RideFeedbackConfig) {DRFC.enabled = req.enabled, DRFC.updatedAt = now}
  CQRFC.clearEnabledByMerchantOpCityIdCache moc.id
  pure Success

-- | Copies a question to other cities of the same merchant. Copies start disabled unless 'enabled' is set.
-- A city is skipped when it does not exist for the merchant, already has the key, or the copy is invalid
-- there (e.g. an option opens a follow-up that the city does not have yet: clone the follow-up first).
postRideFeedbackConfigClone ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DRFC.RideFeedbackConfig ->
  Common.CloneRideFeedbackConfigReq ->
  Flow Common.CloneRideFeedbackConfigRes
postRideFeedbackConfigClone merchantShortId opCity configId req = do
  (merchant, moc) <- resolveCity merchantShortId opCity
  source <- fetchCityConfig moc configId
  now <- getCurrentTime
  results <- forM req.targetCities $ \city ->
    CQMOC.findByMerchantIdAndCity merchant.id city >>= \case
      Nothing -> pure $ Left Common.SkippedClone {city, reason = "CITY_NOT_FOUND"}
      Just targetMoc -> do
        targetConfigs <- QRFC.findAllByMerchantOperatingCityId targetMoc.id
        newId <- generateGUID
        let copy =
              (source :: DRFC.RideFeedbackConfig)
                { DRFC.id = newId,
                  DRFC.merchantOperatingCityId = targetMoc.id,
                  DRFC.version = 1,
                  DRFC.enabled = fromMaybe False req.enabled,
                  DRFC.createdAt = now,
                  DRFC.updatedAt = now
                }
            errors = validateConfig targetConfigs copy
        if
            | any ((== source.questionKey) . (.questionKey)) targetConfigs -> pure $ Left Common.SkippedClone {city, reason = "ALREADY_EXISTS"}
            | not (null errors) -> pure $ Left Common.SkippedClone {city, reason = "INVALID: " <> T.intercalate "; " errors}
            | otherwise -> do
              QRFC.create copy
              CQRFC.clearEnabledByMerchantOpCityIdCache targetMoc.id
              pure $ Right Common.ClonedConfig {city, configId = copy.id}
  pure Common.CloneRideFeedbackConfigRes {created = rights results, skipped = lefts results}

-- | Dry run of create: every problem at once, nothing saved.
postRideFeedbackConfigValidate :: ShortId DM.Merchant -> Context.City -> Common.CreateRideFeedbackConfigReq -> Flow Common.ValidateRideFeedbackConfigRes
postRideFeedbackConfigValidate merchantShortId opCity req = do
  (merchant, moc) <- resolveCity merchantShortId opCity
  now <- getCurrentTime
  let draft = fromCreateReq merchant.id moc.id (Id "draft") now req
  cityConfigs <- QRFC.findAllByMerchantOperatingCityId moc.id
  let errors =
        ["questionKey " <> draft.questionKey <> " already exists in this city" | any ((== draft.questionKey) . (.questionKey)) cityConfigs]
          <> validateConfig cityConfigs draft
  pure Common.ValidateRideFeedbackConfigRes {isValid = null errors, errors}

-------------------------------------------------------------------------------
-- Rides

-- | What the rider of this ride would get right now, and why each question of the city is in or out.
-- Read-only: runs the same pass as the rider API (dynamic logic + per-question limits) without caching.
getRideFeedbackRidePreview :: ShortId DM.Merchant -> Context.City -> Id DCommon.Ride -> Flow Common.RideFeedbackPreviewRes
getRideFeedbackRidePreview merchantShortId opCity dashboardRideId = do
  let rideId = cast dashboardRideId :: Id DRide.Ride
  (_, moc) <- resolveCity merchantShortId opCity
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  unless (booking.merchantOperatingCityId == moc.id) $ throwError (InvalidRequest "Ride does not belong to this merchant city")
  person <- QPerson.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  configs <- QRFC.findAllByMerchantOperatingCityId moc.id
  now <- getCurrentTime
  evaluation <- evaluateRide now person ride booking (filter (.enabled) configs)
  let followUpIds = map (.id) evaluation.followUps
      evaluate c
        | not c.enabled = notEligible c "DISABLED"
        | isFollowUpOnly c =
          if c.id `elem` followUpIds
            then Common.QuestionEvaluation {configId = c.id, questionKey = c.questionKey, eligible = True, isFollowUp = True, reason = Nothing, showAt = Nothing}
            else notEligible c "FOLLOW_UP_NOT_REACHED"
        | otherwise = case snd <$> find ((== c.id) . (.id) . fst) evaluation.decisions of
          Just (Eligible t) -> Common.QuestionEvaluation {configId = c.id, questionKey = c.questionKey, eligible = True, isFollowUp = False, reason = Nothing, showAt = Just t}
          Just (Pending r) -> notEligible c r
          Just (Excluded r) -> notEligible c r
          Nothing -> notEligible c "NOT_SELECTED_BY_LOGIC"
      notEligible c r = Common.QuestionEvaluation {configId = c.id, questionKey = c.questionKey, eligible = False, isFollowUp = isFollowUpOnly c, reason = Just r, showAt = Nothing}
  pure
    Common.RideFeedbackPreviewRes
      { rideId = dashboardRideId,
        rideStatus = ride.status,
        logicVersion = evaluation.logicVersion,
        selectedQuestionKeys = evaluation.selectedKeys,
        context = A.toJSON evaluation.context,
        evaluations = map evaluate (sortOn priorityKey configs)
      }

getRideFeedbackRideResponses :: ShortId DM.Merchant -> Context.City -> Id DCommon.Ride -> Flow [Common.RideFeedbackResponseItem]
getRideFeedbackRideResponses merchantShortId opCity dashboardRideId = do
  (_, moc) <- resolveCity merchantShortId opCity
  responses <- filter ((== moc.id) . (.merchantOperatingCityId)) <$> QRFR.findAllByRideId (cast dashboardRideId)
  pure $ map toResponseItem (sortOn (.createdAt) responses)

-- | Re-runs the failed or pending actions of an answer (e.g. after the BPP was down). Successful ones are kept.
postRideFeedbackResponseRetryActions :: ShortId DM.Merchant -> Context.City -> Id DRFR.RideFeedbackResponse -> Flow APISuccess
postRideFeedbackResponseRetryActions merchantShortId opCity responseId = do
  (merchant, moc) <- resolveCity merchantShortId opCity
  response <- QRFR.findByPrimaryKey responseId >>= fromMaybeM (RideFeedbackResponseNotFound responseId.getId)
  unless (response.merchantOperatingCityId == moc.id) $ throwError (RideFeedbackResponseNotFound responseId.getId)
  answer <- case (response.status, response.answer) of
    (DRFR.ANSWERED, Just a) -> pure a
    _ -> throwError (InvalidRequest "Only answered responses have actions")
  config <- QRFC.findById response.configId >>= fromMaybeM (RideFeedbackConfigNotFound response.configId.getId)
  ride <- QRide.findById response.rideId >>= fromMaybeM (RideDoesNotExist response.rideId.getId)
  booking <- QB.findById response.bookingId >>= fromMaybeM (BookingNotFound response.bookingId.getId)
  person <- QPerson.findById response.personId >>= fromMaybeM (PersonNotFound response.personId.getId)
  riderConfig <- fetchRiderConfig booking
  scheduled <- retryFailedActions ActionEnv {merchant, riderConfig, person, ride, booking, config, response, answer}
  when (scheduled == 0) $ throwError (InvalidRequest "No failed or pending actions to retry")
  pure Success

-------------------------------------------------------------------------------
-- Dashboard form metadata

getRideFeedbackMeta :: ShortId DM.Merchant -> Context.City -> Flow Common.RideFeedbackMetaRes
getRideFeedbackMeta merchantShortId opCity = do
  void $ resolveCity merchantShortId opCity
  pure
    Common.RideFeedbackMetaRes
      { questionTypes = [minBound .. maxBound],
        actionTypes = [minBound .. maxBound],
        deliveryModes = [minBound .. maxBound],
        allowedRideStatuses = [DRS.NEW, DRS.INPROGRESS],
        issueReportTypes = reportableIssueTypes,
        languages = [minBound .. maxBound],
        uiLayouts = ["inline_card", "bottom_sheet", "chips", "poll"],
        supportedOperators = supportedOperatorNames,
        contextSample = A.toJSON (def :: RideFeedbackContext)
      }

-------------------------------------------------------------------------------
-- Helpers

resolveCity :: ShortId DM.Merchant -> Context.City -> Flow (DM.Merchant, DMOC.MerchantOperatingCity)
resolveCity merchantShortId opCity = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  moc <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  pure (merchant, moc)

fetchCityConfig :: DMOC.MerchantOperatingCity -> Id DRFC.RideFeedbackConfig -> Flow DRFC.RideFeedbackConfig
fetchCityConfig moc configId = do
  config <- QRFC.findById configId >>= fromMaybeM (RideFeedbackConfigNotFound configId.getId)
  unless (config.merchantOperatingCityId == moc.id) $ throwError (RideFeedbackConfigNotFound configId.getId)
  pure config

failIfInvalid :: [Text] -> Flow ()
failIfInvalid errors = unless (null errors) $ throwError (RideFeedbackInvalidConfig errors)

fromCreateReq :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Id DRFC.RideFeedbackConfig -> UTCTime -> Common.CreateRideFeedbackConfigReq -> DRFC.RideFeedbackConfig
fromCreateReq merchantId merchantOperatingCityId configId now req =
  DRFC.RideFeedbackConfig
    { id = configId,
      merchantId,
      merchantOperatingCityId,
      questionKey = T.strip req.questionKey,
      version = 1,
      questionType = req.questionType,
      title = req.title,
      description = req.description,
      options = req.options,
      inputConfig = req.inputConfig,
      acknowledgement = req.acknowledgement,
      uiConfig = req.uiConfig,
      isSkippable = req.isSkippable,
      isFollowUpOnly = req.isFollowUpOnly,
      allowedRideStatuses = req.allowedRideStatuses,
      displayTrigger = req.displayTrigger,
      actionRules = req.actionRules,
      priority = req.priority,
      maxShowsPerRide = req.maxShowsPerRide,
      cooldownDays = req.cooldownDays,
      startsAt = req.startsAt,
      endsAt = req.endsAt,
      enabled = fromMaybe False req.enabled,
      createdBy = Nothing,
      updatedBy = Nothing,
      createdAt = now,
      updatedAt = now
    }

toItem :: DRFC.RideFeedbackConfig -> Common.RideFeedbackConfigItem
toItem c =
  Common.RideFeedbackConfigItem
    { id = c.id,
      questionKey = c.questionKey,
      version = c.version,
      questionType = c.questionType,
      title = c.title,
      description = c.description,
      options = c.options,
      inputConfig = c.inputConfig,
      acknowledgement = c.acknowledgement,
      uiConfig = c.uiConfig,
      isSkippable = c.isSkippable,
      isFollowUpOnly = c.isFollowUpOnly,
      allowedRideStatuses = c.allowedRideStatuses,
      displayTrigger = c.displayTrigger,
      actionRules = c.actionRules,
      priority = c.priority,
      maxShowsPerRide = c.maxShowsPerRide,
      cooldownDays = c.cooldownDays,
      startsAt = c.startsAt,
      endsAt = c.endsAt,
      enabled = c.enabled,
      createdAt = c.createdAt,
      updatedAt = c.updatedAt
    }

toResponseItem :: DRFR.RideFeedbackResponse -> Common.RideFeedbackResponseItem
toResponseItem r =
  Common.RideFeedbackResponseItem
    { id = r.id,
      configId = r.configId,
      questionKey = r.questionKey,
      configVersion = r.configVersion,
      logicVersion = r.logicVersion,
      status = r.status,
      answer = r.answer,
      parentResponseId = r.parentResponseId,
      rideStatusAtResponse = r.rideStatusAtResponse,
      secondsIntoRide = r.secondsIntoRide,
      actionResults = r.actionResults,
      createdAt = r.createdAt,
      updatedAt = r.updatedAt
    }

defaultListLimit :: Int
defaultListLimit = 50

maxListLimit :: Int
maxListLimit = 200
