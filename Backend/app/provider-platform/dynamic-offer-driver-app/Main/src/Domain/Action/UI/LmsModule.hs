module Domain.Action.UI.LmsModule where

import API.Types.UI.LmsModule
import qualified Domain.Types.DocumentReminderHistory as DRH
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.DriverModuleCompletion as DTDMC
import qualified Domain.Types.LmsCertificate as DTLC
import Domain.Types.LmsModule
import Domain.Types.LmsModule as LmsModule
import Domain.Types.LmsModuleVideoInformation as LmsModuleVideoInformation
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.ModuleCompletionInformation as DTMCI
import qualified Domain.Types.Person
import qualified Domain.Types.QuestionInformation as DTQI
import qualified Domain.Types.QuestionModuleMapping as DTQMM
import qualified Domain.Types.ReelsData as DTRD
import qualified Domain.Types.VehicleVariant
import qualified Domain.Types.VehicleVariant as DTVeh
import qualified Environment
import EulerHS.Prelude hiding (id, length)
import Kernel.Beam.Functions
import Kernel.External.Types (Language (..), SchedulerFlow, ServiceFlow)
import Kernel.Prelude hiding (all, elem, find, foldl', map, notElem, null, whenJust)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Coins as DC
import Lib.DriverCoins.Types as DCT
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.Reminder.Helper (cancelRemindersForDriverByDocumentType, recordDocumentCompletion)
import qualified Storage.CachedQueries.CoinsConfig as CDCQ
import qualified Storage.CachedQueries.Lms as SCQL
import Storage.CachedQueries.Merchant.MerchantOperatingCity as SCQMM
import qualified Storage.Cac.TransporterConfig as CCT
import qualified Storage.Queries.Coins.CoinHistory as SQCC
import qualified Storage.Queries.DocumentReminderHistory as QDRH
import qualified Storage.Queries.DriverInformationExtra as QDIExtra
import qualified Storage.Queries.DriverModuleCompletion as SQDMC
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.LmsCertificate as SQLC
import Storage.Queries.LmsVideoTranslation as SQLVT
import Storage.Queries.ModuleCompletionInformation as SQMCI
import qualified Storage.Queries.Person as QPerson
import Tools.Error

-- types of coin event and its corresponding function
-- LMS - QuizQuestionCompleted
-- LMSBonus - BonusQuizCoins

getLmsListAllModules :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id DM.Merchant, Kernel.Types.Id.Id DMOC.MerchantOperatingCity) -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Domain.Types.LmsModule.ModuleSection) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Domain.Types.VehicleVariant.VehicleVariant) -> Environment.Flow API.Types.UI.LmsModule.LmsGetModuleRes
getLmsListAllModules (mbPersonId, _merchantId, merchantOpCityId) mbLanguage _mbLimit mbModuleSection _mbOffset _mbVariant = do
  personId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  driver <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  let language = fromMaybe ENGLISH mbLanguage

  let moduleSection = Just $ fromMaybe BENEFITS mbModuleSection
  modules <- mapM (generateModuleInfo language driver.id) =<< SCQL.getAllModulesWithModuleSection Nothing Nothing merchantOpCityId moduleSection -- todo :: mbLimit and mbOffset ( phase 2 of LMS)
  return $
    API.Types.UI.LmsModule.LmsGetModuleRes
      { completed = sortOn (.completedAt) $ filter ((== DTDMC.MODULE_COMPLETED) . (.moduleCompletionStatus)) modules,
        remaining = sortOn (.rank) $ filter ((/= DTDMC.MODULE_COMPLETED) . (.moduleCompletionStatus)) modules
      }
  where
    fetchQuestionCoins vehCategory question = do
      case question.quizCoinFunction of
        Nothing -> return 0
        Just functionName -> do
          coinsConfig <- CDCQ.fetchConfigOnEventAndFunctionBasis DCT.LMS functionName _merchantId merchantOpCityId vehCategory Nothing
          return $ maybe 0 (\cc -> cc.coins) coinsConfig

    generateModuleInfo language personId eModule@LmsModule {..} = do
      -- fetching bonus coins for current module
      let vehCategory = DTVeh.getVehicleCategoryFromVehicleVariantDefault Nothing
      bonusCoins <- case bonusCoinEventFunction of
        Nothing -> pure $ Nothing
        Just functionName -> do
          coinsConfig <- CDCQ.fetchConfigOnEventAndFunctionBasis DCT.LMSBonus functionName _merchantId merchantOpCityId vehCategory Nothing
          return $ maybe Nothing (\cc -> Just cc.coins) coinsConfig

      -- fetching total coins for quiz
      questions <- SCQL.getAllQuestions eModule.id
      totalQuizCoinsVal <- Kernel.Prelude.sum <$> forM questions (fetchQuestionCoins vehCategory)
      let totalQuizCoins = if (totalQuizCoinsVal == 0) then Nothing else Just totalQuizCoinsVal

      -- getting all translation for current moduleId
      translations <- SCQL.getAllModuleTranslations eModule.id
      translation <-
        ( \case
            Nothing -> fromMaybeM (LmsModuleTranslationNotFound eModule.id.getId language) $ find ((== ENGLISH) . (.language)) translations
            Just translation -> return translation
          )
          $ find ((== language) . (.language)) translations

      -- finding valid DriverModuleCompletion entry
      now <- getCurrentTime
      allDriverModuleCompletion <- SQDMC.findAllByDriverIdAndModuleId personId eModule.id
      let mbDriverModuleCompletion = find (\driverModuleComp -> maybe True (now >) driverModuleComp.expiry) allDriverModuleCompletion

      -- finding total videos watched for a given module
      totalVideosWatched <- maybe (pure []) (SQMCI.findAllByCompletionIdAndEntityAndStatus DTMCI.VIDEO DTMCI.ENTITY_PASSED . (.completionId)) mbDriverModuleCompletion

      -- fetching total bonus earned val
      bonusEarnedVal <- do
        if (length allDriverModuleCompletion == 1)
          then do
            case bonusCoinEventFunction of
              Nothing -> return Nothing
              Just functionName -> do
                mbCoinHistory <- SQCC.getCoinsByEventFunction personId functionName (Just eModule.id.getId)
                return $ maybe Nothing (\val -> Just val.coins) mbCoinHistory
          else do return Nothing

      let moduleAlreadyCompleted = Just $ (length allDriverModuleCompletion) > 1
      return $
        API.Types.UI.LmsModule.LmsModuleRes
          { moduleId = eModule.id,
            name = (.name) translation,
            description = (.description) translation,
            thumbnailImage = (.thumbnailImage) translation,
            moduleCompletionStatus = maybe DTDMC.MODULE_NOT_YET_STARTED (.status) mbDriverModuleCompletion,
            completedAt = maybe (Just now) (.completedAt) mbDriverModuleCompletion,
            noOfVideosCompleted = length totalVideosWatched,
            ..
          }

getLmsListAllVideos :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id DM.Merchant, Kernel.Types.Id.Id DMOC.MerchantOperatingCity) -> Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> Environment.Flow API.Types.UI.LmsModule.LmsGetVideosRes
getLmsListAllVideos (mbPersonId, _merchantId, merchantOpCityId) modId mbLanguage = do
  personId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  let language = fromMaybe ENGLISH mbLanguage
  merchantOperatingCity <- SCQMM.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityDoesNotExist merchantOpCityId.getId)
  moduleInfo <- fromMaybeM (LmsModuleNotFound modId.getId) . find ((== modId) . (.id)) =<< SCQL.getAllModules Nothing Nothing merchantOpCityId
  mbDriverModuleCompletion <- getmbDriverModuleCompletion personId modId
  videos <- mapM (generateVideoRes merchantOperatingCity language mbDriverModuleCompletion) =<< SCQL.getAllVideos modId [ACTIVE]
  selectedModuleInfo <- generateModuleInfoRes moduleInfo language
  return $
    API.Types.UI.LmsModule.LmsGetVideosRes
      { quizEnabled = case moduleInfo.moduleCompletionCriteria of
          LmsModule.VIDEOS_AND_QUIZ _ -> True
          _ -> False,
        quizStatus = maybe API.Types.UI.LmsModule.ENTITY_INCOMPLETE (\modCompInfo -> if DTDMC.QUIZ `elem` modCompInfo.entitiesCompleted then API.Types.UI.LmsModule.ENTITY_COMPLETED else API.Types.UI.LmsModule.ENTITY_INCOMPLETE) mbDriverModuleCompletion,
        completed = sortOn (.completedAt) $ filter ((== API.Types.UI.LmsModule.ENTITY_COMPLETED) . (.videoCompletionStatus)) videos,
        pending = sortOn (.rank) $ filter ((== API.Types.UI.LmsModule.ENTITY_INCOMPLETE) . (.videoCompletionStatus)) videos,
        selectedModuleInfo = selectedModuleInfo
      }
  where
    generateVideoRes merchantOperatingCity language mbDriverModuleCompletion video@LmsModuleVideoInformation {..} = do
      translations <- SCQL.getAllTranslationsForVideoId video.id
      translation <-
        ( \case
            Nothing ->
              ( \case
                  Nothing -> fromMaybeM (LmsVideoTranslationNotFound video.id.getId language) $ find ((== ENGLISH) . (.language)) translations
                  Just translation -> return translation
              )
                $ find ((== merchantOperatingCity.language) . (.language)) translations
            Just translation -> return translation
          )
          $ find ((== language) . (.language)) translations
      videoUrl <- getVideoUrlAccordingToConditionSet merchantOperatingCity translations translation
      now <- getCurrentTime
      mbVideoCompletionInfo <- listToMaybe <$> maybe (pure []) (SQMCI.findByCompletionIdAndEntityAndEntityId (Just 1) Nothing DTMCI.VIDEO video.id.getId . (.completionId)) mbDriverModuleCompletion
      return $
        API.Types.UI.LmsModule.LmsVideoRes
          { videoId = video.id,
            url = videoUrl,
            ytVideoId = (.ytVideoId) translation,
            duration = (.duration) translation,
            completedWatchCount = (.completedWatchCount) translation,
            viewCount = (.viewCount) translation,
            thumbnailImage = (.thumbnailImage) translation,
            title = (.title) translation,
            description = (.description) translation,
            thresholdEnabled = (.thresholdEnabled) translation,
            startThresholdInPercentage = (.startThresholdInPercentage) translation,
            completedThresholdInPercentage = (.completedThresholdInPercentage) translation,
            videoCompletionStatus = maybe API.Types.UI.LmsModule.ENTITY_INCOMPLETE (\vidComInfo -> if vidComInfo.entityStatus == DTMCI.ENTITY_PASSED then API.Types.UI.LmsModule.ENTITY_COMPLETED else API.Types.UI.LmsModule.ENTITY_INCOMPLETE) mbVideoCompletionInfo,
            attemptNumber = maybe 0 (.attempt) mbVideoCompletionInfo,
            completedAt = maybe now (.createdAt) mbVideoCompletionInfo,
            rank = video.rank,
            sideButtonConfig = generateButtonConfigData translation.sideButtonConfig,
            bottomButtonConfig = generateButtonConfigData translation.bottomButtonConfig,
            ..
          }

    getVideoUrlAccordingToConditionSet merchantOperatingCity translations videoTranslation =
      if videoTranslation.useMerchantOperatingCityDefaultLanguageVideoUrl
        then
          (.url)
            <$> ( \case
                    Nothing -> fromMaybeM (LmsVideoTranslationNotFound videoTranslation.videoId.getId ENGLISH) $ find ((== ENGLISH) . (.language)) translations
                    Just translation -> return translation
                )
              (find ((== merchantOperatingCity.language) . (.language)) translations)
        else return $ videoTranslation.url

    generateButtonConfigData :: [DTRD.ReelRowButtonConfig] -> [[DTRD.ReelButtonConfig]]
    generateButtonConfigData = foldl' (\acc eachRow -> acc <> [eachRow.row]) [[]]

getLmsListAllQuiz :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id DM.Merchant, Kernel.Types.Id.Id DMOC.MerchantOperatingCity) -> Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> Environment.Flow API.Types.UI.LmsModule.LmsGetQuizRes
getLmsListAllQuiz (mbPersonId, _merchantId, merchantOpCityId) modId mbLanguage = do
  personId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  moduleInfo <- fromMaybeM (LmsModuleNotFound modId.getId) . find ((== modId) . (.id)) =<< SCQL.getAllModules Nothing Nothing merchantOpCityId
  merchantOperatingCity <- SCQMM.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityDoesNotExist merchantOpCityId.getId)
  let language = fromMaybe ENGLISH mbLanguage
  mbDriverModuleCompletion <- getmbDriverModuleCompletion personId modId
  questions <- mapM (generateQuizRes personId merchantOperatingCity language mbDriverModuleCompletion) =<< SCQL.getAllQuestions modId

  case moduleInfo.moduleCompletionCriteria of
    LmsModule.VIDEOS_AND_QUIZ noOfQuestionsToBeCorrectToPass -> when (noOfQuestionsToBeCorrectToPass > length questions) $ throwError (NotEnoughQuestionsForModuleCompletionCriteria modId.getId)
    _ -> pure ()

  selectedModuleInfo <- generateModuleInfoRes moduleInfo language
  return $
    API.Types.UI.LmsModule.LmsGetQuizRes
      { questions = questions,
        selectedModuleInfo = selectedModuleInfo
      }
  where
    generateQuizRes personId merchantOperatingCity language mbDriverModuleCompletion question@DTQMM.QuestionModuleMapping {..} = do
      -- getting coins for current question
      questionCoins <- do
        case question.quizCoinFunction of
          Nothing -> return Nothing
          Just functionName -> do
            let vehCategory = DTVeh.getVehicleCategoryFromVehicleVariantDefault Nothing
            coinsConfig <- CDCQ.fetchConfigOnEventAndFunctionBasis DCT.LMS functionName _merchantId merchantOpCityId vehCategory Nothing
            return $ maybe Nothing (\cc -> Just cc.coins) coinsConfig

      translations <- SCQL.getAllTranslationsForQuestionId question.questionId
      questionInfo <-
        ( \case
            Nothing ->
              ( \case
                  Nothing -> fromMaybeM (LmsQuestionTranslationNotFound question.questionId.getId language) $ find ((== ENGLISH) . (.language)) translations
                  Just translation -> return translation
              )
                $ find ((== merchantOperatingCity.language) . (.language)) translations
            Just translation -> return translation
          )
          $ find ((== language) . (.language)) translations
      mbLastQuizAttempt <- listToMaybe <$> maybe (pure []) (SQMCI.findByCompletionIdAndEntityAndEntityId (Just 1) Nothing DTMCI.QUIZ question.questionId.getId . (.completionId)) mbDriverModuleCompletion

      quizCoinsEarnedVal <- do
        allDriverModuleCompletion <- SQDMC.findAllByDriverIdAndModuleId personId modId
        if (length allDriverModuleCompletion == 1)
          then do
            case question.quizCoinFunction of
              Nothing -> return Nothing
              Just functionName -> do
                mbCoinHistory <- SQCC.getCoinsByEventFunction personId functionName (Just question.questionId.getId)
                return $ maybe Nothing (\val -> Just val.coins) mbCoinHistory
          else do return Nothing

      return $
        API.Types.UI.LmsModule.LmsQuestionRes
          { question = (.question) questionInfo,
            options = case questionInfo.questionType of
              DTQI.SingleSelect -> API.Types.UI.LmsModule.SingleSelect $ API.Types.UI.LmsModule.Options {options = (.options) questionInfo}
              DTQI.MultiSelect -> API.Types.UI.LmsModule.MultiSelect $ API.Types.UI.LmsModule.Options {options = (.options) questionInfo},
            previousHistory = generatePreviousQuizHistory quizCoinsEarnedVal =<< mbLastQuizAttempt,
            ..
          }

    generatePreviousQuizHistory quizCoinsEarnedVal questionAttempt =
      Just $
        API.Types.UI.LmsModule.LmsQuizHistory
          { attemptNumber = questionAttempt.attempt,
            selectedOptions = questionAttempt.selectedEntityId,
            status = if questionAttempt.entityStatus == DTMCI.ENTITY_PASSED then API.Types.UI.LmsModule.CORRECT else API.Types.UI.LmsModule.INCORRECT,
            coinsEarned = quizCoinsEarnedVal
          }

postLmsMarkVideoAsStarted :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id DM.Merchant, Kernel.Types.Id.Id DMOC.MerchantOperatingCity) -> API.Types.UI.LmsModule.VideoUpdateAPIReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postLmsMarkVideoAsStarted (mbPersonId, merchantId, merchantOpCityId) req = markVideoByStatus (mbPersonId, merchantId, merchantOpCityId) req DTMCI.ENTITY_ONGOING

postLmsMarkVideoAsCompleted :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id DM.Merchant, Kernel.Types.Id.Id DMOC.MerchantOperatingCity) -> API.Types.UI.LmsModule.VideoUpdateAPIReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postLmsMarkVideoAsCompleted (mbPersonId, merchantId, merchantOpCityId) req = markVideoByStatus (mbPersonId, merchantId, merchantOpCityId) req DTMCI.ENTITY_PASSED

markVideoByStatus :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id DM.Merchant, Kernel.Types.Id.Id DMOC.MerchantOperatingCity) -> API.Types.UI.LmsModule.VideoUpdateAPIReq -> DTMCI.EntityStatus -> Environment.Flow Kernel.Types.APISuccess.APISuccess
markVideoByStatus (mbPersonId, merchantId, merchantOpCityId) req status = do
  personId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  driverStats <- runInReplica $ QDriverStats.findById personId >>= fromMaybeM DriverInfoNotFound
  moduleInfo <- fromMaybeM (LmsModuleNotFound req.moduleId.getId) . find ((== req.moduleId) . (.id)) =<< SCQL.getAllModules Nothing Nothing merchantOpCityId
  videos <- SCQL.getAllVideos req.moduleId [ACTIVE]
  unless (req.videoId `elem` ((.id) <$> videos)) $ do throwError (LmsVideoNotFound req.videoId.getId req.moduleId.getId)

  now <- getCurrentTime
  allDriverModuleCompletion <- SQDMC.findAllByDriverIdAndModuleId personId req.moduleId

  let mbDriverModuleCompletion =
        find
          ( \driverModuleComp -> case driverModuleComp.expiry of
              Nothing -> True
              Just expiry -> (now > expiry)
          )
          allDriverModuleCompletion
  -- create driver module completion entry if not created
  driverModuleCompletion <- maybe (buildDriverModuleCompletion personId req.moduleId merchantId merchantOpCityId) pure =<< (pure mbDriverModuleCompletion)
  currentVideo <- fromMaybeM (LmsVideoTranslationNotFound req.videoId.getId req.language) . find ((== req.language) . (.language)) =<< SCQL.getAllTranslationsForVideoId req.videoId

  -- create module completion information entry with given status
  void $
    (SQMCI.findByCompletionIdAndEntityAndEntityId (Just 1) Nothing DTMCI.VIDEO req.videoId.getId driverModuleCompletion.completionId)
      >>= ( \case
              Nothing -> (createModuleCompletionInformation driverModuleCompletion.completionId 1) >>= SQMCI.create
              Just attempt -> when (attempt.entityStatus /= DTMCI.ENTITY_PASSED) $ do (createModuleCompletionInformation driverModuleCompletion.completionId (attempt.attempt + 1)) >>= SQMCI.create
          )
        . listToMaybe

  void $ case status of
    DTMCI.ENTITY_ONGOING -> SQLVT.updateViewCount (currentVideo.viewCount + 1) currentVideo.videoId currentVideo.language
    DTMCI.ENTITY_PASSED -> do
      void $ SQLVT.updateCompletedWatchCount (currentVideo.completedWatchCount + 1) currentVideo.videoId currentVideo.language
      updateModuleInformationIfVideosCompletedCriteriaIsPassed personId moduleInfo driverModuleCompletion driverStats videos merchantId merchantOpCityId
    _ -> pure ()

  pure Kernel.Types.APISuccess.Success
  where
    createModuleCompletionInformation completionId attemptNumber = do
      now <- getCurrentTime
      return $
        DTMCI.ModuleCompletionInformation
          { completionId = completionId,
            entity = DTMCI.VIDEO,
            entityId = req.videoId.getId,
            selectedEntityId = [req.videoId.getId],
            attempt = attemptNumber,
            entityStatus = status,
            createdAt = now,
            updatedAt = now
          }

    updateModuleInformationIfVideosCompletedCriteriaIsPassed personId moduleInfo dmc driver videos merchantIdParam merchantOpCityIdParam = do
      when ((dmc.status /= DTDMC.MODULE_COMPLETED) && notElem DTDMC.VIDEO (dmc.entitiesCompleted)) $ do
        onlyVideosAsPassingCriteria <- case moduleInfo.moduleCompletionCriteria of
          LmsModule.ONLY_VIDEOS -> pure True
          _ -> pure False
        completedVideos <- SQMCI.findAllByCompletionIdAndEntityAndStatus DTMCI.VIDEO DTMCI.ENTITY_PASSED dmc.completionId
        let completedVideoIds = completedVideos <&> (.entityId)
        let allVideosCompleted = all (\video -> video.id.getId `elem` completedVideoIds) videos
        now <- getCurrentTime
        if allVideosCompleted && onlyVideosAsPassingCriteria
          then do
            void $ SQDMC.updatedCompletedAt (Just now) (dmc.entitiesCompleted <> [DTDMC.VIDEO]) DTDMC.MODULE_COMPLETED driver.rating dmc.completionId
            expiryTime <- getExpiryTime moduleInfo.moduleExpiryConfig personId
            SQDMC.updateExpiryTime expiryTime dmc.completionId
            -- Handle training completion: cancel reminders, record completion, and restore approved flag if needed
            handleTrainingCompletion personId merchantIdParam merchantOpCityIdParam

            case moduleInfo.certificationEnabled of
              Nothing -> pure ()
              Just whetherEnabled -> when whetherEnabled $ do
                void $ generateLmsCertificate personId moduleInfo.id dmc.completionId
          else when allVideosCompleted $ do SQDMC.updateEntitiesCompleted (Just now) (dmc.entitiesCompleted <> [DTDMC.VIDEO]) dmc.completionId

postLmsQuestionConfirm :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id DM.Merchant, Kernel.Types.Id.Id DMOC.MerchantOperatingCity) -> API.Types.UI.LmsModule.QuestionConfirmReq -> Environment.Flow API.Types.UI.LmsModule.QuestionConfirmRes
postLmsQuestionConfirm (mbPersonId, _merchantId, merchantOpCityId) req = do
  personId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  driver <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  driverStats <- runInReplica $ QDriverStats.findById driver.id >>= fromMaybeM DriverInfoNotFound
  moduleInfo <- fromMaybeM (LmsModuleNotFound req.moduleId.getId) . find ((== req.moduleId) . (.id)) =<< SCQL.getAllModules Nothing Nothing merchantOpCityId
  questions <- SCQL.getAllQuestions req.moduleId
  unless (req.questionId `elem` ((.questionId) <$> questions)) $ do throwError (LmsQuestionNotFoundForModule req.questionId.getId req.moduleId.getId)
  let questionModuleMap = find (\eq -> eq.questionId == req.questionId) questions
  mbDriverModuleCompletion <- getmbDriverModuleCompletion personId req.moduleId
  allModulesCompletionInfo <- SQDMC.findAllByDriverIdAndModuleId personId req.moduleId -- all dmc
  driverModuleCompletion <- pure mbDriverModuleCompletion >>= fromMaybeM (LmsDriverModuleCompletionEntryNotFound req.moduleId.getId personId.getId)
  questionInfo <- fromMaybeM (LmsQuestionNotFound req.questionId.getId req.language) . find ((== req.language) . (.language)) =<< SCQL.getAllTranslationsForQuestionId req.questionId
  (validationRes, isCorrect, selectedOptions) <-
    case req.selectedOption of
      API.Types.UI.LmsModule.SingleSelectedOption optionId -> do
        correctOption <-
          ( case questionInfo.questionType of
              DTQI.SingleSelect -> pure $ find (.isCorrect) questionInfo.options
              _ -> pure Nothing
            )
            >>= fromMaybeM (LmsCorrectOptionNotFound req.questionId.getId req.language)
        let validationResult = API.Types.UI.LmsModule.ValidationResult {id = optionId, isCorrect = optionId == correctOption.optionId.getId}
        return (API.Types.UI.LmsModule.SingleSelectedOptionValidation validationResult, optionId == correctOption.optionId.getId, [optionId])
      API.Types.UI.LmsModule.MultiSelectedOption optionIds -> do
        let correctOptionList = case questionInfo.questionType of
              DTQI.MultiSelect -> map (.optionId.getId) $ filter (.isCorrect) questionInfo.options
              _ -> []
        when (null correctOptionList) $ do throwError (LmsCorrectOptionNotFound req.questionId.getId req.language)
        let areAllCorrect = all (`elem` optionIds) correctOptionList
            validationResult = map (\eOptionId -> API.Types.UI.LmsModule.ValidationResult {id = eOptionId, isCorrect = eOptionId `elem` correctOptionList}) optionIds
        return (API.Types.UI.LmsModule.MultiSelectedOptionValidation validationResult, areAllCorrect, correctOptionList)
  -- the following code check whether coins are earned or
  mbCoinHistory <- case questionModuleMap of
    Nothing -> return Nothing
    Just questionMM -> case questionMM.quizCoinFunction of
      Nothing -> return Nothing
      Just functionName -> SQCC.getCoinsByEventFunction personId functionName (Just req.questionId.getId)

  isCoinsEarned <- do
    case mbCoinHistory of
      Nothing ->
        (SQMCI.findByCompletionIdAndEntityAndEntityId (Just 1) Nothing DTMCI.QUIZ req.questionId.getId driverModuleCompletion.completionId)
          >>= ( \case
                  Nothing ->
                    if (isCorrect && (length allModulesCompletionInfo == 1))
                      then do
                        _ <- DC.driverCoinsEvent personId _merchantId merchantOpCityId DCT.LMS (Just req.questionId.getId) Nothing Nothing
                        return (Just True)
                      else do return Nothing
                  _ -> pure Nothing
              )
            . listToMaybe
      Just _ -> pure Nothing

  void $
    (SQMCI.findByCompletionIdAndEntityAndEntityId (Just 1) Nothing DTMCI.QUIZ req.questionId.getId driverModuleCompletion.completionId)
      >>= ( \case
              Nothing -> (createModuleCompletionInformation driverModuleCompletion.completionId 1 isCorrect selectedOptions) >>= SQMCI.create
              Just attempt -> when (attempt.entityStatus /= DTMCI.ENTITY_PASSED) $ do (createModuleCompletionInformation driverModuleCompletion.completionId (attempt.attempt + 1) isCorrect selectedOptions) >>= SQMCI.create
          )
        . listToMaybe

  isbonusEarned <- updateModuleInformationIfQuizCompletedCriteriaIsPassed personId driverModuleCompletion driverStats moduleInfo questions (length allModulesCompletionInfo)

  return $
    API.Types.UI.LmsModule.QuestionConfirmRes
      { validation = if isCorrect then API.Types.UI.LmsModule.CORRECT_ANSWER else API.Types.UI.LmsModule.INCORRECT_ANSWER,
        validationRes = validationRes,
        bonusEarned = isbonusEarned,
        coinsEarned = isCoinsEarned
      }
  where
    createModuleCompletionInformation completionId attemptNumber isCorrect selectedOptions = do
      now <- getCurrentTime
      return $
        DTMCI.ModuleCompletionInformation
          { completionId = completionId,
            entity = DTMCI.QUIZ,
            entityId = req.questionId.getId,
            selectedEntityId = selectedOptions,
            attempt = attemptNumber,
            entityStatus = if isCorrect then DTMCI.ENTITY_PASSED else DTMCI.ENTITY_FAILED,
            createdAt = now,
            updatedAt = now
          }

    updateModuleInformationIfQuizCompletedCriteriaIsPassed personId dmc driverStats moduleInfo questions totalModuleCompletionEntries = do
      if (dmc.status /= DTDMC.MODULE_COMPLETED)
        then do
          completedQuestions <- SQMCI.findAllByCompletionIdAndEntityAndStatus DTMCI.QUIZ DTMCI.ENTITY_PASSED dmc.completionId
          let completedQuestionIds = completedQuestions <&> (.entityId)
          let completedQuestionAttempts = completedQuestions <&> (.attempt)
          let totalCorrectCompleted = foldl' (\acc eQuestion -> if eQuestion.questionId.getId `elem` completedQuestionIds then (acc + 1) else acc) 0 questions

          case moduleInfo.moduleCompletionCriteria of
            LmsModule.VIDEOS_AND_QUIZ noOfQuestion ->
              if (noOfQuestion <= totalCorrectCompleted)
                then do
                  now <- getCurrentTime
                  void $ SQDMC.updatedCompletedAt (Just now) (dmc.entitiesCompleted <> [DTDMC.QUIZ]) DTDMC.MODULE_COMPLETED driverStats.rating dmc.completionId

                  expiryTime <- getExpiryTime moduleInfo.moduleExpiryConfig personId
                  SQDMC.updateExpiryTime expiryTime dmc.completionId
                  -- Handle training completion: cancel reminders, record completion, and restore approved flag if needed
                  handleTrainingCompletion personId _merchantId merchantOpCityId

                  -- adding certificate
                  _ <- do
                    case moduleInfo.certificationEnabled of
                      Nothing -> pure ()
                      Just whetherEnabled -> when whetherEnabled $ do
                        void $ generateLmsCertificate personId moduleInfo.id dmc.completionId

                  -- adding bonus coins
                  if ((all (== 1) completedQuestionAttempts) && totalModuleCompletionEntries == 1 && moduleInfo.bonusCoinEventFunction /= Nothing)
                    then do
                      _ <- DC.driverCoinsEvent personId _merchantId merchantOpCityId DCT.LMSBonus (Just moduleInfo.id.getId) Nothing Nothing
                      return (Just True)
                    else do return (Nothing)
                else do return Nothing
            _ -> return Nothing
        else do return Nothing

generateLmsCertificate :: (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> Kernel.Types.Id.Id DTDMC.DriverModuleCompletion -> Environment.Flow DTLC.LmsCertificate
generateLmsCertificate personId modId driverModuleCompletionId = do
  lmsCertificateId <- Kernel.Types.Id.Id <$> generateGUID
  now <- getCurrentTime
  let certificateInfo =
        DTLC.LmsCertificate
          { id = lmsCertificateId,
            moduleCompletionId = driverModuleCompletionId.getId,
            driverId = personId,
            moduleId = modId,
            createdAt = now,
            updatedAt = now
          }
  void $ SQLC.create certificateInfo
  return certificateInfo

getExpiryTime :: Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.Flow (Maybe UTCTime)
getExpiryTime mbExpiryConfig personId = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  let driverTags = Yudhishthira.convertTags $ fromMaybe [] person.driverTag
  let mbDriverSafetyTag = Yudhishthira.accessTagKey (LYT.TagName "SafetyCohort") driverTags
  now <- getCurrentTime
  case mbExpiryConfig of
    Nothing -> return Nothing
    Just configSeconds -> case mbDriverSafetyTag of
      Nothing -> return Nothing
      Just safetyTag -> case safetyTag of
        "New" -> return $ Just $ addUTCTime (intToNominalDiffTime 7776000) now
        "Safe" -> return $ Just $ addUTCTime (intToNominalDiffTime 7776000) now
        "Unsafe" -> return $ Just $ addUTCTime (intToNominalDiffTime configSeconds) now
        "Watchlist" -> return $ Just $ addUTCTime (intToNominalDiffTime configSeconds) now
        _ -> return Nothing

shouldRestoreApprovedAfterTraining ::
  (MonadFlow m, EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Kernel.Types.Id.Id DMOC.MerchantOperatingCity ->
  m Bool
shouldRestoreApprovedAfterTraining personId merchantOpCityId = do
  transporterConfig <- CCT.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let reminderSystemEnabled = transporterConfig.reminderSystemEnabled == Just True
      driverInspectionEnabled = transporterConfig.requiresDriverOnboardingInspection == Just True
  if reminderSystemEnabled && driverInspectionEnabled
    then do
      -- Check if driver inspection was completed
      inspectionHistories <- QDRH.findAllByDocumentTypeAndEntity DVC.DriverInspectionForm personId.getId DRH.DRIVER
      return $ not (null inspectionHistories)
    else return False

-- | Handle training completion: cancel reminders, record completion, and restore approved flag if needed
handleTrainingCompletion ::
  ( MonadFlow m,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    SchedulerFlow r,
    ServiceFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasField "blackListedJobs" r [Text]
  ) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Kernel.Types.Id.Id DM.Merchant ->
  Kernel.Types.Id.Id DMOC.MerchantOperatingCity ->
  m ()
handleTrainingCompletion personId merchantId merchantOpCityId = do
  -- Cancel pending training video reminders
  cancelRemindersForDriverByDocumentType personId DVC.TrainingForm
  -- Record training video completion for auto-trigger monitoring
  recordDocumentCompletion DVC.TrainingForm personId.getId DRH.DRIVER (Just personId) merchantId merchantOpCityId
  -- Restore approved flag if inspection was already done (only when reminder system and inspection are enabled)
  shouldRestore <- shouldRestoreApprovedAfterTraining personId merchantOpCityId
  when shouldRestore $ do
    QDIExtra.updateApproved (Just True) personId
    logInfo $ "Restored approved=true for driver " <> personId.getId <> " after training completion since inspection was already done"

buildDriverModuleCompletion :: (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> Kernel.Types.Id.Id DM.Merchant -> Kernel.Types.Id.Id DMOC.MerchantOperatingCity -> Environment.Flow DTDMC.DriverModuleCompletion
buildDriverModuleCompletion personId moduleId merchantId merchantOpCityId = do
  completionId <- Kernel.Types.Id.Id <$> generateGUID
  now <- getCurrentTime
  let driverModCompletion =
        DTDMC.DriverModuleCompletion
          { completedAt = Nothing,
            completionId = completionId,
            driverId = personId,
            entitiesCompleted = [],
            moduleId = moduleId,
            ratingAtTheTimeOfCompletion = Nothing,
            startedAt = now,
            status = DTDMC.MODULE_ONGOING,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOpCityId,
            createdAt = now,
            updatedAt = now,
            expiry = Nothing
          }
  void $ SQDMC.create driverModCompletion
  return driverModCompletion

generateModuleInfoRes :: Domain.Types.LmsModule.LmsModule -> Language -> Environment.Flow API.Types.UI.LmsModule.LmsTranslatedModuleInfoRes
generateModuleInfoRes moduleInfo@Domain.Types.LmsModule.LmsModule {..} language = do
  translations <- SCQL.getAllModuleTranslations moduleInfo.id
  translation <-
    ( \case
        Nothing -> fromMaybeM (LmsModuleTranslationNotFound moduleInfo.id.getId language) $ find ((== ENGLISH) . (.language)) translations
        Just translation -> return translation
      )
      $ find ((== language) . (.language)) translations
  return $
    API.Types.UI.LmsModule.LmsTranslatedModuleInfoRes
      { description = (.description) translation,
        name = (.name) translation,
        thumbnailImage = (.thumbnailImage) translation,
        moduleId = moduleInfo.id,
        ..
      }

----------------------------------------------------------------- Certificate ---------------------------------------------------------------------------------

getLmsGetCertificate ::
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id DM.Merchant, Kernel.Types.Id.Id DMOC.MerchantOperatingCity) ->
  Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule ->
  Environment.Flow API.Types.UI.LmsModule.LmsCertificateRes
getLmsGetCertificate (mbPersonId, _merchantId, merchantOpCityId) moduleId = do
  personId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  driver <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  moduleInfo <- fromMaybeM (LmsModuleNotFound moduleId.getId) . find ((== moduleId) . (.id)) =<< SCQL.getAllModules Nothing Nothing merchantOpCityId

  mbDriverModuleCompletion <- getmbDriverModuleCompletion personId moduleId
  getCertificateInfo personId moduleInfo driver mbDriverModuleCompletion
  where
    getCertificateInfo personId moduleInfo driver mbDriverModuleCompletion = do
      case mbDriverModuleCompletion of
        Nothing -> return API.Types.UI.LmsModule.LmsCertificateRes {certificateInfo = Nothing}
        Just dmc -> do
          mbCertificateInfo <- SQLC.findByModuleCompletionIdAndDriverIdAndModuleId dmc.completionId.getId personId moduleInfo.id
          case mbCertificateInfo of
            Nothing -> return API.Types.UI.LmsModule.LmsCertificateRes {certificateInfo = Nothing}
            Just certificate -> do
              let certificateInfo =
                    Just $
                      API.Types.UI.LmsModule.CertificateInfo
                        { certificateId = certificate.id,
                          moduleId = moduleId,
                          certificateCourseName = fromMaybe "" moduleInfo.moduleNameForCertificate,
                          certificateOwnerName = driver.firstName <> (fromMaybe "" driver.lastName),
                          driverId = driver.id,
                          completedAt = dmc.completedAt
                        }
              return $ API.Types.UI.LmsModule.LmsCertificateRes {..}

getLmsGetAllCertificates ::
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id DM.Merchant, Kernel.Types.Id.Id DMOC.MerchantOperatingCity) ->
  Environment.Flow [API.Types.UI.LmsModule.CertificateInfo]
getLmsGetAllCertificates (mbPersonId, _merchantId, merchantOpCityId) = do
  personId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  driver <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)

  allModules <- SCQL.getAllModules Nothing Nothing merchantOpCityId
  certificates <- mapM (generateCertificateResult personId driver) allModules

  return $ catMaybes certificates
  where
    generateCertificateResult personId driver moduleInfo = do
      mbDriverModuleCompletion <- getmbDriverModuleCompletion personId moduleInfo.id
      getCertificateInfo personId moduleInfo driver mbDriverModuleCompletion

    getCertificateInfo personId moduleInfo driver mbDriverModuleCompletion = do
      case mbDriverModuleCompletion of
        Nothing -> return Nothing
        Just dmc -> do
          mbCertificateInfo <- SQLC.findByModuleCompletionIdAndDriverIdAndModuleId dmc.completionId.getId personId moduleInfo.id
          case mbCertificateInfo of
            Nothing -> return Nothing
            Just certificate -> do
              let certificateInfo =
                    Just $
                      API.Types.UI.LmsModule.CertificateInfo
                        { certificateId = certificate.id,
                          moduleId = moduleInfo.id,
                          certificateCourseName = fromMaybe "" moduleInfo.moduleNameForCertificate,
                          certificateOwnerName = driver.firstName <> (fromMaybe "" driver.lastName),
                          driverId = driver.id,
                          completedAt = dmc.completedAt
                        }
              return $ certificateInfo

------------------------------------------------------------ getBonusInfo ---------------------------------------------------------------------------------------------

getLmsGetBonusCoins ::
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id DM.Merchant, Kernel.Types.Id.Id DMOC.MerchantOperatingCity) ->
  Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule ->
  Environment.Flow API.Types.UI.LmsModule.BonusRes
getLmsGetBonusCoins (mbPersonId, _merchantId, merchantOpCityId) moduleId = do
  personId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  moduleInfo <- fromMaybeM (LmsModuleNotFound moduleId.getId) . find ((== moduleId) . (.id)) =<< SCQL.getAllModules Nothing Nothing merchantOpCityId

  allDriverModuleCompletion <- SQDMC.findAllByDriverIdAndModuleId personId moduleInfo.id

  if (length allDriverModuleCompletion == 1)
    then do
      case moduleInfo.bonusCoinEventFunction of
        Nothing -> return API.Types.UI.LmsModule.BonusRes {coins = Nothing}
        Just functionName -> do
          mbCoinHistory <- SQCC.getCoinsByEventFunction personId functionName (Just moduleInfo.id.getId)
          let coinsVal = maybe 0 (\val -> val.coins) mbCoinHistory
          return API.Types.UI.LmsModule.BonusRes {coins = if coinsVal == 0 then Nothing else Just coinsVal}
    else do return $ API.Types.UI.LmsModule.BonusRes {coins = Nothing}

-------------- update expiry time when driver is downgraded to watchlisted / unsafe -----------------------------------------------------------------------------------------------

-- logic :-
-- 1. fetch all modules related to Driver Safety Score -- (because we want specifically for driver safety score)
-- 2. go through all fetched modules and get valid dmc ( Driver Module Completion entry)
-- 3. update valid dmc with provided expiry time

-- following will be configuration for expiry time
-- ==================================================================
-- safe to unsafe/watchlisted = expiryTime = now
-- unsafe to watchlist = dmc.completedAt + 30 days
-- unsafe to safe / watchlisted to safe - dmc.completedAt + 90 days

updateExpiryTimeForDowngradedTag ::
  ( EsqDBFlow m r,
    MonadFlow m,
    CacheFlow m r
  ) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  m ()
updateExpiryTimeForDowngradedTag personId = do
  driver <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  expiryTime <- getCurrentTime
  modules <- SCQL.getAllModulesWithModuleSection Nothing Nothing driver.merchantOperatingCityId (Just DRIVER_SAFETY_SCORE)
  void $ mapM (updateExpiryTime (Just expiryTime)) modules
  where
    updateExpiryTime expiryTime eModule = do
      mbDriverModuleCompletion <- getmbDriverModuleCompletion personId eModule.id
      maybe (pure ()) (SQDMC.updateExpiryTime expiryTime . (.completionId)) mbDriverModuleCompletion

------------------- get valid driver module completion entry ----------------------------------------------------------------------------------------------------------

-- 1. Multiple entries for same driverId and moduleId can be present due to expiry time enabled for driver safety score modules
getmbDriverModuleCompletion ::
  ( EsqDBFlow m r,
    MonadFlow m,
    CacheFlow m r
  ) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule ->
  m (Maybe DTDMC.DriverModuleCompletion)
getmbDriverModuleCompletion personId moduleId = do
  now <- getCurrentTime
  allDriverModuleCompletion <- SQDMC.findAllByDriverIdAndModuleId personId moduleId
  let mbDriverModuleCompletion =
        find
          ( \driverModuleComp -> case driverModuleComp.expiry of
              Nothing -> True
              Just expiry -> (now > expiry)
          )
          allDriverModuleCompletion
  return mbDriverModuleCompletion

-- todo ::
-- check if driver is safe or not - while giving certificate - handle it in frontend
