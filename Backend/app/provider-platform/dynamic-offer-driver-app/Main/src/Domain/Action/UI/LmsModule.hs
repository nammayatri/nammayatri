{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.UI.LmsModule where

import API.Types.UI.LmsModule
import qualified Domain.Types.DriverModuleCompletion as DTDMC
import Domain.Types.LmsModule (LmsModule (LmsModule))
import Domain.Types.LmsModule as LmsModule
import Domain.Types.LmsModuleVideoInformation as LmsModuleVideoInformation
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ModuleCompletionInformation as DTMCI
import qualified Domain.Types.Person
import qualified Domain.Types.QuestionInformation as DTQI
import qualified Domain.Types.QuestionModuleMapping as DTQMM
import qualified Domain.Types.ReelsData as DTRD
import qualified Domain.Types.VehicleVariant
import qualified Environment
import EulerHS.Prelude hiding (id, length)
import Kernel.Beam.Functions
import Kernel.External.Types (Language (..))
import Kernel.Prelude hiding (all, elem, find, foldl', map, notElem, null, whenJust)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Lms as SCQL
import Storage.CachedQueries.Merchant.MerchantOperatingCity as SCQMM
import qualified Storage.Queries.DriverModuleCompletion as SQDMC
import qualified Storage.Queries.DriverStats as QDriverStats
import Storage.Queries.LmsVideoTranslation as SQLVT
import Storage.Queries.ModuleCompletionInformation as SQMCI
import qualified Storage.Queries.Person as QPerson
import Tools.Error

getLmsListAllModules :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant, Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Domain.Types.VehicleVariant.VehicleVariant) -> Environment.Flow API.Types.UI.LmsModule.LmsGetModuleRes
getLmsListAllModules (mbPersonId, _merchantId, merchantOpCityId) mbLanguage _mbLimit _mbOffset _mbVariant = do
  personId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  driver <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  let language = fromMaybe ENGLISH mbLanguage
  modules <- mapM (generateModuleInfo language driver.id) =<< SCQL.getAllModules Nothing Nothing merchantOpCityId -- todo :: mbLimit and mbOffset ( phase 2 of LMS)
  return $
    API.Types.UI.LmsModule.LmsGetModuleRes
      { completed = sortOn (.completedAt) $ filter ((== DTDMC.MODULE_COMPLETED) . (.moduleCompletionStatus)) modules,
        remaining = sortOn (.rank) $ filter ((/= DTDMC.MODULE_COMPLETED) . (.moduleCompletionStatus)) modules
      }
  where
    generateModuleInfo language personId eModule@LmsModule {..} = do
      translations <- SCQL.getAllModuleTranslations eModule.id
      translation <-
        ( \case
            Nothing -> fromMaybeM (LmsModuleTranslationNotFound eModule.id.getId language) $ find ((== ENGLISH) . (.language)) translations
            Just translation -> return translation
          )
          $ find ((== language) . (.language)) translations
      mbModuleCompletionInfo <- SQDMC.findByDriverIdAndModuleId personId eModule.id
      totalVideosWatched <- maybe (pure []) (SQMCI.findAllByCompletionIdAndEntityAndStatus DTMCI.VIDEO DTMCI.ENTITY_PASSED . (.completionId)) mbModuleCompletionInfo
      now <- getCurrentTime
      return $
        API.Types.UI.LmsModule.LmsModuleRes
          { moduleId = eModule.id,
            name = (.name) translation,
            description = (.description) translation,
            thumbnailImage = (.thumbnailImage) translation,
            moduleCompletionStatus = maybe DTDMC.MODULE_NOT_YET_STARTED (.status) mbModuleCompletionInfo,
            completedAt = maybe (Just now) (.completedAt) mbModuleCompletionInfo,
            noOfVideosCompleted = length totalVideosWatched,
            ..
          }

getLmsListAllVideos :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant, Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> Environment.Flow API.Types.UI.LmsModule.LmsGetVideosRes
getLmsListAllVideos (mbPersonId, _merchantId, merchantOpCityId) modId mbLanguage = do
  personId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  let language = fromMaybe ENGLISH mbLanguage
  merchantOperatingCity <- SCQMM.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityDoesNotExist merchantOpCityId.getId)
  moduleInfo <- fromMaybeM (LmsModuleNotFound modId.getId) . find ((== modId) . (.id)) =<< SCQL.getAllModules Nothing Nothing merchantOpCityId
  mbModuleCompletionInfo <- SQDMC.findByDriverIdAndModuleId personId modId
  videos <- mapM (generateVideoRes merchantOperatingCity language mbModuleCompletionInfo) =<< SCQL.getAllVideos modId [ACTIVE]
  selectedModuleInfo <- generateModuleInfoRes moduleInfo language
  return $
    API.Types.UI.LmsModule.LmsGetVideosRes
      { quizEnabled = case moduleInfo.moduleCompletionCriteria of
          LmsModule.VIDEOS_AND_QUIZ _ -> True
          _ -> False,
        quizStatus = maybe API.Types.UI.LmsModule.ENTITY_INCOMPLETE (\modCompInfo -> if DTDMC.QUIZ `elem` modCompInfo.entitiesCompleted then API.Types.UI.LmsModule.ENTITY_COMPLETED else API.Types.UI.LmsModule.ENTITY_INCOMPLETE) mbModuleCompletionInfo,
        completed = sortOn (.completedAt) $ filter ((== API.Types.UI.LmsModule.ENTITY_COMPLETED) . (.videoCompletionStatus)) videos,
        pending = sortOn (.rank) $ filter ((== API.Types.UI.LmsModule.ENTITY_INCOMPLETE) . (.videoCompletionStatus)) videos,
        selectedModuleInfo = selectedModuleInfo
      }
  where
    generateVideoRes merchantOperatingCity language mbModuleCompletionInfo video@LmsModuleVideoInformation {..} = do
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
      mbVideoCompletionInfo <- listToMaybe <$> maybe (pure []) (SQMCI.findByCompletionIdAndEntityAndEntityId (Just 1) Nothing DTMCI.VIDEO video.id.getId . (.completionId)) mbModuleCompletionInfo
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

getLmsListAllQuiz :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant, Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> Environment.Flow API.Types.UI.LmsModule.LmsGetQuizRes
getLmsListAllQuiz (mbPersonId, _merchantId, merchantOpCityId) modId mbLanguage = do
  personId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  moduleInfo <- fromMaybeM (LmsModuleNotFound modId.getId) . find ((== modId) . (.id)) =<< SCQL.getAllModules Nothing Nothing merchantOpCityId
  merchantOperatingCity <- SCQMM.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityDoesNotExist merchantOpCityId.getId)
  let language = fromMaybe ENGLISH mbLanguage
  mbModuleCompletionInfo <- SQDMC.findByDriverIdAndModuleId personId modId
  questions <- mapM (generateQuizRes merchantOperatingCity modId language mbModuleCompletionInfo) =<< SCQL.getAllQuestions modId

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
    generateQuizRes merchantOperatingCity _modId language mbModuleCompletionInfo question@DTQMM.QuestionModuleMapping {..} = do
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
      mbLastQuizAttempt <- listToMaybe <$> maybe (pure []) (SQMCI.findByCompletionIdAndEntityAndEntityId (Just 1) Nothing DTMCI.QUIZ question.questionId.getId . (.completionId)) mbModuleCompletionInfo
      return $
        API.Types.UI.LmsModule.LmsQuestionRes
          { question = (.question) questionInfo,
            options = case questionInfo.questionType of
              DTQI.SingleSelect -> API.Types.UI.LmsModule.SingleSelect $ API.Types.UI.LmsModule.Options {options = (.options) questionInfo}
              DTQI.MultiSelect -> API.Types.UI.LmsModule.MultiSelect $ API.Types.UI.LmsModule.Options {options = (.options) questionInfo},
            previousHistory = generatePreviousQuizHistory =<< mbLastQuizAttempt,
            ..
          }
    generatePreviousQuizHistory questionAttempt =
      Just $
        API.Types.UI.LmsModule.LmsQuizHistory
          { attemptNumber = questionAttempt.attempt,
            selectedOptions = questionAttempt.selectedEntityId,
            status = if questionAttempt.entityStatus == DTMCI.ENTITY_PASSED then API.Types.UI.LmsModule.CORRECT else API.Types.UI.LmsModule.INCORRECT
          }

postLmsMarkVideoAsStarted :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant, Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> API.Types.UI.LmsModule.VideoUpdateAPIReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postLmsMarkVideoAsStarted (mbPersonId, merchantId, merchantOpCityId) req = markVideoByStatus (mbPersonId, merchantId, merchantOpCityId) req DTMCI.ENTITY_ONGOING

postLmsMarkVideoAsCompleted :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant, Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> API.Types.UI.LmsModule.VideoUpdateAPIReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postLmsMarkVideoAsCompleted (mbPersonId, merchantId, merchantOpCityId) req = markVideoByStatus (mbPersonId, merchantId, merchantOpCityId) req DTMCI.ENTITY_PASSED

markVideoByStatus :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant, Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> API.Types.UI.LmsModule.VideoUpdateAPIReq -> DTMCI.EntityStatus -> Environment.Flow Kernel.Types.APISuccess.APISuccess
markVideoByStatus (mbPersonId, merchantId, merchantOpCityId) req status = do
  personId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  driverStats <- runInReplica $ QDriverStats.findById personId >>= fromMaybeM DriverInfoNotFound
  moduleInfo <- fromMaybeM (LmsModuleNotFound req.moduleId.getId) . find ((== req.moduleId) . (.id)) =<< SCQL.getAllModules Nothing Nothing merchantOpCityId
  videos <- SCQL.getAllVideos req.moduleId [ACTIVE]
  unless (req.videoId `elem` ((.id) <$> videos)) $ do throwError (LmsVideoNotFound req.videoId.getId req.moduleId.getId)

  -- create driver module completion entry if not created
  driverModuleCompletion <- maybe (buildDriverModuleCompletion personId req.moduleId merchantId merchantOpCityId) pure =<< (SQDMC.findByDriverIdAndModuleId personId req.moduleId)

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
      updateModuleInformationIfVideosCompletedCriteriaIsPassed moduleInfo driverModuleCompletion driverStats videos
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
    updateModuleInformationIfVideosCompletedCriteriaIsPassed moduleInfo dmc driver videos = do
      when ((dmc.status /= DTDMC.MODULE_COMPLETED) && notElem DTDMC.VIDEO (dmc.entitiesCompleted)) $ do
        onlyVideosAsPassingCriteria <- case moduleInfo.moduleCompletionCriteria of
          LmsModule.ONLY_VIDEOS -> pure True
          _ -> pure False
        completedVideos <- SQMCI.findAllByCompletionIdAndEntityAndStatus DTMCI.VIDEO DTMCI.ENTITY_PASSED dmc.completionId
        let completedVideoIds = completedVideos <&> (.entityId)
        let allVideosCompleted = all (\video -> video.id.getId `elem` completedVideoIds) videos
        now <- getCurrentTime
        if allVideosCompleted && onlyVideosAsPassingCriteria
          then SQDMC.updatedCompletedAt (Just now) (dmc.entitiesCompleted <> [DTDMC.VIDEO]) DTDMC.MODULE_COMPLETED driver.rating dmc.completionId
          else when allVideosCompleted $ do SQDMC.updateEntitiesCompleted (Just now) (dmc.entitiesCompleted <> [DTDMC.VIDEO]) dmc.completionId

postLmsQuestionConfirm :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant, Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> API.Types.UI.LmsModule.QuestionConfirmReq -> Environment.Flow API.Types.UI.LmsModule.QuestionConfirmRes
postLmsQuestionConfirm (mbPersonId, _merchantId, merchantOpCityId) req = do
  personId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  driver <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  driverStats <- runInReplica $ QDriverStats.findById driver.id >>= fromMaybeM DriverInfoNotFound
  moduleInfo <- fromMaybeM (LmsModuleNotFound req.moduleId.getId) . find ((== req.moduleId) . (.id)) =<< SCQL.getAllModules Nothing Nothing merchantOpCityId
  questions <- SCQL.getAllQuestions req.moduleId
  unless (req.questionId `elem` ((.questionId) <$> questions)) $ do throwError (LmsQuestionNotFoundForModule req.questionId.getId req.moduleId.getId)
  driverModuleCompletion <- SQDMC.findByDriverIdAndModuleId personId req.moduleId >>= fromMaybeM (LmsDriverModuleCompletionEntryNotFound req.moduleId.getId personId.getId)
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
  void $
    (SQMCI.findByCompletionIdAndEntityAndEntityId (Just 1) Nothing DTMCI.QUIZ req.questionId.getId driverModuleCompletion.completionId)
      >>= ( \case
              Nothing -> (createModuleCompletionInformation driverModuleCompletion.completionId 1 isCorrect selectedOptions) >>= SQMCI.create
              Just attempt -> when (attempt.entityStatus /= DTMCI.ENTITY_PASSED) $ do (createModuleCompletionInformation driverModuleCompletion.completionId (attempt.attempt + 1) isCorrect selectedOptions) >>= SQMCI.create
          )
        . listToMaybe
  void $ updateModuleInformationIfQuizCompletedCriteriaIsPassed driverModuleCompletion driverStats moduleInfo questions
  -- todo :: in future if rewards are there for each question watched then add the rewards && bonus
  return $
    API.Types.UI.LmsModule.QuestionConfirmRes
      { validation = if isCorrect then API.Types.UI.LmsModule.CORRECT_ANSWER else API.Types.UI.LmsModule.INCORRECT_ANSWER,
        validationRes = validationRes
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
    updateModuleInformationIfQuizCompletedCriteriaIsPassed dmc driverStats moduleInfo questions = do
      when (dmc.status /= DTDMC.MODULE_COMPLETED) $ do
        completedQuestions <- SQMCI.findAllByCompletionIdAndEntityAndStatus DTMCI.QUIZ DTMCI.ENTITY_PASSED dmc.completionId
        let completedQuestionIds = completedQuestions <&> (.entityId)
        let totalCorrectCompleted = foldl' (\acc eQuestion -> if eQuestion.questionId.getId `elem` completedQuestionIds then (acc + 1) else acc) 0 questions
        case moduleInfo.moduleCompletionCriteria of
          LmsModule.VIDEOS_AND_QUIZ noOfQuestion -> when (noOfQuestion <= totalCorrectCompleted) $ do
            now <- getCurrentTime
            SQDMC.updatedCompletedAt (Just now) (dmc.entitiesCompleted <> [DTDMC.QUIZ]) DTDMC.MODULE_COMPLETED driverStats.rating dmc.completionId
          _ -> return ()

buildDriverModuleCompletion :: (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Environment.Flow DTDMC.DriverModuleCompletion
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
            updatedAt = now
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
