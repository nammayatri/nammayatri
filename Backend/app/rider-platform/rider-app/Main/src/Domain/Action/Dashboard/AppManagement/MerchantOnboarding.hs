{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.AppManagement.MerchantOnboarding
  ( merchantOnboardingInfo,
    merchantOnboardingStart,
    merchantOnboardingList,
    merchantOnboardingStepSubmit,
    merchantOnboardingStepUpdatePayload,
    merchantOnboardingStepReject,
    merchantOnboardingStepApprove,
    merchantOnboardingStepUploadFile,
    merchantOnboardingReject,
    merchantOnboadingListAll,
    merchantOnboardingStepList,
  )
where

import qualified API.Types.Dashboard.AppManagement.MerchantOnboarding
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Message as Common
import qualified AWS.S3 as S3
import qualified Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.MerchantOnboarding
import qualified "this" Domain.Types.MerchantOnboardingStep
import qualified "this" Domain.Types.MerchantOnboardingStepConfig
import qualified Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import EulerHS.Types (base64Encode)
import qualified IssueManagement.Domain.Types.MediaFile as DMF
import qualified IssueManagement.Storage.Queries.MediaFile as MFQuery
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, generateGUID, getCurrentTime, logInfo, throwError)
import Servant hiding (throwError)
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Beam.IssueManagement ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.Queries.MerchantOnboarding as QMO
import Storage.Queries.MerchantOnboardingStep as QMOS
import Storage.Queries.MerchantOnboardingStepConfig as QMOSC
import Tools.Auth
import Tools.Error

mkMerchantOnboardingAPI :: Domain.Types.MerchantOnboarding.MerchantOnboarding -> [Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep] -> Domain.Types.MerchantOnboarding.MerchantOnboardingAPI
mkMerchantOnboardingAPI onboarding steps =
  Domain.Types.MerchantOnboarding.MerchantOnboardingAPI
    { id = onboarding.id,
      requestorId = onboarding.requestorId,
      onboardingType = onboarding.onboardingType,
      description = onboarding.description,
      status = onboarding.status,
      remarks = onboarding.remarks,
      steps = steps,
      createdAt = onboarding.createdAt,
      updatedAt = onboarding.updatedAt
    }

getStepsAndUpdate :: Kernel.Types.Id.Id Domain.Types.MerchantOnboarding.MerchantOnboarding -> Environment.Flow [Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep]
getStepsAndUpdate onboardingId = do
  steps <- QMOS.findByMerchantOnboardingId (Kernel.Types.Id.getId onboardingId)

  let stepStatusMap =
        Kernel.Prelude.foldl'
          ( \acc step ->
              Map.insert (step.id.getId) step.status acc
          )
          Map.empty
          steps

  let updatedSteps = map (updateStepStatus' stepStatusMap) steps

  let stepsToUpdate =
        filter (\(original, updated) -> original.status /= updated.status) $
          zip steps updatedSteps

  forM_ stepsToUpdate $ \(_, updatedStep) ->
    QMOS.updateStepStatus updatedStep.status updatedStep.id

  return $ if null stepsToUpdate then steps else updatedSteps
  where
    updateStepStatus' statusMap step =
      case step.status of
        Domain.Types.MerchantOnboardingStep.AVAILABLE -> step
        Domain.Types.MerchantOnboardingStep.UNAVAILABLE ->
          if allDependenciesCompleted statusMap step.dependency
            then step {Domain.Types.MerchantOnboardingStep.status = Domain.Types.MerchantOnboardingStep.AVAILABLE}
            else step
        _ -> step

    allDependenciesCompleted statusMap dependencies =
      null dependencies
        || all
          ( \depId ->
              case Map.lookup depId.getId statusMap of
                Just Domain.Types.MerchantOnboardingStep.COMPLETED -> True
                _ -> False
          )
          dependencies

merchantOnboardingInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Domain.Types.MerchantOnboarding.OnboardingType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingInfo merchantShortId opCity onboardingType requestorId = do
  reqId <- requestorId & fromMaybeM (InvalidRequest "Requestor ID is required")

  onboarding <- QMO.findByRequestorIdAndOnboardingType reqId onboardingType >>= fromMaybeM (InvalidRequest $ "No onboarding present of type " <> show onboardingType)

  steps <- getStepsAndUpdate onboarding.id

  return $ mkMerchantOnboardingAPI onboarding steps

merchantOnboardingStart :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Domain.Types.MerchantOnboarding.OnboardingType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingStart merchantShortId opCity onboardingType requestorId = do
  reqId <- requestorId & fromMaybeM (InvalidRequest "Requestor ID is required")

  mbOnboarding <- QMO.findByRequestorIdAndOnboardingType reqId onboardingType

  case mbOnboarding of
    Just onboarding -> do
      steps <- getStepsAndUpdate onboarding.id
      return $ mkMerchantOnboardingAPI onboarding steps
    Nothing -> do
      now <- getCurrentTime
      onboardingId <- generateGUID

      stepConfigs <- QMOSC.findByOnboardingType onboardingType
      when (null stepConfigs) $ throwError (InvalidRequest $ "No step configurations found for onboarding type " <> show onboardingType)

      let onboarding =
            Domain.Types.MerchantOnboarding.MerchantOnboarding
              { id = Kernel.Types.Id.Id onboardingId,
                requestorId = reqId,
                onboardingType = onboardingType,
                description = Just $ "Onboarding process for " <> show onboardingType,
                status = Domain.Types.MerchantOnboarding.INPROGRESS,
                remarks = Nothing,
                createdAt = now,
                updatedAt = now
              }

      QMO.create onboarding

      stepNameToIdMap <-
        foldM
          ( \m config -> do
              stepId <- generateGUID
              return $ Map.insert config.stepNameIdentifier stepId m
          )
          Map.empty
          stepConfigs

      steps <- forM stepConfigs $ \config -> do
        stepId <- (Map.lookup config.stepNameIdentifier stepNameToIdMap) & fromMaybeM (InternalError $ "Step ID not found for " <> config.stepNameIdentifier)

        let initialStatus =
              if null config.dependency
                then Domain.Types.MerchantOnboardingStep.AVAILABLE
                else Domain.Types.MerchantOnboardingStep.UNAVAILABLE

        let step =
              Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep
                { id = Kernel.Types.Id.Id stepId,
                  merchantOnboardingId = onboardingId,
                  stepNameIdentifier = config.stepNameIdentifier,
                  stepDescription = config.stepDescription,
                  dependency = [],
                  status = initialStatus,
                  isApprovalRequired = config.isApprovalRequired,
                  payload = Nothing,
                  remarks = Nothing,
                  createdAt = now,
                  updatedAt = now
                }

        QMOS.create step
        return (step, config.dependency)

      forM_ steps $ \(step, dependencyNames) -> do
        let stepDependencies = mapMaybe (\depName -> Map.lookup depName stepNameToIdMap) dependencyNames
        when (not $ null stepDependencies) $
          QMOS.updateDependency (map Kernel.Types.Id.Id stepDependencies) step.id

      updatedSteps <- getStepsAndUpdate onboarding.id

      return $ mkMerchantOnboardingAPI onboarding updatedSteps

merchantOnboardingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow [Domain.Types.MerchantOnboarding.MerchantOnboarding])
merchantOnboardingList merchantShortId opCity requestorId = do
  reqId <- requestorId & fromMaybeM (InvalidRequest "Requestor ID is required")
  onboardings <- QMO.findAllByRequestorId reqId
  return onboardings

merchantOnboardingStepSubmit :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Data.Aeson.Value -> Environment.Flow Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingStepSubmit merchantShortId opCity stepId requestorId req = do
  error "Not implemented"

merchantOnboardingStepUpdatePayload :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Data.Aeson.Value -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
merchantOnboardingStepUpdatePayload merchantShortId opCity stepId requestorId req = do
  error "Not implemented"

merchantOnboardingStepReject :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Data.Aeson.Value -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
merchantOnboardingStepReject merchantShortId opCity stepId requestorId req = do
  error "Not implemented"

merchantOnboardingStepApprove :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Data.Aeson.Value -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
merchantOnboardingStepApprove merchantShortId opCity stepId requestorId req = do
  error "Not implemented"

merchantOnboardingStepUploadFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.Dashboard.AppManagement.MerchantOnboarding.UploadFileRequest -> Environment.Flow API.Types.Dashboard.AppManagement.MerchantOnboarding.UploadFileResponse)
merchantOnboardingStepUploadFile _merchantShortId opCity stepId payloadKey requestorId req = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantConfig <- CQM.findById (merchant.id) >>= fromMaybeM (MerchantNotFound merchant.id.getId)
  documentFile <- L.runIO $ base64Encode <$> BS.readFile req.file
  filePath <- S3.createFilePath "/onboarding/" ("step-" <> stepId <> "-payloadKey-" <> payloadKey) req.fileType ""
  let fileUrl =
        merchantConfig.mediaFileUrlPattern
          & T.replace "<DOMAIN>" "merchant-onboarding"
          & T.replace "<FILE_PATH>" filePath
  _ <- fork "S3 put file" $ S3.put (T.unpack filePath) documentFile
  uploadFileRes <- createMediaEntry Common.AddLinkAsMedia {url = fileUrl, fileType = req.fileType}
  return uploadFileRes

data MBRejectRequest = MBRejectRequest
  { remarks :: Kernel.Prelude.Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

merchantOnboardingReject :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Data.Aeson.Value -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
merchantOnboardingReject merchantShortId opCity stepId requestorId req = do
  rejectReq :: MBRejectRequest <- fromjson req
  updateStepStatus Domain.Types.MerchantOnboardingStep.REOPENED (Kernel.Types.Id.Id stepId)
  updateStepRemarks (Just rejectReq.remarks) (Kernel.Types.Id.Id stepId)
  return Kernel.Types.APISuccess.Success

merchantOnboadingListAll :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.OnboardingStatus -> Environment.Flow [Domain.Types.MerchantOnboarding.MerchantOnboarding])
merchantOnboadingListAll merchantShortId opCity requestorId status = do
  allOnboardings <- QMO.findAllMerchantOnboardings merchantShortId

  -- Filter based on status if provided
  return $ case mbStatus of
    Nothing -> allOnboardings
    Just status -> filter (\onboarding -> onboarding.status == status) allOnboardings

merchantOnboardingStepList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow [Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep])
merchantOnboardingStepList merchantShortId opCity stepId requestorId status = do
  error "Not implemented"

createMediaEntry :: Common.AddLinkAsMedia -> Environment.Flow API.Types.Dashboard.AppManagement.MerchantOnboarding.UploadFileResponse
createMediaEntry Common.AddLinkAsMedia {..} = do
  fileEntity <- mkFile url
  MFQuery.create fileEntity
  return $ API.Types.Dashboard.AppManagement.MerchantOnboarding.UploadFileResponse {fileId = Kernel.Types.Id.cast $ fileEntity.id}
  where
    mkFile fileUrl = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        DMF.MediaFile
          { id,
            _type = S3.Image,
            url = fileUrl,
            s3FilePath = Nothing,
            createdAt = now
          }

fromjson :: (FromJSON a) => Data.Aeson.Value -> Environment.Flow a
fromjson val = case Data.Aeson.fromJSON val of
  Data.Aeson.Success x -> return x
  Data.Aeson.Error err -> throwError (InvalidRequest $ T.pack err)
