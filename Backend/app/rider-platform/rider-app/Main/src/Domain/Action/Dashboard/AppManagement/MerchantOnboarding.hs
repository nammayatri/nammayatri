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
    merchantOnboardingGetFile,
    merchantOnboardingCancel,
  )
where

import API.Types.Dashboard.AppManagement.MerchantOnboarding (UploadFileRequest (..))
import qualified API.Types.Dashboard.AppManagement.MerchantOnboarding
import Data.Aeson as A
import qualified Data.Aeson
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Domain.Action.Dashboard.AppManagement.MerchantOnboarding.Handlers as Handlers
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOnboarding as DMO
import qualified "this" Domain.Types.MerchantOnboarding
import qualified Domain.Types.MerchantOnboardingStep as DMOS
import qualified "this" Domain.Types.MerchantOnboardingStep
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
import Kernel.Types.Id (Id)
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, throwError)
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Beam.IssueManagement ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Flow as Storage
import Storage.Queries.MerchantOnboarding as QMO
import Storage.Queries.MerchantOnboardingStep as QMOS
import Storage.Queries.MerchantOnboardingStepConfig as QMOSC
import Storage.Types (FileType (..))
import System.IO (hFileSize)
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
  let finalSteps = if null stepsToUpdate then steps else updatedSteps
  when (all (\step -> step.status == DMOS.COMPLETED) finalSteps) $
    QMO.updateOnboardingStatus DMO.COMPLETED onboardingId
  return finalSteps
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

merchantOnboardingInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.Flow Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingInfo _merchantShortId _opCity onboardingType' requestorId mbRequestorRole = do
  reqId <- requestorId & fromMaybeM (InvalidRequest "Requestor ID is required")
  reqRole <- mbRequestorRole & fromMaybeM (InvalidRequest "RequestorRole is required")
  onboardingType <- readMaybe onboardingType' & fromMaybeM (InvalidRequest "Invalid onboarding type")
  onboarding <- QMO.findByRequestorIdAndOnboardingType reqId onboardingType >>= fromMaybeM (InvalidRequest $ "No onboarding present of type " <> show onboardingType)
  unless (onboarding.requestorId == reqId || reqRole `elem` [DMO.TICKET_DASHBOARD_ADMIN, DMO.TICKET_DASHBOARD_APPROVER]) $
    throwError $ InvalidRequest "RequestorId does not have access to this onboarding"
  steps <- getStepsAndUpdate onboarding.id
  if (onboarding.status /= DMO.COMPLETED && all (\step -> step.status == DMOS.COMPLETED) steps)
    then do
      QMO.updateOnboardingStatus DMO.COMPLETED onboarding.id
      return $ mkMerchantOnboardingAPI (onboarding {DMO.status = DMO.COMPLETED}) steps
    else return $ mkMerchantOnboardingAPI onboarding steps

merchantOnboardingStart :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.Flow Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingStart _merchantShortId _opCity onboardingType' requestorId _mbRequestorRole = do
  reqId <- requestorId & fromMaybeM (InvalidRequest "Requestor ID is required")
  onboardingType <- readMaybe onboardingType' & fromMaybeM (InvalidRequest "Invalid onboarding type")
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
      forM_ stepConfigs $ \config -> do
        stepId <- (Map.lookup config.stepNameIdentifier stepNameToIdMap) & fromMaybeM (InternalError $ "Step ID not found for " <> config.stepNameIdentifier)
        let stepDependencies = map (Kernel.Types.Id.Id) $ catMaybes $ map (\depName -> Map.lookup depName stepNameToIdMap) config.dependency
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
                  dependency = stepDependencies,
                  status = initialStatus,
                  isApprovalRequired = config.isApprovalRequired,
                  isAdminOnly = config.isAdminOnly,
                  payload = Nothing,
                  remarks = Nothing,
                  createdAt = now,
                  updatedAt = now
                }

        QMOS.create step
      updatedSteps <- getStepsAndUpdate onboarding.id
      return $ mkMerchantOnboardingAPI onboarding updatedSteps

merchantOnboardingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.Flow [Domain.Types.MerchantOnboarding.MerchantOnboarding])
merchantOnboardingList _merchantShortId _opCity requestorId _mbRequestorRole = do
  reqId <- requestorId & fromMaybeM (InvalidRequest "Requestor ID is required")
  onboardings <- QMO.findAllByRequestorId reqId
  return onboardings

merchantOnboardingStepSubmit :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Text -> Maybe Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Value -> Environment.Flow Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingStepSubmit _merchantShortId _opCity stepId requestorId mbRequestorRole payload = do
  reqId <- fromMaybeM (InvalidRequest "RequestorId is required") requestorId
  reqRole <- mbRequestorRole & fromMaybeM (InvalidRequest "RequestorRole is required")
  step <- QMOS.findByStepId (Kernel.Types.Id.Id stepId) >>= fromMaybeM (InvalidRequest "Step not found")
  onboarding <- QMO.findById (Kernel.Types.Id.Id step.merchantOnboardingId) >>= fromMaybeM (InvalidRequest "No onboarding found")
  unless (onboarding.requestorId == reqId || reqRole `elem` [DMO.TICKET_DASHBOARD_ADMIN, DMO.TICKET_DASHBOARD_APPROVER]) $
    throwError $ InvalidRequest "RequestorId does not match onboarding requestorId"
  let isAdminOnlyStep = fromMaybe False step.isAdminOnly
  when (reqRole `notElem` [DMO.TICKET_DASHBOARD_ADMIN, DMO.TICKET_DASHBOARD_APPROVER] && isAdminOnlyStep) $
    throwError $ InvalidRequest "Don't have permission to submit this step"
  unless (step.status == DMOS.AVAILABLE || step.status == DMOS.INPROGRESS || step.status == DMOS.REOPENED) $
    throwError $ InvalidRequest "Step is not available for submission"
  let mbHandler = Map.lookup (onboarding.onboardingType, step.stepNameIdentifier <> "-SUBMIT-HANDLER") Handlers.handlerRegistry.handlers
  whenJust mbHandler $ \handler -> do
    result <- handler.validateAndProcess (Kernel.Types.Id.Id stepId) payload
    unless result.success $
      throwError $ InvalidRequest $ fromMaybe "Step processing failed" result.message
  if step.isApprovalRequired
    then updateStepStatus DMOS.SUBMITTED step.id
    else updateStepStatus DMOS.COMPLETED step.id
  steps <- getStepsAndUpdate onboarding.id
  return $ mkMerchantOnboardingAPI onboarding steps

merchantOnboardingStepUpdatePayload :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Text -> Maybe Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Value -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
merchantOnboardingStepUpdatePayload _merchantShortId _opCity stepId requestorId mbRequestorRole payload = do
  reqId <- fromMaybeM (InvalidRequest "RequestorId is required") requestorId
  reqRole <- mbRequestorRole & fromMaybeM (InvalidRequest "RequestorRole is required")
  step <- QMOS.findByStepId (Kernel.Types.Id.Id stepId) >>= fromMaybeM (InvalidRequest "Step not found")
  onboarding <- QMO.findById (Kernel.Types.Id.Id step.merchantOnboardingId) >>= fromMaybeM (InvalidRequest "No onboarding found")
  unless (onboarding.requestorId == reqId || reqRole `elem` [DMO.TICKET_DASHBOARD_ADMIN, DMO.TICKET_DASHBOARD_APPROVER]) $
    throwError $ InvalidRequest "RequestorId does not have access to this onboarding"
  unless (step.status == DMOS.AVAILABLE || step.status == DMOS.INPROGRESS || step.status == DMOS.REOPENED) $
    throwError $ InvalidRequest "Step is not available for update"
  QMOS.updateStepPayload (Just payload) step.id
  return Kernel.Types.APISuccess.Success

data StepRejectRequest = StepRejectRequest
  { remarks :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

merchantOnboardingStepReject :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Text -> Maybe Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Value -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
merchantOnboardingStepReject _merchantShortId _opCity stepId requestorId mbRequestorRole payload = do
  _reqId <- fromMaybeM (InvalidRequest "RequestorId is required") requestorId
  reqRole <- mbRequestorRole & fromMaybeM (InvalidRequest "RequestorRole is required")
  unless (reqRole `elem` [DMO.TICKET_DASHBOARD_ADMIN, DMO.TICKET_DASHBOARD_APPROVER]) $
    throwError $ InvalidRequest "RequestorId does not have access to this onboarding"
  step <- QMOS.findByStepId (Kernel.Types.Id.Id stepId) >>= fromMaybeM (InvalidRequest "Step not found")
  unless (step.status == DMOS.SUBMITTED) $
    throwError $ InvalidRequest "Step is not pending approval"
  rejectReq :: StepRejectRequest <- fromjson payload
  updateStepStatus DMOS.REOPENED step.id
  updateStepRemarks (Just rejectReq.remarks) step.id
  return Kernel.Types.APISuccess.Success

data StepApproveRequest = StepApproveRequest
  { remarks :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

merchantOnboardingStepApprove :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Text -> Maybe Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Value -> Environment.Flow API.Types.Dashboard.AppManagement.MerchantOnboarding.ApproveResponse)
merchantOnboardingStepApprove _merchantShortId _opCity stepId requestorId mbRequestorRole payload = do
  _reqId <- requestorId & fromMaybeM (InvalidRequest "RequestorId is required")
  reqRole <- mbRequestorRole & fromMaybeM (InvalidRequest "RequestorRole is required")
  unless (reqRole `elem` [DMO.TICKET_DASHBOARD_ADMIN, DMO.TICKET_DASHBOARD_APPROVER]) $
    throwError $ InvalidRequest "RequestorId does not have access to this onboarding"
  step <- QMOS.findByStepId (Kernel.Types.Id.Id stepId) >>= fromMaybeM (InvalidRequest "Step not found")
  onboarding <- QMO.findById (Kernel.Types.Id.Id step.merchantOnboardingId) >>= fromMaybeM (InvalidRequest "No onboarding found")
  unless (step.status == DMOS.SUBMITTED && step.isApprovalRequired) $
    throwError $ InvalidRequest "Step is not pending approval"
  approveReq :: StepApproveRequest <- fromjson payload
  dashboardSideHandler <- case Map.lookup (onboarding.onboardingType, step.stepNameIdentifier <> "-APPROVAL-HANDLER") Handlers.handlerRegistry.handlers of
    Nothing -> do
      updateStepStatus DMOS.COMPLETED step.id
      updateStepRemarks approveReq.remarks step.id
      return Nothing
    Just handler -> do
      res <- handler.validateAndProcess (Kernel.Types.Id.Id stepId) payload
      unless res.success $
        throwError $ InvalidRequest $ fromMaybe "Step processing failed" res.message
      updateStepStatus DMOS.COMPLETED step.id
      updateStepRemarks approveReq.remarks step.id
      pure res.dashboardSideHandler
  _ <- getStepsAndUpdate onboarding.id
  return $ API.Types.Dashboard.AppManagement.MerchantOnboarding.ApproveResponse {success = True, handler = dashboardSideHandler}

merchantOnboardingStepUploadFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> API.Types.Dashboard.AppManagement.MerchantOnboarding.UploadFileRequest -> Environment.Flow API.Types.Dashboard.AppManagement.MerchantOnboarding.UploadFileResponse)
merchantOnboardingStepUploadFile _merchantShortId _opCity stepId payloadKey requestorId mbRequestorRole (UploadFileRequest {..}) = do
  reqId <- fromMaybeM (InvalidRequest "RequestorId is required") requestorId
  reqRole <- mbRequestorRole & fromMaybeM (InvalidRequest "RequestorRole is required")
  step <- QMOS.findByStepId (Kernel.Types.Id.Id stepId) >>= fromMaybeM (InvalidRequest "Step not found")
  unless (step.status == DMOS.AVAILABLE || step.status == DMOS.INPROGRESS || step.status == DMOS.REOPENED) $
    throwError $ InvalidRequest "Step is not available for upload"
  onboarding <- QMO.findById (Kernel.Types.Id.Id step.merchantOnboardingId) >>= fromMaybeM (InvalidRequest "No onboarding found")
  unless (onboarding.requestorId == reqId || reqRole `elem` [DMO.TICKET_DASHBOARD_ADMIN, DMO.TICKET_DASHBOARD_APPROVER]) $
    throwError $ InvalidRequest "RequestorId does not have this access"
  merchant <- findMerchantByShortId _merchantShortId
  merchantConfig <- CQM.findById (merchant.id) >>= fromMaybeM (MerchantNotFound merchant.id.getId)
  fileSize <- L.runIO $ withFile file ReadMode hFileSize
  when (fileSize > fromIntegral merchantConfig.mediaFileSizeUpperLimit) $
    throwError $ FileSizeExceededError ("File size " <> show fileSize <> " exceeds the limit of " <> show merchantConfig.mediaFileSizeUpperLimit)

  documentFile <- L.runIO $ base64Encode <$> BS.readFile file
  filePath <- Storage.createFilePath "/onboarding/" ("step-" <> stepId <> "-payloadKey-" <> payloadKey) fileType reqContentType
  let fileUrl =
        merchantConfig.mediaFileUrlPattern
          & T.replace "<DOMAIN>" "merchant-onboarding"
          & T.replace "<FILE_PATH>" filePath
  _ <- fork "Storage put file" $ Storage.put (T.unpack filePath) documentFile
  uploadFileRes <- createMediaEntry fileUrl fileType filePath
  updateStepPayloadWithFileId step payloadKey uploadFileRes.fileId.getId
  return uploadFileRes
  where
    updateStepPayloadWithFileId step_ payloadKey_ fileId_ = do
      let currentPayload = fromMaybe (A.Null) step_.payload
      let newPayload = case currentPayload of
            A.Object obj -> A.Object $ KeyMap.insert (fromText payloadKey_) (A.String fileId_) obj
            _ -> A.Object $ KeyMap.singleton (fromText payloadKey_) (A.String fileId_)
      QMOS.updateStepPayload (Just newPayload) step_.id

data MBRejectRequest = MBRejectRequest
  { remarks :: Kernel.Prelude.Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

merchantOnboardingReject :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Data.Aeson.Value -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
merchantOnboardingReject _merchantShortId _opCity onboardingId requestorId mbRequestorRole req = do
  _reqId <- requestorId & fromMaybeM (InvalidRequest "RequestorId is required")
  reqRole <- mbRequestorRole & fromMaybeM (InvalidRequest "RequestorRole is required")
  unless (reqRole `elem` [DMO.TICKET_DASHBOARD_ADMIN, DMO.TICKET_DASHBOARD_APPROVER]) $
    throwError $ InvalidRequest "RequestorId does not have access to this onboarding"
  rejectReq :: MBRejectRequest <- fromjson req
  onboarding <- QMO.findById (Kernel.Types.Id.Id onboardingId) >>= fromMaybeM (InvalidRequest "No onboarding found")
  unless (onboarding.status /= DMO.COMPLETED) $
    throwError $ InvalidRequest "RequestorId does not match onboarding requestorId"
  QMO.updateOnboardingStatusAndRemarks DMO.REJECTED (Just rejectReq.remarks) onboarding.id
  return Kernel.Types.APISuccess.Success

merchantOnboadingListAll :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.OnboardingStatus -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.OnboardingType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.Flow [Domain.Types.MerchantOnboarding.MerchantOnboardingAPI])
merchantOnboadingListAll _merchantShortId _opCity mbRequestorId mbRequestorRole mbStatus mbOnboardingType limit offset = do
  onboardingType <- mbOnboardingType & fromMaybeM (InvalidRequest "OnboardingType is required")
  _reqId <- mbRequestorId & fromMaybeM (InvalidRequest "RequestorId is required")
  reqRole <- mbRequestorRole & fromMaybeM (InvalidRequest "RequestorRole is required")
  unless (reqRole `elem` [DMO.TICKET_DASHBOARD_ADMIN, DMO.TICKET_DASHBOARD_APPROVER]) $
    throwError $ InvalidRequest "RequestorId does not have access to this onboarding"
  onboardings <- filter (\ob -> maybe True (== ob.status) mbStatus) <$> QMO.findAllByOnboardingType limit offset onboardingType
  forM onboardings $ \onboarding -> do
    steps <- getStepsAndUpdate onboarding.id
    return $ mkMerchantOnboardingAPI onboarding steps

merchantOnboardingStepList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Text -> Maybe Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.Flow [DMOS.MerchantOnboardingStep])
merchantOnboardingStepList _merchantShortId _opCity onboardingId requestorId mbRequestorRole = do
  reqId <- fromMaybeM (InvalidRequest "RequestorId is required") requestorId
  reqRole <- mbRequestorRole & fromMaybeM (InvalidRequest "RequestorRole is required")
  onboarding <- QMO.findById (Kernel.Types.Id.Id onboardingId) >>= fromMaybeM (InvalidRequest "No onboarding found")
  unless (onboarding.requestorId == reqId || reqRole `elem` [DMO.TICKET_DASHBOARD_ADMIN, DMO.TICKET_DASHBOARD_APPROVER]) $
    throwError $ InvalidRequest "RequestorId does not have access to this onboarding"
  steps <- QMOS.findByMerchantOnboardingId onboarding.id.getId
  return steps

createMediaEntry :: Text -> FileType -> Text -> Environment.Flow API.Types.Dashboard.AppManagement.MerchantOnboarding.UploadFileResponse
createMediaEntry url fileType filePath = do
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
            _type = fileType,
            url = fileUrl,
            s3FilePath = Just filePath,
            createdAt = now
          }

merchantOnboardingGetFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.Flow Domain.Types.MerchantOnboarding.GetFileResponse)
merchantOnboardingGetFile _merchantShortId _opCity onboardingId fileId requestorId requestorRole = do
  reqId <- requestorId & fromMaybeM (InvalidRequest "RequestorId is required")
  reqRole <- requestorRole & fromMaybeM (InvalidRequest "RequestorRole is required")
  onboarding <- QMO.findById (Kernel.Types.Id.Id onboardingId) >>= fromMaybeM (InvalidRequest "No onboarding found")
  unless (onboarding.requestorId == reqId || reqRole `elem` [DMO.TICKET_DASHBOARD_ADMIN, DMO.TICKET_DASHBOARD_APPROVER]) $
    throwError $ InvalidRequest "RequestorId does not have access to this file"
  file <- MFQuery.findById (Kernel.Types.Id.Id fileId) >>= fromMaybeM (InvalidRequest "No file found")
  filePath <- file.s3FilePath & fromMaybeM (FileDoNotExist fileId)
  base64File <- Storage.get $ T.unpack filePath
  return $ Domain.Types.MerchantOnboarding.GetFileResponse {fileBase64 = base64File, fileType = show file._type}

merchantOnboardingCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
merchantOnboardingCancel _merchantShortId _opCity onboardingId requestorId requestorRole = do
  reqId <- requestorId & fromMaybeM (InvalidRequest "RequestorId is required")
  reqRole <- requestorRole & fromMaybeM (InvalidRequest "RequestorRole is required")
  onboarding <- QMO.findById (Kernel.Types.Id.Id onboardingId) >>= fromMaybeM (InvalidRequest "No onboarding found")
  unless ((onboarding.requestorId == reqId && reqRole == DMO.TICKET_DASHBOARD_USER) || reqRole `elem` [DMO.TICKET_DASHBOARD_ADMIN, DMO.TICKET_DASHBOARD_APPROVER]) $
    throwError $ InvalidRequest "RequestorId does not have access to this file"
  QMO.deleteById onboarding.id
  QMOS.deleteByOnboardingId onboarding.id.getId
  return Kernel.Types.APISuccess.Success

fromjson :: (FromJSON a) => Data.Aeson.Value -> Environment.Flow a
fromjson val = case Data.Aeson.fromJSON val of
  Data.Aeson.Success x -> return x
  Data.Aeson.Error err -> throwError (InvalidRequest $ T.pack err)
