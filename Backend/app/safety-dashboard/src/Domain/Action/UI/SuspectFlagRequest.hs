{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.UI.SuspectFlagRequest where

import qualified API.Types.UI.Notification as Notification
import API.Types.UI.SuspectFlagRequest
import qualified "dashboard-helper-api" Dashboard.SafetyPlatform as Safety
import Data.Aeson as A
import Data.Time hiding (getCurrentTime)
import qualified Domain.Action.UI.Suspect as DS
import Domain.Action.UI.Webhook as Webhook
import qualified "lib-dashboard" Domain.Types.Merchant as Merchant
import qualified Domain.Types.Notification
import qualified Domain.Types.Suspect
import qualified Domain.Types.SuspectFlagRequest
import qualified Domain.Types.Transaction as DT
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (forM_, id, length, map, mapM_, readMaybe)
import Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Transaction as T
import qualified "lib-dashboard" Storage.Queries.Merchant as QMerchant
import qualified "lib-dashboard" Storage.Queries.MerchantAccess as QMerchantAccess
import qualified Storage.Queries.MerchantConfigs as SQMC
import "lib-dashboard" Storage.Queries.Person as QP
import qualified Storage.Queries.Suspect as SQ
import Storage.Queries.SuspectExtra
import qualified Storage.Queries.SuspectFlagRequest as SQF
import Storage.Queries.SuspectFlagRequestExtra as SQFE
import "lib-dashboard" Tools.Auth
import Tools.Error

buildTransaction ::
  ( MonadFlow m
  ) =>
  Safety.SafetyEndpoint ->
  TokenInfo ->
  Text ->
  m DT.Transaction
buildTransaction endpoint tokenInfo = T.buildTransactionForSafetyDashboard (DT.SafetyAPI endpoint) (Just tokenInfo)

newtype WebhookReqBody = WebhookReqBody
  { suspectList :: [SuspectBody]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data SuspectBody = SuspectBody
  { dl :: Maybe Text,
    voterId :: Maybe Text,
    flaggedCategory :: Text,
    flaggedReason :: Text,
    flaggedBy :: Text,
    flaggedStatus :: Domain.Types.Suspect.FlaggedStatus
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

getListSuspectsFlag :: TokenInfo -> Maybe Domain.Types.SuspectFlagRequest.AdminApproval -> Maybe Text -> Maybe UTCTime -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Maybe UTCTime -> Maybe Text -> Environment.Flow API.Types.UI.SuspectFlagRequest.SuspectFlagRequestList
getListSuspectsFlag tokenInfo mbAdminApproval mbDl mbFrom mbLimit mbOffset mbTo mbVoterId = do
  let adminApproval = fromMaybe Domain.Types.SuspectFlagRequest.Approved mbAdminApproval
  now <- getCurrentTime
  let limit = max 100 $ fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
      defaultFrom = UTCTime (utctDay now) 0
      from_ = fromMaybe defaultFrom mbFrom
      to = fromMaybe now mbTo
  flagReqList <- SQFE.findByMerchantIdAndAdminApproval limit offset (Just tokenInfo.merchantId) adminApproval mbDl mbVoterId from_ to
  let resp = map mkListFlagResp flagReqList
  let cnt = length resp
  let summary = Notification.Summary {totalCount = 10000, count = cnt}
  return $
    SuspectFlagRequestList
      { flagRequestList = resp,
        summary = summary
      }

mkListFlagResp :: Domain.Types.SuspectFlagRequest.SuspectFlagRequest -> API.Types.UI.SuspectFlagRequest.SuspectFlagRequestResp
mkListFlagResp Domain.Types.SuspectFlagRequest.SuspectFlagRequest {..} = do
  SuspectFlagRequestResp
    { id = id.getId,
      ..
    }

postProcessSuspectFlagRequest :: TokenInfo -> API.Types.UI.SuspectFlagRequest.SuspectApprovalReqList -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postProcessSuspectFlagRequest tokenInfo req = do
  transaction <- buildTransaction Safety.ProcessSuspectFlagRequestListEndpoint tokenInfo (encodeToText req)
  T.withTransactionStoring transaction $ do
    flagReqList <- findFlaggedRequests req tokenInfo.merchantId
    handlePendingFlaggedRequests tokenInfo req flagReqList

findFlaggedRequests :: API.Types.UI.SuspectFlagRequest.SuspectApprovalReqList -> Id Merchant.Merchant -> Environment.Flow [Domain.Types.SuspectFlagRequest.SuspectFlagRequest]
findFlaggedRequests req merchantId = do
  let flagReqIdList = map (\id -> Kernel.Types.Id.Id id) req.suspectFlagRequestIdList
  SQFE.findAllPendingRequestByRequestId flagReqIdList (Just merchantId)

handlePendingFlaggedRequests :: TokenInfo -> API.Types.UI.SuspectFlagRequest.SuspectApprovalReqList -> [Domain.Types.SuspectFlagRequest.SuspectFlagRequest] -> Environment.Flow Kernel.Types.APISuccess.APISuccess
handlePendingFlaggedRequests tokenInfo req pendingFlagReqList = do
  case pendingFlagReqList of
    [] -> return Kernel.Types.APISuccess.Success
    _ -> handlePendingRequests tokenInfo req.adminApproval pendingFlagReqList

handlePendingRequests :: TokenInfo -> Domain.Types.SuspectFlagRequest.AdminApproval -> [Domain.Types.SuspectFlagRequest.SuspectFlagRequest] -> Environment.Flow Kernel.Types.APISuccess.APISuccess
handlePendingRequests tokenInfo adminApproval pendingFlagReqList = do
  let notificationMetadataForMerchantUsers = encodeToText pendingFlagReqList
  merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantNotFound tokenInfo.merchantId.getId)
  person <- QP.findById tokenInfo.personId >>= fromMaybeM (PersonNotFound tokenInfo.personId.getId)
  let approvedBy = person.firstName <> " " <> person.lastName
  DS.updateSuspectStatusHistoryByRequest tokenInfo adminApproval approvedBy Domain.Types.Suspect.Flagged pendingFlagReqList
  allMerchantUsers <- QMerchantAccess.findAllUserAccountForMerchant merchant.id
  let allMerchantUserExceptHim = filter (\user -> user.personId /= tokenInfo.personId) allMerchantUsers
  merchantReceivers <- QP.findAllByIdsAndReceiveNotification $ map (.personId) allMerchantUserExceptHim
  let merchantUsersReceiversIdList = map (.id) merchantReceivers
  updatePendingFlaggedRequestStaus adminApproval pendingFlagReqList
  case adminApproval of
    Domain.Types.SuspectFlagRequest.Approved -> do
      suspectListForNotification <- mapM (\suspect -> addOrUpdateSuspect suspect merchant.shortId.getShortId Domain.Types.Suspect.Flagged) pendingFlagReqList
      let notificationMetadataForOthers = encodeToText suspectListForNotification
      adminReceiverIds <- DS.getRecieverIdListByAcessType DASHBOARD_ADMIN
      merchantAdminReceiverIds <- DS.getMerchantAdminReceiverIdList merchant.id
      DS.sendNotification tokenInfo merchant notificationMetadataForMerchantUsers (length pendingFlagReqList) Domain.Types.Notification.FLAG_REQUEST_APPROVED merchantUsersReceiversIdList
      DS.sendNotification tokenInfo merchant notificationMetadataForOthers (length pendingFlagReqList) Domain.Types.Notification.PARTNER_FLAGGED_SUSPECT (adminReceiverIds <> merchantAdminReceiverIds)
      sendingWebhookToPartners merchant.shortId.getShortId pendingFlagReqList
      return Kernel.Types.APISuccess.Success
    Domain.Types.SuspectFlagRequest.Rejected -> do
      DS.sendNotification tokenInfo merchant notificationMetadataForMerchantUsers (length pendingFlagReqList) Domain.Types.Notification.FLAG_REQUEST_REJECTED merchantUsersReceiversIdList
      return Kernel.Types.APISuccess.Success
    _ -> throwError InvalidAdminApproval

sendingWebhookToPartners :: Text -> [Domain.Types.SuspectFlagRequest.SuspectFlagRequest] -> Environment.Flow ()
sendingWebhookToPartners merchantShortId pendingFlagReqList = do
  merchantConfigs <- SQMC.findByRequestWebHook True
  let suspectListForWebhook = map (\flagReq' -> buildSuspectBody merchantShortId flagReq') pendingFlagReqList
      webhookBody = A.encode $ WebhookReqBody {suspectList = suspectListForWebhook}
  fork "Sending webhook to partners" $ do
    Webhook.sendWebHook merchantConfigs webhookBody

updatePendingFlaggedRequestStaus :: Domain.Types.SuspectFlagRequest.AdminApproval -> [Domain.Types.SuspectFlagRequest.SuspectFlagRequest] -> Environment.Flow ()
updatePendingFlaggedRequestStaus adminApproval pendingFlagReqList = do
  forM_ pendingFlagReqList $ \flagReq ->
    SQF.updateAdminApprovalById adminApproval flagReq.id

addOrUpdateSuspect :: Domain.Types.SuspectFlagRequest.SuspectFlagRequest -> Text -> Domain.Types.Suspect.FlaggedStatus -> Environment.Flow Domain.Types.Suspect.Suspect
addOrUpdateSuspect req merchantShortId flaggedStatus = do
  mbSuspect <- DS.findSuspect req.dl req.voterId
  case mbSuspect of
    Just suspect -> updateSuspect suspect req merchantShortId flaggedStatus
    Nothing -> createNewSuspect req flaggedStatus

updateSuspect :: Domain.Types.Suspect.Suspect -> Domain.Types.SuspectFlagRequest.SuspectFlagRequest -> Text -> Domain.Types.Suspect.FlaggedStatus -> Environment.Flow Domain.Types.Suspect.Suspect
updateSuspect suspect req merchantShortId flaggedStatus = do
  let flaggedBy = Domain.Types.Suspect.FlaggedBy {flaggedCategory = req.flaggedCategory, partnerName = Just merchantShortId, flaggedReason = Just req.flaggedReason, agentContactNumber = req.mobileNumber, reportDetails = req.reportDetails, totalComplaintsCount = req.totalComplaintsCount} : suspect.flaggedBy
  updateFlaggedCounterByKey (suspect.flaggedCounter + 1) flaggedStatus flaggedBy (getUpdateKey req)
  buildUpdatedSuspect (suspect.flaggedCounter + 1) flaggedBy flaggedStatus suspect

createNewSuspect :: Domain.Types.SuspectFlagRequest.SuspectFlagRequest -> Domain.Types.Suspect.FlaggedStatus -> Environment.Flow Domain.Types.Suspect.Suspect
createNewSuspect req flaggedStatus = do
  suspect <- buildSuspect flaggedStatus req
  SQ.create suspect
  return suspect

getUpdateKey :: Domain.Types.SuspectFlagRequest.SuspectFlagRequest -> Maybe Text
getUpdateKey req = case req.dl of
  Just dl -> Just dl
  Nothing -> req.voterId

buildSuspect :: Domain.Types.Suspect.FlaggedStatus -> Domain.Types.SuspectFlagRequest.SuspectFlagRequest -> Environment.Flow Domain.Types.Suspect.Suspect
buildSuspect flaggedStatus' Domain.Types.SuspectFlagRequest.SuspectFlagRequest {..} = do
  now <- getCurrentTime
  id_ <- generateGUID
  return
    Domain.Types.Suspect.Suspect
      { id = id_,
        dl = dl,
        firstName = firstName,
        lastName = lastName,
        voterId = voterId,
        flaggedStatus = flaggedStatus',
        flagUpdatedAt = now,
        flaggedBy = [Domain.Types.Suspect.FlaggedBy {flaggedCategory = flaggedCategory, partnerName = merchantShortId, flaggedReason = Just flaggedReason, agentContactNumber = mobileNumber, reportDetails = reportDetails, totalComplaintsCount = totalComplaintsCount}],
        statusChangedReason = Nothing,
        flaggedCounter = 1,
        createdAt = now,
        updatedAt = now
      }

buildUpdatedSuspect :: Int -> [Domain.Types.Suspect.FlaggedBy] -> Domain.Types.Suspect.FlaggedStatus -> Domain.Types.Suspect.Suspect -> Environment.Flow Domain.Types.Suspect.Suspect
buildUpdatedSuspect flaggedCounter' flaggedBy' flaggedStatus' Domain.Types.Suspect.Suspect {..} = do
  now <- getCurrentTime
  return
    Domain.Types.Suspect.Suspect
      { flaggedCounter = flaggedCounter',
        flaggedBy = flaggedBy',
        flaggedStatus = flaggedStatus',
        updatedAt = now,
        ..
      }

buildSuspectBody :: Text -> Domain.Types.SuspectFlagRequest.SuspectFlagRequest -> SuspectBody
buildSuspectBody merchantshortId req =
  SuspectBody
    { dl = req.dl,
      voterId = req.voterId,
      flaggedCategory = req.flaggedCategory,
      flaggedReason = req.flaggedReason,
      flaggedBy = merchantshortId,
      flaggedStatus = Domain.Types.Suspect.Flagged
    }
