{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.Suspect where

import API.Types.UI.Suspect
import qualified API.Types.UI.Suspect
import qualified "dashboard-helper-api" Dashboard.SafetyPlatform as Safety
import Data.OpenApi (ToSchema)
import Data.Text as T hiding (concat, elem, filter, length, map)
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Notification as Domain.Types.Notification
import Domain.Types.Suspect as Domain.Types.Suspect
import Domain.Types.SuspectFlagRequest as Domain.Types.SuspectFlagRequest
import qualified Domain.Types.SuspectFlagRequest as Domain.Types.SuspectFlagRequest
import qualified Domain.Types.SuspectStatusChangeRequest as Domain.Types.SuspectStatusChangeRequest
import qualified Domain.Types.SuspectStatusHistory as Domain.Types.SuspectStatusHistory
import qualified Domain.Types.Transaction as DT
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (concatMap, elem, filter, id, length, map, mapM_, readMaybe, whenJust)
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Id
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import qualified "lib-dashboard" Storage.Queries.Merchant as QMerchant
import qualified "lib-dashboard" Storage.Queries.MerchantAccess as QMerchantAccess
import qualified Storage.Queries.Notification as SQN
import "lib-dashboard" Storage.Queries.Person as QP
import qualified Storage.Queries.PortalConfigs as PC
import qualified "lib-dashboard" Storage.Queries.Role as QRole
import qualified Storage.Queries.Suspect as SQ
import Storage.Queries.SuspectExtra
import qualified Storage.Queries.SuspectFlagRequest as SQF
import Storage.Queries.SuspectFlagRequestExtra
import qualified Storage.Queries.SuspectStatusChangeRequest as SQSS
import qualified Storage.Queries.SuspectStatusHistory as SQSH
import "lib-dashboard" Tools.Auth
import Tools.Error
import "lib-dashboard" Tools.Error

buildTransaction ::
  ( MonadFlow m
  ) =>
  Safety.SafetyEndpoint ->
  TokenInfo ->
  Text ->
  m DT.Transaction
buildTransaction endpoint tokenInfo request =
  T.buildTransactionForSafetyDashboard (DT.SafetyAPI endpoint) (Just tokenInfo) request

data ChangeFlagNotificationMetadata = ChangeFlagNotificationMetadata
  { suspect :: Domain.Types.Suspect.Suspect,
    reasonToChange :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

postUploadSuspectBulk :: TokenInfo -> API.Types.UI.Suspect.SuspectBulkUploadReq -> Environment.Flow API.Types.UI.Suspect.SuspectBulkUploadResp
postUploadSuspectBulk tokenInfo req = do
  transaction <- buildTransaction Safety.UploadBulkSuspectEndpoint tokenInfo (encodeToText req)
  portalConfig <- PC.findByConfigName "BULK_UPLOAD_COUNT"
  let bulkUploadCount = case portalConfig of
        Just config -> read (T.unpack config.value) :: Int
        Nothing -> 100
  T.withTransactionStoring transaction $ do
    when (length req.suspects > bulkUploadCount) $ throwError (BulkUploadLimitExceeded bulkUploadCount)
    let validSuspects = filter (\suspect -> isJust (suspect.dl) || isJust (suspect.voterId)) req.suspects
        dlList = mapMaybe (\suspect -> suspect.dl) $ validSuspects
        voterIdList = mapMaybe (\suspect -> suspect.voterId) $ validSuspects
    suspectAlreadyFlagged <- findAllByDlAndVoterIdAndMerchantId dlList voterIdList (Just tokenInfo.merchantId)
    let dlListForAlreadyFlagged = mapMaybe (\suspect -> suspect.dl) suspectAlreadyFlagged
        voterIdListForAlreadyFlagged = mapMaybe (\suspect -> suspect.voterId) suspectAlreadyFlagged
    suspectCleanedByAdmin <- findAllByDlOrVoterIdAndFlaggedStatus dlListForAlreadyFlagged voterIdListForAlreadyFlagged Domain.Types.Suspect.Clean
    let suspectNeedToFlagAgain = filter (\suspect -> (suspect.dl, suspect.voterId) `elem` (map (\suspect' -> (suspect'.dl, suspect'.voterId)) suspectCleanedByAdmin)) validSuspects
        suspectNotFlagged = filter (\suspect -> not ((suspect.dl, suspect.voterId) `elem` (map (\suspect' -> (suspect'.dl, suspect'.voterId)) suspectAlreadyFlagged))) validSuspects
        suspectsNeedToFlag = suspectNotFlagged <> suspectNeedToFlagAgain
    when (length suspectsNeedToFlag > 0) $ do
      person <- findById tokenInfo.personId >>= fromMaybeM (PersonNotFound tokenInfo.personId.getId)
      merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantNotFound tokenInfo.merchantId.getId)
      let flaggedBy = person.firstName <> " " <> person.lastName
      suspectFlagRequest <- mapM (\suspect -> buildSuspectFlagRequest suspect tokenInfo merchant.shortId.getShortId flaggedBy Domain.Types.SuspectFlagRequest.Pending) suspectsNeedToFlag
      mapM_
        ( \flagRequest -> do
            suspectStatusHistory <- buildSuspectStatusHistory tokenInfo (Just flaggedBy) (Just Domain.Types.SuspectFlagRequest.Pending) Domain.Types.Suspect.Flagged flagRequest
            SQSH.create suspectStatusHistory
        )
        suspectFlagRequest
      SQF.createMany suspectFlagRequest
      let notificationMetadata = encodeToText $ suspectFlagRequest
      adminRole <- QRole.findByDashboardAccessType MERCHANT_ADMIN >>= fromMaybeM (RoleDoesNotExist "MERCHANT_ADMIN")
      allMerchantUsers <- QMerchantAccess.findAllUserAccountForMerchant merchant.id
      let personIds = map (\merchantUser -> merchantUser.personId) allMerchantUsers
      merchantAdminList <- QP.findAllByIdAndRoleId personIds adminRole.id
      mapM_
        ( \receiver -> do
            notification <- buildNotification tokenInfo.merchantId merchant.shortId.getShortId (length suspectsNeedToFlag) Domain.Types.Notification.FLAG_REQUEST_UPLOAD notificationMetadata receiver.id.getId tokenInfo.personId.getId
            SQN.create notification
        )
        merchantAdminList
    let message = case length suspectsNeedToFlag of
          0 -> "All DLs or VoterIds are already flagged before"
          _ -> "Dl and VoterId List provided who were already Flagged before."
    return $ SuspectBulkUploadResp {dlList = map (\suspect -> suspect.dl) suspectAlreadyFlagged, voterIdList = map (\suspect -> suspect.voterId) suspectAlreadyFlagged, message = message}

postChangeFlag :: TokenInfo -> API.Types.UI.Suspect.SuspectFlagStatusChangeReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postChangeFlag tokenInfo req = do
  transaction <- buildTransaction Safety.ChangeFlagRequestEndpoint tokenInfo (encodeToText req)
  T.withTransactionStoring transaction $ do
    suspect <- case (req.dl, req.voterId) of
      (Nothing, Nothing) -> throwError SuspectDlOrVoterIdRequired
      (Just dl, _) -> SQ.findByDl req.dl >>= fromMaybeM (SuspectNotFound dl)
      (_, Just voterId) -> SQ.findByVoterId req.voterId >>= fromMaybeM (SuspectNotFound voterId)
    alreadyRequested <- SQSS.findBySuspectId suspect.id.getId
    whenJust alreadyRequested $ \request -> do
      if request.reqStatus == Domain.Types.SuspectFlagRequest.Pending
        then throwError RequestAlreadyExists
        else throwError FlagRequestAlreadyProcessed
    merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantNotFound tokenInfo.merchantId.getId)
    suspectChangeRequest <- buildSuspectStatusChangeRequest suspect merchant req.reasonToChange
    SQSS.create suspectChangeRequest
    suspectStatusHistory <- buildSuspectStatus tokenInfo merchant.shortId.getShortId Nothing suspect
    SQSH.create suspectStatusHistory
    let notificationMetadata = encodeToText $ [ChangeFlagNotificationMetadata {suspect = suspect, reasonToChange = req.reasonToChange}]
    adminRole <- QRole.findByDashboardAccessType DASHBOARD_ADMIN >>= fromMaybeM (RoleDoesNotExist "DASHBOARD_ADMIN")
    receiversList <- QP.findAllByRole adminRole.id
    mapM_
      ( \receiver -> do
          notification <- buildNotification tokenInfo.merchantId merchant.shortId.getShortId 1 Domain.Types.Notification.CHANGE_REQUEST_PARTNER_ADMIN notificationMetadata receiver.id.getId tokenInfo.personId.getId
          SQN.create notification
      )
      receiversList
    return Kernel.Types.APISuccess.Success

buildSuspectFlagRequest :: API.Types.UI.Suspect.SuspectUploadReq -> TokenInfo -> Text -> Text -> Domain.Types.SuspectFlagRequest.AdminApproval -> Environment.Flow Domain.Types.SuspectFlagRequest.SuspectFlagRequest
buildSuspectFlagRequest req tokenInfo merchantShortId name approval = do
  uid <- generateGUID
  now <- getCurrentTime
  return
    Domain.Types.SuspectFlagRequest.SuspectFlagRequest
      { id = Id uid,
        dl = req.dl,
        voterId = req.voterId,
        adminApproval = approval,
        flaggedStatus = Domain.Types.Suspect.Flagged,
        flaggedReason = req.flaggedReason,
        flaggedBy = name,
        merchantId = Just tokenInfo.merchantId,
        merchantShortId = Just merchantShortId,
        createdAt = now,
        firstName = req.firstName,
        lastName = req.lastName,
        approvedBy = Nothing,
        updatedAt = now,
        flaggedCategory = req.flaggedCategory
      }

buildNotification :: Id Domain.Types.Merchant.Merchant -> Text -> Int -> Domain.Types.Notification.NotificationCategory -> Text -> Text -> Text -> Environment.Flow Domain.Types.Notification.Notification
buildNotification merchantId merchantShortId notificationCount notificationCategory metadata receiverId senderId = do
  uid <- generateGUID
  now <- getCurrentTime
  return
    Domain.Types.Notification.Notification
      { id = Id uid,
        notificationCategory = notificationCategory,
        readStatus = False,
        notificationCount = notificationCount,
        merchantId = Just merchantId,
        merchantShortId = merchantShortId,
        metadata = metadata,
        receiverId = receiverId,
        senderId = senderId,
        createdAt = now,
        updatedAt = now
      }

buildSuspectStatusChangeRequest :: Domain.Types.Suspect.Suspect -> Domain.Types.Merchant.Merchant -> Text -> Environment.Flow Domain.Types.SuspectStatusChangeRequest.SuspectStatusChangeRequest
buildSuspectStatusChangeRequest suspect merchant reasonTochange = do
  uid <- generateGUID
  now <- getCurrentTime
  return
    Domain.Types.SuspectStatusChangeRequest.SuspectStatusChangeRequest
      { id = Id uid,
        suspectId = suspect.id.getId,
        reasonToChange = reasonTochange,
        merchantId = Just merchant.id,
        reqStatus = Domain.Types.SuspectFlagRequest.Pending,
        merchantShortId = merchant.shortId.getShortId,
        createdAt = now,
        updatedAt = now
      }

buildSuspectStatusHistory :: TokenInfo -> Maybe Text -> Maybe Domain.Types.SuspectFlagRequest.AdminApproval -> Domain.Types.Suspect.FlaggedStatus -> Domain.Types.SuspectFlagRequest.SuspectFlagRequest -> Environment.Flow Domain.Types.SuspectStatusHistory.SuspectStatusHistory
buildSuspectStatusHistory tokenInfo approvedBy' approval flaggedStatus' Domain.Types.SuspectFlagRequest.SuspectFlagRequest {..} = do
  now <- getCurrentTime
  id_ <- generateGUID
  return
    Domain.Types.SuspectStatusHistory.SuspectStatusHistory
      { id = id_,
        dl = dl,
        firstName = Just firstName,
        lastName = Just lastName,
        flaggedStatus = flaggedStatus',
        statusChangedReason = Just flaggedReason,
        voterId = voterId,
        flaggedBy = Just [Domain.Types.Suspect.FlaggedBy flaggedCategory merchantShortId],
        createdAt = now,
        updatedAt = now,
        merchantId = Just tokenInfo.merchantId,
        merchantShortId = merchantShortId,
        adminApproval = approval,
        approvedBy = approvedBy'
      }

--buildSuspectStatusBySuspect :: TokenInfo -> Domain.Types.Suspect.Suspect -> Environment.Flow Domain.Types.SuspectStatusHistory.SuspectStatusHistory
buildSuspectStatus :: TokenInfo -> Text -> Maybe Domain.Types.SuspectFlagRequest.AdminApproval -> Domain.Types.Suspect.Suspect -> Environment.Flow Domain.Types.SuspectStatusHistory.SuspectStatusHistory
buildSuspectStatus tokenInfo merchantShortId approval Domain.Types.Suspect.Suspect {..} = do
  now <- getCurrentTime
  id_ <- generateGUID
  return
    Domain.Types.SuspectStatusHistory.SuspectStatusHistory
      { id = id_,
        dl = dl,
        firstName = Just firstName,
        lastName = Just firstName,
        flaggedStatus = flaggedStatus,
        statusChangedReason = statusChangedReason,
        voterId = voterId,
        createdAt = now,
        updatedAt = now,
        merchantId = Just tokenInfo.merchantId,
        merchantShortId = Just merchantShortId,
        flaggedBy = Just flaggedBy,
        adminApproval = approval,
        approvedBy = Nothing
      }
