{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.UI.Admin where

import API.Types.UI.Admin
import qualified API.Types.UI.Suspect
import qualified "dashboard-helper-api" Dashboard.SafetyPlatform as Safety
import Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text as T hiding (concat, elem, filter, length, map, null)
import qualified Domain.Action.UI.Suspect as DS
import qualified Domain.Action.UI.SuspectFlagRequest as SAF
import Domain.Action.UI.Webhook as Webhook
import qualified Domain.Types.Notification
import qualified Domain.Types.Suspect
import Domain.Types.SuspectFlagRequest
import qualified Domain.Types.Transaction as DT
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (concatMap, elem, filter, id, length, map, mapM_, null, readMaybe, whenJust)
import Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Transaction as T
import qualified "lib-dashboard" Storage.Queries.Merchant as QMerchant
import qualified "lib-dashboard" Storage.Queries.MerchantAccess as QAccess
import qualified Storage.Queries.MerchantConfigs as SQMC
import "lib-dashboard" Storage.Queries.Person as QP
import qualified "lib-dashboard" Storage.Queries.RegistrationToken as QReg
import qualified "lib-dashboard" Storage.Queries.Role as QRole
import qualified Storage.Queries.Suspect as SQ
import qualified Storage.Queries.SuspectExtra as SE
import Storage.Queries.SuspectFlagRequestExtra
import "lib-dashboard" Tools.Auth
import qualified "lib-dashboard" Tools.Auth.Common as Auth
import Tools.Error
import "lib-dashboard" Tools.Error

data AdminCleanSuspect = AdminCleanSuspect
  { id :: Text,
    dl :: Maybe Text,
    voterId :: Maybe Text,
    flaggedStatus :: Domain.Types.Suspect.FlaggedStatus,
    flagUpdatedAt :: UTCTime,
    statusChangedReason :: Maybe Text,
    flaggedCounter :: Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

buildTransaction ::
  ( MonadFlow m
  ) =>
  Safety.SafetyEndpoint ->
  TokenInfo ->
  Text ->
  m DT.Transaction
buildTransaction endpoint tokenInfo = T.buildTransactionForSafetyDashboard (DT.SafetyAPI endpoint) (Just tokenInfo)

postChangeSuspectFlag :: TokenInfo -> API.Types.UI.Admin.SuspectFlagChangeRequestList -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postChangeSuspectFlag tokenInfo req = do
  transaction <- buildTransaction Safety.ChangeSuspectFlagEndpoint tokenInfo (encodeToText req)
  T.withTransactionStoring transaction $ do
    let idList = map Kernel.Types.Id.Id req.ids
    suspectList <- SE.findAllByIds idList
    updatedSuspectList <- mapM (buildUpdateSuspectRequest req) suspectList
    mapM_ SQ.updateByPrimaryKey updatedSuspectList
    let dlList = mapMaybe (\suspect -> suspect.dl) updatedSuspectList
        voterIdList = mapMaybe (\suspect -> suspect.voterId) updatedSuspectList
    mapM_ (\dl -> updateAllWIthDlAndFlaggedStatus dl req.flaggedStatus) dlList
    mapM_ (\voterId -> updateAllWithVoteIdAndFlaggedStatus voterId req.flaggedStatus) voterIdList
    merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantNotFound tokenInfo.merchantId.getId)
    adminIdList <- DS.getRecieverIdListByAcessType DASHBOARD_ADMIN
    merchantAdminIdList <- DS.getMerchantAdminReceiverIdList merchant.id
    DS.sendNotification tokenInfo merchant (encodeToText updatedSuspectList) (length updatedSuspectList) Domain.Types.Notification.ADMIN_CHANGE_SUSPECT_STATUS (adminIdList <> merchantAdminIdList)
    DS.updateSuspectStatusHistoryBySuspect tokenInfo merchant.shortId.getShortId updatedSuspectList (Just Domain.Types.SuspectFlagRequest.Approved)
    webhookBody <- buildAdminSuspectWebhookBody merchant.shortId.getShortId updatedSuspectList
    merchantConfigs <- SQMC.findByRequestWebHook True
    fork "Sending webhook to partners" $ do
      Webhook.sendWebHook merchantConfigs webhookBody
    return Kernel.Types.APISuccess.Success

postAdminUploadSuspectBulk :: TokenInfo -> Maybe Domain.Types.Suspect.FlaggedStatus -> API.Types.UI.Suspect.SuspectBulkUploadReq -> Environment.Flow API.Types.UI.Suspect.SuspectBulkUploadResp
postAdminUploadSuspectBulk tokenInfo mbFlaggedStatus req = do
  transaction <- buildTransaction Safety.UploadBulkSuspectEndpoint tokenInfo (encodeToText req)
  T.withTransactionStoring transaction $ do
    DS.validateUploadCount (length req.suspects)
    (suspectsNeedToFlag, suspectAlreadyFlagged) <- DS.getValidSuspectsToFlagAndAlreadyFlagged tokenInfo.merchantId req.suspects
    case suspectsNeedToFlag.suspects of
      [] -> return $ API.Types.UI.Suspect.SuspectBulkUploadResp {dlList = map (\suspect -> suspect.dl) suspectAlreadyFlagged.suspects, voterIdList = map (\suspect -> suspect.voterId) suspectAlreadyFlagged.suspects, message = DS.getSuspectUploadMessage 0}
      _ -> do
        person <- findById tokenInfo.personId >>= fromMaybeM (PersonNotFound tokenInfo.personId.getId)
        merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantNotFound tokenInfo.merchantId.getId)
        let flaggedBy = person.firstName <> " " <> person.lastName
        suspectFlagRequest <- DS.createSuspectFlagRequest tokenInfo suspectsNeedToFlag merchant.shortId.getShortId flaggedBy Domain.Types.SuspectFlagRequest.Approved
        suspectList <- mapM (\suspect -> SAF.addOrUpdateSuspect suspect merchant.shortId.getShortId (fromMaybe Domain.Types.Suspect.Flagged mbFlaggedStatus)) suspectFlagRequest
        let notificationMetadata = encodeToText $ suspectList
        adminIdList <- DS.getRecieverIdListByAcessType DASHBOARD_ADMIN
        merchantAdminIdList <- DS.getMerchantAdminReceiverIdList merchant.id
        personRole <- QRole.findById person.roleId >>= fromMaybeM (RoleDoesNotExist person.roleId.getId)
        let notificationType = selectNotificationType personRole.name (fromMaybe Domain.Types.Suspect.Flagged mbFlaggedStatus)
        DS.sendNotification tokenInfo merchant notificationMetadata (length suspectList) notificationType (adminIdList <> merchantAdminIdList)
        SAF.sendingWebhookToPartners merchant.shortId.getShortId suspectFlagRequest
        DS.updateSuspectStatusHistoryByRequest tokenInfo Domain.Types.SuspectFlagRequest.Approved flaggedBy (fromMaybe Domain.Types.Suspect.Flagged mbFlaggedStatus) suspectFlagRequest
        return $ API.Types.UI.Suspect.SuspectBulkUploadResp {dlList = map (\suspect -> suspect.dl) suspectAlreadyFlagged.suspects, voterIdList = map (\suspect -> suspect.voterId) suspectAlreadyFlagged.suspects, message = DS.getSuspectUploadMessage (length suspectsNeedToFlag.suspects)}

postMerchantAdminUploadSuspectBulk :: (TokenInfo -> API.Types.UI.Suspect.SuspectBulkUploadReq -> Environment.Flow API.Types.UI.Suspect.SuspectBulkUploadResp)
postMerchantAdminUploadSuspectBulk tokenInfo req = do
  postAdminUploadSuspectBulk tokenInfo (Just Domain.Types.Suspect.Flagged) req

postCheckWebhook :: TokenInfo -> API.Types.UI.Admin.WebhookCheck -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postCheckWebhook _ _ = do
  logDebug $ "Webhook Check Request: checked_ we reached till here"
  return Kernel.Types.APISuccess.Success

postMerchantUserAssignRole :: TokenInfo -> API.Types.UI.Admin.AssignRoleMerchantUserReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postMerchantUserAssignRole tokenInfo req = do
  person <- QP.findByEmail req.email >>= fromMaybeM (PersonNotFound req.email)
  merchantAccess <- QAccess.findByPersonIdAndMerchantIdAndCity person.id tokenInfo.merchantId tokenInfo.city
  case merchantAccess of
    Nothing -> throwError $ InvalidRequest "Server access already denied."
    Just _ -> do
      when (person.id == tokenInfo.personId) $ throwError $ InvalidRequest "Can't change your own role."
      role <- QRole.findByName req.roleName >>= fromMaybeM (RoleDoesNotExist req.roleName)
      QP.updatePersonRole person.id role
      return Kernel.Types.APISuccess.Success

deleteMerchantUserDelete :: TokenInfo -> API.Types.UI.Admin.DeleteMerchantUserReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
deleteMerchantUserDelete tokenInfo req = do
  person <- QP.findByEmail req.email >>= fromMaybeM (PersonNotFound req.email)
  QAccess.deleteAllByMerchantIdAndPersonId tokenInfo.merchantId person.id
  Auth.cleanCachedTokensByMerchantId person.id tokenInfo.merchantId
  QReg.deleteAllByPersonIdAndMerchantId person.id tokenInfo.merchantId
  findMerchantAccess <- QAccess.findAllByPersonId person.id
  when (null findMerchantAccess) $ QP.deletePerson person.id
  return Kernel.Types.APISuccess.Success

buildUpdateSuspectRequest :: API.Types.UI.Admin.SuspectFlagChangeRequestList -> Domain.Types.Suspect.Suspect -> Environment.Flow Domain.Types.Suspect.Suspect
buildUpdateSuspectRequest req Domain.Types.Suspect.Suspect {..} = do
  now <- getCurrentTime
  newFlaggedCounter <- if req.flaggedStatus == Domain.Types.Suspect.NotConfirmed then pure 0 else pure flaggedCounter
  let suspect =
        Domain.Types.Suspect.Suspect
          { flaggedStatus = req.flaggedStatus,
            flagUpdatedAt = now,
            statusChangedReason = req.reasonToChange,
            flaggedCounter = newFlaggedCounter,
            ..
          }
  return suspect

buildAdminSuspectWebhookBody :: Text -> [Domain.Types.Suspect.Suspect] -> Environment.Flow LBS.ByteString
buildAdminSuspectWebhookBody merchantShortId suspectList = do
  let adminSuspectList =
        map
          ( \suspect ->
              SAF.SuspectBody
                { dl = suspect.dl,
                  voterId = suspect.voterId,
                  flaggedStatus = suspect.flaggedStatus,
                  flaggedReason = fromMaybe "Admin" suspect.statusChangedReason,
                  flaggedBy = merchantShortId,
                  flaggedCategory = getFlaggedCategory suspect
                }
          )
          suspectList
  return $ A.encode SAF.WebhookReqBody {suspectList = adminSuspectList}

getFlaggedCategory :: Domain.Types.Suspect.Suspect -> Text
getFlaggedCategory suspect = do
  case suspect.flaggedBy of
    [] -> "Admin" ---- this case will never be hit as we are always passing the flaggedBy
    (fb : _) -> fb.flaggedCategory

selectNotificationType :: Text -> Domain.Types.Suspect.FlaggedStatus -> Domain.Types.Notification.NotificationCategory
selectNotificationType roleName flaggedStatus = do
  case roleName of
    "MERCHANT_ADMIN" -> Domain.Types.Notification.PARTNER_FLAGGED_SUSPECT
    _ -> do
      case flaggedStatus of
        Domain.Types.Suspect.NotConfirmed -> Domain.Types.Notification.ADMIN_CLEAN_SUSPECT
        Domain.Types.Suspect.Confirmed -> Domain.Types.Notification.ADMIN_CHARGED_SUSPECT
        _ -> Domain.Types.Notification.ADMIN_FLAGGED_SUSPECT
