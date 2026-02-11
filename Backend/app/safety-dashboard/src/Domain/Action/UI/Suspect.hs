{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.UI.Suspect where

import API.Types.UI.Suspect
import qualified "dashboard-helper-api" Dashboard.SafetyPlatform as Safety
import qualified Data.List as DL
import Data.Text as T hiding (concat, elem, filter, length, map, null)
import qualified "lib-dashboard" Domain.Types.Merchant as Merchant
import qualified Domain.Types.Notification
import qualified "lib-dashboard" Domain.Types.Person
import Domain.Types.Suspect
import Domain.Types.SuspectFlagRequest
import qualified Domain.Types.SuspectStatusChangeRequest
import qualified Domain.Types.SuspectStatusHistory
import qualified Domain.Types.Transaction as DT
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (concatMap, elem, filter, groupBy, id, length, map, mapM_, maximumBy, null, readMaybe, whenJust)
import Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import qualified "lib-dashboard" Storage.CachedQueries.Role as CQRole
import qualified "lib-dashboard" Storage.Queries.Merchant as QMerchant
import qualified "lib-dashboard" Storage.Queries.MerchantAccess as QMerchantAccess
import qualified Storage.Queries.Notification as SQN
import "lib-dashboard" Storage.Queries.Person as QP
import qualified Storage.Queries.PortalConfigs as PC
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
buildTransaction endpoint tokenInfo = T.buildTransactionForSafetyDashboard (DT.SafetyAPI endpoint) (Just tokenInfo)

data ChangeFlagNotificationMetadata = ChangeFlagNotificationMetadata
  { suspect :: Domain.Types.Suspect.Suspect,
    reasonToChange :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

postUploadSuspectBulk :: TokenInfo -> API.Types.UI.Suspect.SuspectBulkUploadReq -> Environment.Flow API.Types.UI.Suspect.SuspectBulkUploadResp
postUploadSuspectBulk tokenInfo req = do
  transaction <- buildTransaction Safety.UploadBulkSuspectEndpoint tokenInfo (encodeToText req)
  T.withTransactionStoring transaction $ do
    validateUploadCount (length req.suspects)
    (suspectsNeedToFlag, suspectAlreadyFlagged) <- getValidSuspectsToFlagAndAlreadyFlagged tokenInfo.merchantId req.suspects
    case suspectsNeedToFlag.suspects of
      [] -> return $ SuspectBulkUploadResp {dlList = map (\suspect -> suspect.dl) suspectAlreadyFlagged.suspects, voterIdList = map (\suspect -> suspect.voterId) suspectAlreadyFlagged.suspects, message = getSuspectUploadMessage 0}
      _ -> do
        person <- findById tokenInfo.personId >>= fromMaybeM (PersonNotFound tokenInfo.personId.getId)
        merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantNotFound tokenInfo.merchantId.getId)
        let flaggedBy = person.firstName <> " " <> person.lastName
        suspectFlagRequest <- createSuspectFlagRequest tokenInfo suspectsNeedToFlag merchant.shortId.getShortId flaggedBy Domain.Types.SuspectFlagRequest.Pending
        updateSuspectStatusHistoryByRequest tokenInfo Domain.Types.SuspectFlagRequest.Pending flaggedBy Domain.Types.Suspect.Flagged suspectFlagRequest
        let notificationMetadata = encodeToText $ suspectFlagRequest
        merchantAdminIdList <- getAllMerchantAdminIdList tokenInfo.merchantId
        sendNotification tokenInfo merchant notificationMetadata (length suspectFlagRequest) Domain.Types.Notification.FLAG_REQUEST_UPLOAD merchantAdminIdList
        return $ SuspectBulkUploadResp {dlList = map (\suspect -> suspect.dl) suspectAlreadyFlagged.suspects, voterIdList = map (\suspect -> suspect.voterId) suspectAlreadyFlagged.suspects, message = getSuspectUploadMessage (length suspectsNeedToFlag.suspects)}

createSuspectFlagRequest :: TokenInfo -> API.Types.UI.Suspect.SuspectBulkUploadReq -> Text -> Text -> Domain.Types.SuspectFlagRequest.AdminApproval -> Environment.Flow [Domain.Types.SuspectFlagRequest.SuspectFlagRequest]
createSuspectFlagRequest tokenInfo req merchantShortId flaggedBy approval = do
  suspectFlagRequest <- mapM (\suspect -> buildSuspectFlagRequest suspect tokenInfo merchantShortId flaggedBy approval) req.suspects
  SQF.createMany suspectFlagRequest
  return suspectFlagRequest

getSuspectUploadMessage :: Int -> Text
getSuspectUploadMessage cnt = case cnt of
  0 -> "All DLs or VoterIds are already flagged before"
  _ -> "Dl and VoterId List provided who were already Flagged before"

getValidSuspectsToFlagAndAlreadyFlagged :: Id Merchant.Merchant -> [API.Types.UI.Suspect.SuspectUploadReq] -> Environment.Flow (API.Types.UI.Suspect.SuspectBulkUploadReq, API.Types.UI.Suspect.SuspectBulkUploadReq)
getValidSuspectsToFlagAndAlreadyFlagged merchantId suspects = do
  let validSuspects = filter (\suspect -> isJust (suspect.dl) || isJust (suspect.voterId)) suspects
      dlList = mapMaybe (.dl) validSuspects
      voterIdList = mapMaybe (.voterId) validSuspects
  suspectAlreadyFlaggedAndApproved <- findAllByDlAndVoterIdAndMerchantIdAndAdminApproval dlList voterIdList (Just merchantId) Domain.Types.SuspectFlagRequest.Approved
  suspectAlreadyFlaggedAndPending <- findAllByDlAndVoterIdAndMerchantIdAndAdminApproval dlList voterIdList (Just merchantId) Domain.Types.SuspectFlagRequest.Pending
  let latestApprovedRequest = filterLatestApprovedSuspectFlagRequest suspectAlreadyFlaggedAndApproved
  let dlListForAlreadyPending = mapMaybe (.dl) suspectAlreadyFlaggedAndPending
      voterIdListForAlreadyPending = mapMaybe (.voterId) suspectAlreadyFlaggedAndPending
      onlyApproved = filter (\suspect -> not ((isSearchParamPresent suspect.dl dlListForAlreadyPending) || (isSearchParamPresent suspect.voterId voterIdListForAlreadyPending))) latestApprovedRequest
      onlyApprovedDlList = mapMaybe (.dl) onlyApproved
      onlyApprovedVoterIdList = mapMaybe (.voterId) onlyApproved
  let validSuspectAlreadyApproved = filter (\suspect -> (isSearchParamPresent suspect.dl onlyApprovedDlList) || (isSearchParamPresent suspect.voterId onlyApprovedVoterIdList)) validSuspects
  suspectCleanedByAdmin <- findAllByDlOrVoterIdAndFlaggedStatus onlyApprovedDlList onlyApprovedVoterIdList Domain.Types.Suspect.NotConfirmed
  let dlListForCleaned = mapMaybe (.dl) suspectCleanedByAdmin
      voterIdListForCleaned = mapMaybe (.voterId) suspectCleanedByAdmin
      suspectNeedToFlagAgain = filter (\suspect -> (isSearchParamPresent suspect.dl dlListForCleaned) || (isSearchParamPresent suspect.voterId voterIdListForCleaned)) validSuspectAlreadyApproved
      newSuspect = filter (\suspect -> not ((isSearchParamPresent suspect.dl (dlListForAlreadyPending <> onlyApprovedDlList)) || (isSearchParamPresent suspect.voterId (voterIdListForAlreadyPending <> onlyApprovedVoterIdList)))) validSuspects
      suspectNeedToFlagged = API.Types.UI.Suspect.SuspectBulkUploadReq {suspects = newSuspect <> suspectNeedToFlagAgain}
      dlListForNeedToFlag = mapMaybe (.dl) $ newSuspect <> suspectNeedToFlagAgain
      voterIdListForNeedToFlag = mapMaybe (.voterId) $ newSuspect <> suspectNeedToFlagAgain
      alreadyFlagged = filter (\suspect -> not (isSearchParamPresent suspect.dl dlListForNeedToFlag || isSearchParamPresent suspect.voterId voterIdListForNeedToFlag)) validSuspects
      suspectAlreadyFlagged = API.Types.UI.Suspect.SuspectBulkUploadReq {suspects = alreadyFlagged}
  return (suspectNeedToFlagged, suspectAlreadyFlagged)

isSearchParamPresent :: Maybe Text -> [Text] -> Bool
isSearchParamPresent (Just param) list = param `elem` list
isSearchParamPresent Nothing _ = False

filterLatestApprovedSuspectFlagRequest :: [Domain.Types.SuspectFlagRequest.SuspectFlagRequest] -> [Domain.Types.SuspectFlagRequest.SuspectFlagRequest]
filterLatestApprovedSuspectFlagRequest suspects =
  let groupedSuspects = groupByDlAndVoterId suspects
   in map (DL.maximumBy (comparing Domain.Types.SuspectFlagRequest.createdAt)) groupedSuspects

groupByDlAndVoterId :: [Domain.Types.SuspectFlagRequest.SuspectFlagRequest] -> [[Domain.Types.SuspectFlagRequest.SuspectFlagRequest]]
groupByDlAndVoterId = DL.groupBy isEqual
  where
    isEqual :: Domain.Types.SuspectFlagRequest.SuspectFlagRequest -> Domain.Types.SuspectFlagRequest.SuspectFlagRequest -> Bool
    isEqual x y = maybeEqual x.dl y.dl || maybeEqual x.voterId y.voterId

    maybeEqual :: Maybe Text -> Maybe Text -> Bool
    maybeEqual (Just a) (Just b) = a == b
    maybeEqual _ _ = False

validateUploadCount :: Int -> Environment.Flow ()
validateUploadCount cnt = do
  portalConfig <- PC.findByConfigName "BULK_UPLOAD_COUNT"
  let bulkUploadCount = case portalConfig of
        Just config -> read (T.unpack config.value) :: Int
        Nothing -> 100
  when (cnt > bulkUploadCount) $ throwError (BulkUploadLimitExceeded bulkUploadCount)

postChangeFlag :: TokenInfo -> API.Types.UI.Suspect.SuspectFlagStatusChangeReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postChangeFlag tokenInfo req = do
  transaction <- buildTransaction Safety.ChangeFlagRequestEndpoint tokenInfo (encodeToText req)
  T.withTransactionStoring transaction $ do
    suspect <- findSuspect req.dl req.voterId >>= fromMaybeM SuspectNotFound
    validateChangeRequest suspect.id.getId
    merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantNotFound tokenInfo.merchantId.getId)
    createNewChangeRequest suspect merchant req.reasonToChange
    updateSuspectStatusHistoryBySuspect tokenInfo merchant.shortId.getShortId [suspect] Nothing
    let notificationMetadata = encodeToText [ChangeFlagNotificationMetadata {suspect = suspect, reasonToChange = req.reasonToChange}]
    receiverIds <- getRecieverIdListByAcessType DASHBOARD_ADMIN
    sendNotification tokenInfo merchant notificationMetadata 1 Domain.Types.Notification.CHANGE_REQUEST_PARTNER_ADMIN receiverIds
    return Kernel.Types.APISuccess.Success

validateChangeRequest :: Text -> Environment.Flow ()
validateChangeRequest suspectId = do
  alreadyRequested <- SQSS.findBySuspectId suspectId
  whenJust alreadyRequested $ \request -> do
    if request.reqStatus == Domain.Types.SuspectFlagRequest.Pending
      then throwError RequestAlreadyExists
      else throwError FlagRequestAlreadyProcessed

getRecieverIdListByAcessType :: DashboardAccessType -> Environment.Flow [Id Domain.Types.Person.Person]
getRecieverIdListByAcessType acessName = do
  role <- CQRole.findByDashboardAccessType acessName >>= fromMaybeM (RoleDoesNotExist (show acessName))
  receiversList <- QP.findAllByRoleAndReciveNotification role.id
  return $ map (\receiver -> receiver.id) receiversList

getMerchantAdminReceiverIdList :: Id Merchant.Merchant -> Environment.Flow [Id Domain.Types.Person.Person]
getMerchantAdminReceiverIdList merchanId = do
  portalConfig <- PC.findByConfigName "SEND_MERCHANT_NOTIFICATION"
  let notifyMerchant = case portalConfig of
        Just config -> read (T.unpack config.value) :: Bool
        Nothing -> False
  if notifyMerchant then getRecieverIdListByAcessType MERCHANT_ADMIN else getAllMerchantAdminIdList merchanId

getAllMerchantAdminIdList :: Id Merchant.Merchant -> Environment.Flow [Id Domain.Types.Person.Person]
getAllMerchantAdminIdList merchantId = do
  adminRole <- CQRole.findByDashboardAccessType MERCHANT_ADMIN >>= fromMaybeM (RoleDoesNotExist "MERCHANT_ADMIN")
  allMerchantUsers <- QMerchantAccess.findAllUserAccountForMerchant merchantId
  let personIds = map (.personId) allMerchantUsers
  merchantAdminList <- QP.findAllByIdRoleIdAndReceiveNotification personIds adminRole.id
  return $ map (\receiver -> receiver.id) merchantAdminList

findSuspect :: Maybe Text -> Maybe Text -> Environment.Flow (Maybe Domain.Types.Suspect.Suspect)
findSuspect mbDl mbVoterId = case (mbDl, mbVoterId) of
  (Nothing, Nothing) -> throwError SuspectDlOrVoterIdRequired
  (Just _, _) -> SQ.findByDl mbDl
  _ -> SQ.findByVoterId mbVoterId

sendNotification :: TokenInfo -> Merchant.Merchant -> Text -> Int -> Domain.Types.Notification.NotificationCategory -> [Id Domain.Types.Person.Person] -> Environment.Flow ()
sendNotification tokenInfo merchant notificationMetadata lengthOfItems notificationType receiverList = do
  mapM_
    ( \receiver -> do
        notification <- buildNotification tokenInfo.merchantId merchant.shortId.getShortId lengthOfItems notificationType notificationMetadata receiver.getId tokenInfo.personId.getId
        SQN.create notification
    )
    receiverList

createNewChangeRequest :: Domain.Types.Suspect.Suspect -> Merchant.Merchant -> Text -> Environment.Flow ()
createNewChangeRequest suspect merchant reasonToChange = do
  suspectChangeRequest <- buildSuspectStatusChangeRequest suspect merchant reasonToChange
  SQSS.create suspectChangeRequest

updateSuspectStatusHistoryBySuspect :: TokenInfo -> Text -> [Domain.Types.Suspect.Suspect] -> Maybe Domain.Types.SuspectFlagRequest.AdminApproval -> Environment.Flow ()
updateSuspectStatusHistoryBySuspect tokenInfo merchantShortId suspectList adminApproval = do
  mapM_
    ( \suspect -> do
        suspectStatusHistory <- buildSuspectStatus tokenInfo merchantShortId adminApproval suspect
        SQSH.create suspectStatusHistory
    )
    suspectList

updateSuspectStatusHistoryByRequest :: TokenInfo -> Domain.Types.SuspectFlagRequest.AdminApproval -> Text -> Domain.Types.Suspect.FlaggedStatus -> [Domain.Types.SuspectFlagRequest.SuspectFlagRequest] -> Environment.Flow ()
updateSuspectStatusHistoryByRequest tokenInfo adminApproval approvedBy flaggedStatus flagReqList = do
  mapM_
    ( \flagReq -> do
        suspectStatusHistory <- buildSuspectStatusHistory tokenInfo (Just approvedBy) (Just adminApproval) flaggedStatus flagReq
        SQSH.create suspectStatusHistory
    )
    flagReqList

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
        flaggedCategory = req.flaggedCategory,
        mobileNumber = req.mobileNumber,
        reportDetails = req.reportDetails,
        totalComplaintsCount = req.totalComplaintsCount
      }

buildNotification :: Id Merchant.Merchant -> Text -> Int -> Domain.Types.Notification.NotificationCategory -> Text -> Text -> Text -> Environment.Flow Domain.Types.Notification.Notification
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

buildSuspectStatusChangeRequest :: Domain.Types.Suspect.Suspect -> Merchant.Merchant -> Text -> Environment.Flow Domain.Types.SuspectStatusChangeRequest.SuspectStatusChangeRequest
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
        flaggedBy = Just [Domain.Types.Suspect.FlaggedBy {flaggedCategory = flaggedCategory, partnerName = merchantShortId, flaggedReason = Just flaggedReason, agentContactNumber = mobileNumber, reportDetails = reportDetails, totalComplaintsCount = totalComplaintsCount}],
        createdAt = now,
        updatedAt = now,
        merchantId = Just tokenInfo.merchantId,
        merchantShortId = merchantShortId,
        adminApproval = approval,
        approvedBy = approvedBy'
      }

buildSuspectStatus :: TokenInfo -> Text -> Maybe Domain.Types.SuspectFlagRequest.AdminApproval -> Domain.Types.Suspect.Suspect -> Environment.Flow Domain.Types.SuspectStatusHistory.SuspectStatusHistory
buildSuspectStatus tokenInfo merchantShortId approval Domain.Types.Suspect.Suspect {..} = do
  now <- getCurrentTime
  id_ <- generateGUID
  return
    Domain.Types.SuspectStatusHistory.SuspectStatusHistory
      { id = id_,
        dl = dl,
        firstName = Just firstName,
        lastName = Just lastName,
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
