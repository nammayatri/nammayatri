{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.UI.SearchSuspect where

import qualified API.Types.UI.Notification as Notification
import API.Types.UI.SearchSuspect
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime)
import Domain.Types.Suspect
import qualified Domain.Types.SuspectFlagRequest
import qualified Domain.Types.SuspectStatusHistory
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (id, length, map, mapM_, readMaybe)
import Kernel.Prelude
import Kernel.Utils.Common
import Storage.Beam.CommonInstances ()
import qualified "lib-dashboard" Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.PortalConfigs as PC
import qualified Storage.Queries.Suspect as SQ
import qualified Storage.Queries.SuspectExtra as SQE
import Storage.Queries.SuspectFlagRequestExtra as SQF
import qualified Storage.Queries.SuspectStatusHistoryExtra as SQSH
import "lib-dashboard" Tools.Auth
import Tools.Auth.Webhook
import Tools.Error

postSearchSuspectList :: TokenInfo -> API.Types.UI.SearchSuspect.SearchSuspectReqList -> Environment.Flow API.Types.UI.SearchSuspect.SuspectsList
postSearchSuspectList _ req = do
  validateSearchCount (length req.suspectReqList)
  let dlList = mapMaybe (\suspect -> suspect.dl) $ req.suspectReqList
      voterIdList = mapMaybe (\suspect -> suspect.voterId) $ req.suspectReqList
  suspectList <- SQE.findAllByDlOrVoterId dlList voterIdList
  let resp = map buildSearchSuspectResp suspectList
      count = length suspectList
  return $ SuspectsList {suspects = resp, summary = Notification.Summary {totalCount = 10000, count = count}}

postCheckSuspectStatusHistory :: TokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> API.Types.UI.SearchSuspect.SearchSuspectReq -> Environment.Flow API.Types.UI.SearchSuspect.CheckSuspectStatusHistoryResp
postCheckSuspectStatusHistory _ mbLimit mbOffset req = do
  case (req.dl, req.voterId) of
    (Nothing, Nothing) -> throwError SuspectDlOrVoterIdRequired
    (Just dl, _) -> do
      suspect <- SQSH.findAllByDlAndAdminApprovalStatus mbLimit mbOffset (Just dl) Domain.Types.SuspectFlagRequest.Approved Nothing
      let resp = map buildCheckSuspectStatusHistoryResp suspect
      return CheckSuspectStatusHistoryResp {suspectStatusHistory = resp}
    _ -> do
      suspect <- SQSH.findAllByDlAndAdminApprovalStatus mbLimit mbOffset req.voterId Domain.Types.SuspectFlagRequest.Approved Nothing
      let resp = map buildCheckSuspectStatusHistoryResp suspect
      return CheckSuspectStatusHistoryResp {suspectStatusHistory = resp}

postPartnerSearchAgent :: AuthToken -> API.Types.UI.SearchSuspect.SearchSuspectReqList -> Environment.Flow API.Types.UI.SearchSuspect.SuspectsList
postPartnerSearchAgent _ req = do
  validateSearchCount (length req.suspectReqList)
  let dlList = mapMaybe (\suspect -> suspect.dl) $ req.suspectReqList
      voterIdList = mapMaybe (\suspect -> suspect.voterId) $ req.suspectReqList
  suspectList <- SQE.findAllByDlOrVoterId dlList voterIdList
  let resp = map buildSearchSuspectResp suspectList
      count = length suspectList
  return $ SuspectsList {suspects = resp, summary = Notification.Summary {totalCount = 10000, count = count}}

getSuspectList :: TokenInfo -> Kernel.Prelude.Maybe Text -> Kernel.Prelude.Maybe Text -> Kernel.Prelude.Maybe (Domain.Types.Suspect.FlaggedStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe Text -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe Text -> Environment.Flow API.Types.UI.SearchSuspect.SuspectsList
getSuspectList _ mbDl mbFlaggedCategory mbFlaggedStatus mbFrom mbLimit mbOffset mbPartnerName mbTo mbVoterId = do
  case (mbDl, mbVoterId) of
    (Just dl, _) -> do
      suspect <- SQ.findByDl (Just dl)
      let resp = case suspect of
            Nothing -> []
            Just suspect' -> [buildSearchSuspectResp suspect']
      return $ SuspectsList {suspects = resp, summary = Notification.Summary {totalCount = 10000, count = 1}}
    (_, Just voterId) -> do
      suspect <- SQ.findByVoterId (Just voterId)
      let resp = case suspect of
            Nothing -> []
            Just suspect' -> [buildSearchSuspectResp suspect']

      return $ SuspectsList {suspects = resp, summary = Notification.Summary {totalCount = 10000, count = 1}}
    _ -> do
      now <- getCurrentTime
      let limit = fromMaybe 10 mbLimit
          offset = fromMaybe 0 mbOffset
          defaultFrom = UTCTime (utctDay now) 0
          from_ = fromMaybe defaultFrom mbFrom
          to = fromMaybe now mbTo
      case (mbFlaggedCategory, mbPartnerName, mbFlaggedStatus) of -- TODO : This query has to be written in a better way
        (Just flaggedCategory, Just partnerName, Just flaggedStatus) -> do
          flagReqList <- SQF.findByFlaggedCategoryAndPartnerNameAndFlaggedStatus flaggedCategory partnerName flaggedStatus limit offset from_ to
          suspectListByFlagRequestList flagReqList
        (Just flaggedCategory, Just partnerName, Nothing) -> do
          flagReqList <- SQF.findByFlaggedCategoryAndPartnerName flaggedCategory partnerName limit offset from_ to
          suspectListByFlagRequestList flagReqList
        (Just flaggedCategory, Nothing, Nothing) -> do
          flagReqList <- SQF.findByFlaggedCategory flaggedCategory limit offset from_ to
          suspectListByFlagRequestList flagReqList
        (Nothing, Just partnerName, Just flaggedStatus) -> do
          flagReqList <- SQF.findByPartnerNameAndFlaggedStatus partnerName flaggedStatus limit offset from_ to
          suspectListByFlagRequestList flagReqList
        (Nothing, Just partnerName, Nothing) -> do
          flagReqList <- SQF.findByPartnerName partnerName limit offset from_ to
          suspectListByFlagRequestList flagReqList
        (Just flaggedCategory, Nothing, Just flaggedStatus) -> do
          flagReqList <- SQF.findByFlaggedCategoryAndFlaggedStatus flaggedCategory flaggedStatus limit offset from_ to
          suspectListByFlagRequestList flagReqList
        (Nothing, Nothing, Just flaggedStatus) -> do
          flagReqList <- SQF.findByFlaggedStatus flaggedStatus limit offset from_ to
          suspectListByFlagRequestList flagReqList
        _ -> do
          suspectList <- SQE.findAll limit offset from_ to
          let count = length suspectList
              resp = map buildSearchSuspectResp suspectList
          return $ SuspectsList {suspects = resp, summary = Notification.Summary {totalCount = 10000, count = count}}

getPartnerSuspectList :: TokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.Flow API.Types.UI.SearchSuspect.SuspectsList
getPartnerSuspectList _ mbFrom mbLimit mbOffset mbTo = do
  now <- getCurrentTime
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
      defaultFrom = UTCTime (utctDay now) 0
      from_ = fromMaybe defaultFrom mbFrom
      to = fromMaybe now mbTo
  suspectList <- SQE.findAll limit offset from_ to
  let count = length suspectList
      resp = map buildSearchSuspectResp suspectList
  return $ SuspectsList {suspects = resp, summary = Notification.Summary {totalCount = 10000, count = count}}

suspectListByFlagRequestList :: [Domain.Types.SuspectFlagRequest.SuspectFlagRequest] -> Environment.Flow API.Types.UI.SearchSuspect.SuspectsList
suspectListByFlagRequestList flagReqList = do
  let dlList = mapMaybe (\flagReq -> flagReq.dl) flagReqList
      voterIdList = mapMaybe (\flagReq -> flagReq.voterId) flagReqList
  suspectList <- SQE.findAllByDlOrVoterId dlList voterIdList
  let count = length suspectList
      resp = map buildSearchSuspectResp suspectList
  return $ SuspectsList {suspects = resp, summary = Notification.Summary {totalCount = 10000, count = count}}

postMerchantCheckSuspectStatusHistory :: TokenInfo -> API.Types.UI.SearchSuspect.SearchSuspectReq -> Environment.Flow API.Types.UI.SearchSuspect.CheckSuspectStatusHistoryResp
postMerchantCheckSuspectStatusHistory tokenInfo req = do
  merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantNotFound tokenInfo.merchantId.getId)
  case (req.dl, req.voterId) of
    (Nothing, Nothing) -> throwError SuspectDlOrVoterIdRequired
    (Just dl, _) -> do
      suspectList <- SQSH.findAllByDlAndAdminApprovalStatus Nothing Nothing (Just dl) Domain.Types.SuspectFlagRequest.Approved $ Just merchant.shortId.getShortId
      let resp = map buildCheckSuspectStatusHistoryResp suspectList
      return CheckSuspectStatusHistoryResp {suspectStatusHistory = resp}
    (_, Just voterId) -> do
      suspectList <- SQSH.findAllByDlAndAdminApprovalStatus Nothing Nothing (Just voterId) Domain.Types.SuspectFlagRequest.Approved $ Just merchant.shortId.getShortId
      let resp = map buildCheckSuspectStatusHistoryResp suspectList
      return CheckSuspectStatusHistoryResp {suspectStatusHistory = resp}

validateSearchCount :: Kernel.Prelude.Int -> Environment.Flow ()
validateSearchCount count = do
  portalConfig <- PC.findByConfigName "BULK_SEARCH_COUNT"
  let bulkSearchCount = case portalConfig of
        Just config -> read (T.unpack config.value) :: Int
        Nothing -> 100
  when (count > bulkSearchCount) $ throwError (BulkSearchLimitExceeded bulkSearchCount)

buildSearchSuspectResp :: Domain.Types.Suspect.Suspect -> API.Types.UI.SearchSuspect.SearchSuspectResp
buildSearchSuspectResp Domain.Types.Suspect.Suspect {..} = do
  API.Types.UI.SearchSuspect.SearchSuspectResp
    { id = id.getId,
      dl = dl,
      voterId = voterId,
      firstName = firstName,
      lastName = lastName,
      flaggedStatus = flaggedStatus,
      flaggedCounter = flaggedCounter,
      statusChangedReason = statusChangedReason,
      flaggedBy = Just flaggedBy,
      createdAt = createdAt,
      updatedAt = updatedAt,
      flagUpdatedAt = flagUpdatedAt
    }

buildCheckSuspectStatusHistoryResp :: Domain.Types.SuspectStatusHistory.SuspectStatusHistory -> API.Types.UI.SearchSuspect.StatusHistory
buildCheckSuspectStatusHistoryResp Domain.Types.SuspectStatusHistory.SuspectStatusHistory {..} = do
  API.Types.UI.SearchSuspect.StatusHistory
    { id = id.getId,
      dl = dl,
      firstName = firstName,
      lastName = lastName,
      voterId = voterId,
      flaggedStatus = flaggedStatus,
      statusChangedReason = statusChangedReason,
      flaggedBy = flaggedBy,
      adminApproval = adminApproval,
      merchantShortId = merchantShortId,
      approvedBy = approvedBy,
      createdAt = createdAt,
      updatedAt = updatedAt
    }
