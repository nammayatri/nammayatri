{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Select
  ( DSelect.DSelectReq (..),
    DSelect.DSelectRes (..),
    DSelect.DSelectResultRes (..),
    DSelect.SelectListRes (..),
    DSelect.QuotesResultResponse (..),
    DSelect.CancelAPIResponse (..),
    API,
    select2',
    selectList',
    selectResult',
    cancelSearch',
    handler,
  )
where

import qualified Beckn.ACL.Cancel as CACL
import qualified Beckn.ACL.Select as ACL
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Action.UI.Select as DSelect
import Domain.Types.Booking
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as DPerson
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified SharedLogic.CallBPP as CallBPP
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Auth
import Tools.Constants
import Tools.Error

-------- Select Flow --------
type API =
  "estimate"
    :> ( TokenAuth
           :> Capture "estimateId" (Id DEstimate.Estimate)
           :> "select"
           :> ReqBody '[JSON] DSelect.DSelectReq
           :> Post '[JSON] DSelect.DSelectResultRes
           :<|> TokenAuth
             :> Capture "estimateId" (Id DEstimate.Estimate)
             :> "select2"
             :> ReqBody '[JSON] DSelect.DSelectReq
             :> Post '[JSON] APISuccess
           :<|> TokenAuth
             :> Capture "estimateId" (Id DEstimate.Estimate)
             :> "quotes"
             :> Get '[JSON] DSelect.SelectListRes
           :<|> TokenAuth
             :> Capture "estimateId" (Id DEstimate.Estimate)
             :> "results"
             :> Get '[JSON] DSelect.QuotesResultResponse
           :<|> TokenAuth
             :> Capture "estimateId" (Id DEstimate.Estimate)
             :> "cancel"
             :> Post '[JSON] DSelect.CancelAPIResponse
           :<|> TokenAuth
             :> Capture "estimateId" (Id DEstimate.Estimate)
             :> "rejectUpgrade"
             :> Post '[JSON] DSelect.CancelAPIResponse
       )

handler :: FlowServer API
handler =
  select
    :<|> select2
    :<|> selectList
    :<|> selectResult
    :<|> cancelSearch
    :<|> rejectUpgrade

select :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> DSelect.DSelectReq -> FlowHandler DSelect.DSelectResultRes
select (personId, merchantId) estimateId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  Redis.whenWithLockRedis (selectEstimateLockKey personId) 60 $ do
    dSelectReq <- DSelect.select personId estimateId req
    becknReq <- ACL.buildSelectReqV2 dSelectReq
    void $ withShortRetry $ CallBPP.selectV2 dSelectReq.providerUrl becknReq merchantId
  estimate <- QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
  let searchRequestId = estimate.requestId
  searchRequest <- QSearchRequest.findByPersonId personId searchRequestId >>= fromMaybeM (SearchRequestDoesNotExist personId.getId)
  autoAssignEnabled <- searchRequest.autoAssignEnabled & fromMaybeM (InternalError "Invalid autoAssignEnabled")
  bapConfigs <- QBC.findByMerchantIdDomainandMerchantOperatingCityId searchRequest.merchantId "MOBILITY" searchRequest.merchantOperatingCityId
  bapConfig <- listToMaybe bapConfigs & fromMaybeM (InvalidRequest $ "BecknConfig not found for merchantId " <> show searchRequest.merchantId.getId <> " merchantOperatingCityId " <> show searchRequest.merchantOperatingCityId.getId) -- Using findAll for backward compatibility, TODO : Remove findAll and use findOne
  selectTtl <- bapConfig.selectTTLSec & fromMaybeM (InternalError "Invalid ttl")
  ttlInInt <-
    if autoAssignEnabled
      then do
        initTtl <- bapConfig.initTTLSec & fromMaybeM (InternalError "Invalid ttl")
        confirmTtl <- bapConfig.confirmTTLSec & fromMaybeM (InternalError "Invalid ttl")
        confirmBufferTtl <- bapConfig.confirmBufferTTLSec & fromMaybeM (InternalError "Invalid ttl")
        pure (selectTtl + initTtl + confirmTtl + confirmBufferTtl)
      else pure selectTtl
  pure DSelect.DSelectResultRes {selectTtl = ttlInInt}

select2 :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> DSelect.DSelectReq -> FlowHandler APISuccess
select2 (personId, merchantId) estimateId = withFlowHandlerAPI . select2' (personId, merchantId) estimateId

select2' :: DSelect.SelectFlow m r c => (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> DSelect.DSelectReq -> m APISuccess
select2' (personId, merchantId) estimateId req = withPersonIdLogTag personId $ do
  Redis.whenWithLockRedis (selectEstimateLockKey personId) 60 $ do
    dSelectReq <- DSelect.select2 personId estimateId req
    becknReq <- ACL.buildSelectReqV2 dSelectReq
    void $ withShortRetry $ CallBPP.selectV2 dSelectReq.providerUrl becknReq merchantId
  pure Success

selectList :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> FlowHandler DSelect.SelectListRes
selectList (personId, merchantId) = withFlowHandlerAPI . selectList' (personId, merchantId)

selectList' :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> Flow DSelect.SelectListRes
selectList' (personId, _) = withPersonIdLogTag personId . DSelect.selectList

selectResult :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> FlowHandler DSelect.QuotesResultResponse
selectResult (personId, merchantId) = withFlowHandlerAPI . selectResult' (personId, merchantId)

selectResult' :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> Flow DSelect.QuotesResultResponse
selectResult' (personId, _) = withPersonIdLogTag personId . DSelect.selectResult

cancelSearch :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> FlowHandler DSelect.CancelAPIResponse
cancelSearch (personId, merchantId) = withFlowHandlerAPI . cancelSearch' (personId, merchantId)

cancelSearch' :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> Flow DSelect.CancelAPIResponse
cancelSearch' (personId, merchantId) estimateId = withPersonIdLogTag personId $ cancelSearchUtil (personId, merchantId) estimateId

rejectUpgrade :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> FlowHandler DSelect.CancelAPIResponse
rejectUpgrade (personId, merchantId) estimateId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let personTags = fromMaybe [] person.customerNammaTags
  unless (rejectUpgradeTag `elem` personTags) $ QP.updateCustomerTags (Just $ personTags <> [rejectUpgradeTag]) person.id
  cancelSearchUtil (personId, merchantId) estimateId

cancelSearchUtil :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> Flow DSelect.CancelAPIResponse
cancelSearchUtil (personId, merchantId) estimateId = do
  estimate <- QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
  activeBooking <- B.runInReplica $ QRB.findByTransactionIdAndStatus estimate.requestId.getId activeBookingStatus
  if isJust activeBooking
    then do
      logTagInfo "Booking already created while cancelling estimate." estimateId.getId
      pure DSelect.BookingAlreadyCreated
    else do
      dCancelSearch <- DCancel.mkDomainCancelSearch personId estimateId
      result <-
        try @_ @SomeException $
          when dCancelSearch.sendToBpp . void . withShortRetry $ do
            CallBPP.cancelV2 merchantId dCancelSearch.providerUrl =<< CACL.buildCancelSearchReqV2 dCancelSearch
      case result of
        Left err -> do
          logTagInfo "Failed to cancel" $ show err
          pure DSelect.FailedToCancel
        Right _ -> do
          DCancel.cancelSearch personId dCancelSearch
          pure DSelect.Success

selectEstimateLockKey :: Id DPerson.Person -> Text
selectEstimateLockKey personId = "Customer:SelectEstimate:CustomerId-" <> personId.getId
