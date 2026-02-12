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
    DSelect.MultimodalSelectRes (..),
    API,
    select2',
    selectList',
    selectResult',
    handler,
  )
where

import qualified Beckn.ACL.Select as ACL
import qualified Domain.Action.UI.Search as DSearch
import qualified Domain.Action.UI.Select as DSelect
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as DPerson
import Environment
import Kernel.External.Slack.Types (SlackConfig)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions)
import Kernel.Utils.Common
import Servant hiding (throwError)
import SharedLogic.BPPFlowRunner (withDirectBPP)
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.DirectBPPCall as DirectBPPCall
import SharedLogic.Cancel
import Storage.Beam.SystemConfigs ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Auth
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
             :> Post '[JSON] DSelect.MultimodalSelectRes
           :<|> TokenAuth
             :> Capture "estimateId" (Id DEstimate.Estimate)
             :> "quotes"
             :> Get '[JSON] DSelect.SelectListRes
           :<|> TokenAuth
             :> Capture "estimateId" (Id DEstimate.Estimate)
             :> "results"
             :> Get '[JSON] DSelect.QuotesResultResponse
       )

handler :: FlowServer API
handler =
  select
    :<|> select2
    :<|> selectList
    :<|> selectResult

select :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> DSelect.DSelectReq -> FlowHandler DSelect.DSelectResultRes
select (personId, merchantId) estimateId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  Redis.whenWithLockRedis (selectEstimateLockKey personId) 60 $ do
    dSelectReq <- DSelect.select personId estimateId req
    becknReq <- ACL.buildSelectReqV2 dSelectReq
    withDirectBPP
      (\rt -> DirectBPPCall.directSelect rt becknReq merchantId)
      (void $ withShortRetry $ CallBPP.selectV2 dSelectReq.providerUrl becknReq merchantId)
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

select2 :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> DSelect.DSelectReq -> FlowHandler DSelect.MultimodalSelectRes
select2 (personId, merchantId) estimateId = withFlowHandlerAPI . select2' (personId, merchantId) estimateId

select2' :: (DSelect.SelectFlow m r c, DSearch.SearchRequestFlow m r, HasFlowEnv m r '["slackCfg" ::: SlackConfig], HasFlowEnv m r '["searchRateLimitOptions" ::: APIRateLimitOptions], HasFlowEnv m r '["searchLimitExceedNotificationTemplate" ::: Text]) => (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> DSelect.DSelectReq -> m DSelect.MultimodalSelectRes
select2' (personId, merchantId) estimateId req = withPersonIdLogTag personId $ do
  -- Note: This `cancelSearch` only handles cancelling the currently selected estimate's previous searches. If any another estimate was selected previously that UI has to ensure to call cancelSearch for that and then call select upon it's success.
  journeyID <- Redis.whenWithLockRedisAndReturnValue (selectEstimateLockKey personId) 60 $ do
    void $ cancelSearchUtil (personId, merchantId) estimateId
    dSelectReq <- DSelect.select2 personId estimateId req
    becknReq <- ACL.buildSelectReqV2 dSelectReq
    withDirectBPP
      (\rt -> DirectBPPCall.directSelect rt becknReq merchantId)
      (void $ withShortRetry $ CallBPP.selectV2 dSelectReq.providerUrl becknReq merchantId)
    let journeyId = dSelectReq.mbJourneyId
    pure journeyId
  case journeyID of
    Right jId -> pure DSelect.MultimodalSelectRes {journeyId = jId, result = "Success"}
    Left _ -> pure DSelect.MultimodalSelectRes {journeyId = Nothing, result = "Success"}

selectList :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> FlowHandler DSelect.SelectListRes
selectList (personId, merchantId) = withFlowHandlerAPI . selectList' (personId, merchantId)

selectList' :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> Flow DSelect.SelectListRes
selectList' (personId, _) = withPersonIdLogTag personId . DSelect.selectList

selectResult :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> FlowHandler DSelect.QuotesResultResponse
selectResult (personId, merchantId) = withFlowHandlerAPI . selectResult' (personId, merchantId)

selectResult' :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> Flow DSelect.QuotesResultResponse
selectResult' (personId, _) = withPersonIdLogTag personId . DSelect.selectResult

selectEstimateLockKey :: Id DPerson.Person -> Text
selectEstimateLockKey personId = "Customer:SelectEstimate:CustomerId-" <> personId.getId
