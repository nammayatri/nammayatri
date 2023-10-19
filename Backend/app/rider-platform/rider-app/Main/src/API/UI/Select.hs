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
    DSelect.SelectListRes (..),
    DSelect.QuotesResultResponse (..),
    DSelect.CancelAPIResponse (..),
    API,
    select2,
    selectList,
    selectResult,
    cancelSearch,
    handler,
  )
where

import qualified Beckn.ACL.Cancel as CACL
import qualified Beckn.ACL.Select as ACL
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Action.UI.Select as DSelect
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as DPerson
import Environment
-- import qualified Kernel.Storage.Esqueleto as Esq

import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified SharedLogic.CallBPP as CallBPP
import qualified Storage.Queries.Booking as QRB
import Tools.Auth

-------- Select Flow --------
type API =
  "estimate"
    :> ( TokenAuth
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
       )

handler :: FlowServer API
handler =
  select2
    :<|> selectList
    :<|> selectResult
    :<|> cancelSearch

select2 :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> DSelect.DSelectReq -> FlowHandler APISuccess
select2 (personId, _) estimateId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  Redis.whenWithLockRedis (selectEstimateLockKey personId) 60 $ do
    dSelectReq <- DSelect.select personId estimateId req
    becknReq <- ACL.buildSelectReq dSelectReq
    void $ withShortRetry $ CallBPP.select dSelectReq.providerUrl becknReq
  pure Success

selectList :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> FlowHandler DSelect.SelectListRes
selectList (personId, _) = withFlowHandlerAPI . withPersonIdLogTag personId . DSelect.selectList

selectResult :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> FlowHandler DSelect.QuotesResultResponse
selectResult (personId, _) = withFlowHandlerAPI . withPersonIdLogTag personId . DSelect.selectResult

cancelSearch :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> FlowHandler DSelect.CancelAPIResponse
cancelSearch (personId, _) estimateId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  activeBooking <- B.runInReplica $ QRB.findLatestByRiderIdAndStatusObj personId SRB.activeBookingStatusObj
  -- activeBooking <- QRB.findLatestByRiderIdAndStatus personId SRB.activeBookingStatus
  if isJust activeBooking
    then do
      logTagInfo "Booking already created while cancelling estimate." estimateId.getId
      pure DSelect.BookingAlreadyCreated
    else do
      dCancelSearch <- DCancel.mkDomainCancelSearch personId estimateId
      let sendToBpp = dCancelSearch.estimateStatus /= DEstimate.NEW
      result <-
        try @_ @SomeException $
          when sendToBpp . void . withShortRetry $
            CallBPP.cancel dCancelSearch.providerUrl =<< CACL.buildCancelSearchReq dCancelSearch
      case result of
        Left err -> do
          logTagInfo "Failed to cancel" $ show err
          pure DSelect.FailedToCancel
        Right _ -> do
          DCancel.cancelSearch personId dCancelSearch
          pure DSelect.Success

selectEstimateLockKey :: Id DPerson.Person -> Text
selectEstimateLockKey personId = "Customer:SelectEstimate:CustomerId-" <> personId.getId
