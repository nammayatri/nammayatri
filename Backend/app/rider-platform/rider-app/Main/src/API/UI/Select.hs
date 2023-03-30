{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Select
  ( DSelect.DSelectRes (..),
    DSelect.SelectListRes (..),
    DSelect.QuotesResultResponse (..),
    API,
    handler,
  )
where

import qualified Beckn.ACL.Cancel as CACL
import qualified Beckn.ACL.Select as ACL
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Action.UI.Select as DSelect
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Person as DPerson
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
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
           :> "select"
           :> Post '[JSON] APISuccess
           :<|> TokenAuth
             :> Capture "estimateId" (Id DEstimate.Estimate)
             :> "select2" -- TODO will replace "select" once 100% rolled out
             :> ReqBody '[JSON] DSelect.DEstimateSelectReq
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
  select
    :<|> select2
    :<|> selectList
    :<|> selectResult
    :<|> cancelSearch

select :: Id DPerson.Person -> Id DEstimate.Estimate -> FlowHandler APISuccess
select personId estimateId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  let autoAssignFlag = False
  let req = DSelect.DEstimateSelectReq {autoAssignEnabled = autoAssignFlag, autoAssignEnabledV2 = Nothing}
  dSelectReq <- DSelect.select personId estimateId autoAssignFlag autoAssignFlag
  becknReq <- ACL.buildSelectReq dSelectReq req.autoAssignEnabled
  void $ withShortRetry $ CallBPP.select dSelectReq.providerUrl becknReq
  pure Success

select2 :: Id DPerson.Person -> Id DEstimate.Estimate -> DSelect.DEstimateSelectReq -> FlowHandler APISuccess
select2 personId estimateId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  let autoAssignV2Flag = fromMaybe False req.autoAssignEnabledV2
  dSelectReq <- DSelect.select personId estimateId req.autoAssignEnabled autoAssignV2Flag
  becknReq <- ACL.buildSelectReq dSelectReq req.autoAssignEnabled
  void $ withShortRetry $ CallBPP.select dSelectReq.providerUrl becknReq
  pure Success

selectList :: Id DPerson.Person -> Id DEstimate.Estimate -> FlowHandler DSelect.SelectListRes
selectList personId = withFlowHandlerAPI . withPersonIdLogTag personId . DSelect.selectList

selectResult :: Id DPerson.Person -> Id DEstimate.Estimate -> FlowHandler DSelect.QuotesResultResponse
selectResult personId = withFlowHandlerAPI . withPersonIdLogTag personId . DSelect.selectResult

cancelSearch :: Id DPerson.Person -> Id DEstimate.Estimate -> FlowHandler DSelect.CancelAPIResponse
cancelSearch personId estimateId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  activeBooking <- Esq.runInReplica $ QRB.findLatestByRiderIdAndStatus personId SRB.activeBookingStatus
  if isJust activeBooking
    then pure DSelect.BookingAlreadyCreated
    else do
      dCancelSearch <- DCancel.mkDomainCancelSearch personId estimateId
      let sendToBpp = dCancelSearch.estimateStatus /= Just DEstimate.NEW
      result <-
        try @_ @SomeException $
          when sendToBpp . void . withShortRetry $
            CallBPP.cancel dCancelSearch.providerUrl =<< CACL.buildCancelSearchReq dCancelSearch
      case result of
        Left err -> do
          logTagInfo "THishis" $ show err
          pure DSelect.FailedToCancel
        Right _ -> do
          DCancel.cancelSearch personId dCancelSearch
          pure DSelect.Success
