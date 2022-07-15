module Product.BecknProvider.Confirm (confirm) where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Queries.BecknRequest as QBR
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Taxi.API.Confirm as Confirm
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Core.ACL.Confirm as ACL
import qualified Core.ACL.OnConfirm as ACL
import Data.Aeson (encode)
import qualified Domain.Action.Beckn.Confirm as DConfirm
import qualified Domain.Types.Organization as Org
import Environment
import ExternalAPI.Flow as ExternalAPI
import qualified Product.BecknProvider.BP as BP
import Utils.Common

confirm ::
  Id Org.Organization ->
  SignatureAuthResult ->
  Confirm.ConfirmReq ->
  FlowHandler AckResponse
confirm transporterId (SignatureAuthResult signPayload subscriber) req =
  withFlowHandlerAPI . withTransactionIdLogTag req $ do
    logTagInfo "Confirm API Flow" "Reached"
    Esq.runTransaction $
      QBR.logBecknRequest (show $ encode req) (show $ signPayload.signature)

    dConfirmReq <- ACL.buildConfirmReq req
    let context = req.context
    dConfirmRes <- DConfirm.handler subscriber transporterId dConfirmReq
    now <- getCurrentTime
    fork "on_confirm/on_update" $ do
      ExternalAPI.callOnConfirm dConfirmRes.transporter context $ ACL.mkOnConfirmMessage now dConfirmRes
      BP.sendRideAssignedUpdateToBAP dConfirmRes.booking dConfirmRes.ride
    -- FIXME: we might try to send these two events in one request
    pure Ack
