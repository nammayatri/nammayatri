module Product.BecknProvider.Confirm where

import App.Types
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.Confirm as Confirm
import qualified Beckn.Types.Core.Taxi.API.OnConfirm as OnConfirm
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Core.ACL.Confirm as ACL
import qualified Core.ACL.OnConfirm as ACL
import qualified Domain.Action.Beckn.Confirm as DConfirm
import qualified Domain.Types.Organization as Organization
import EulerHS.Prelude hiding (id)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified SharedLogic.Transporter as Shared
import Utils.Common

confirm ::
  Id Organization.Organization ->
  SignatureAuthResult ->
  Confirm.ConfirmReq ->
  FlowHandler AckResponse
confirm transporterId (SignatureAuthResult _ subscriber) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    logTagInfo "confirm API Flow" "Reached"
    dConfirmReq <- ACL.buildConfirmReq subscriber req
    transporter <- Shared.findTransporter transporterId
    let context = req.context
    let callbackUrl = context.bap_uri
    ExternalAPI.withCallback' withRetry transporter Context.CONFIRM OnConfirm.onConfirmAPI context callbackUrl $ do
      dOnConfirmReq <- DConfirm.handler transporter dConfirmReq
      pure $ ACL.makeOnConfirmReq dOnConfirmReq
