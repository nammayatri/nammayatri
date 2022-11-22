module API.Beckn.Select (API, handler) where

import Beckn.Prelude
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Taxi.API.Select as Select
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.Select as ACL
import qualified Domain.Action.Beckn.Select as DSelect
import qualified Domain.Types.Merchant as DM
import Environment
import Servant

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth "Authorization"
    :> Select.SelectAPI

handler :: FlowServer API
handler = select

select ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Select.SelectReq ->
  FlowHandler AckResponse
select transporterId (SignatureAuthResult _ subscriber _) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    logTagInfo "Select API Flow" "Reached"
    dSelectReq <- ACL.buildSelectReq subscriber req
    DSelect.handler transporterId dSelectReq
    pure Ack
