module API.Beckn.Select (API, handler) where

import qualified Beckn.ACL.Select as ACL
import qualified Beckn.Types.Core.Taxi.API.Select as Select
import qualified Domain.Action.Beckn.Select as DSelect
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
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
    Redis.whenWithLockRedis (selectLockKey dSelectReq.messageId) 60 $
      DSelect.handler transporterId dSelectReq
    pure Ack

selectLockKey :: Text -> Text
selectLockKey id = "Driver:Select:MessageId-" <> id
