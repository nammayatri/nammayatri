module Product.BecknProvider.Select (select) where

import Beckn.Prelude
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Taxi.API.Select as Select
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Core.ACL.Select as ACL
import qualified Domain.Action.Beckn.Select as DSelect
import qualified Domain.Types.Organization as Org
import Environment
import Utils.Common

select ::
  Id Org.Organization ->
  SignatureAuthResult ->
  Select.SelectReq ->
  FlowHandler AckResponse
select transporterId (SignatureAuthResult _ subscriber _) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    logTagInfo "Select API Flow" "Reached"
    dSelectReq <- ACL.buildSelectReq subscriber req

    DSelect.handler transporterId dSelectReq
    pure Ack
