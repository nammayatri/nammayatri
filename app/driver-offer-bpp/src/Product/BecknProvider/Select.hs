module Product.BecknProvider.Select (select) where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Queries.BecknRequest as QBR
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Taxi.API.Select as Select
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Core.ACL.Select as ACL
import Data.Aeson (encode)
import qualified Domain.Action.Beckn.Select as DSelect
import qualified Domain.Types.Organization as Org
import Environment
import Utils.Common

select ::
  Id Org.Organization ->
  SignatureAuthResult ->
  Select.SelectReq ->
  FlowHandler AckResponse
select transporterId (SignatureAuthResult signPayload subscriber) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    logTagInfo "Select API Flow" "Reached"
    Esq.runTransaction $
      QBR.logBecknRequest (show $ encode req) (show $ signPayload.signature)

    dSelectReq <- ACL.buildSelectReq subscriber req

    DSelect.handler transporterId dSelectReq
    pure Ack
