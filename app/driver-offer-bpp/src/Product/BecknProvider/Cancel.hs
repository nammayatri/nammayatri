module Product.BecknProvider.Cancel where

import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Queries.BecknRequest as QBR
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Taxi.API.Cancel as Cancel
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Core.ACL.Cancel as ACL
import Data.Aeson (encode)
import qualified Domain.Action.Beckn.Cancel as DCancel
import qualified Domain.Types.Organization as Organization
import Environment
import EulerHS.Prelude
import Utils.Common

cancel ::
  Id Organization.Organization ->
  SignatureAuthResult ->
  Cancel.CancelReq ->
  FlowHandler AckResponse
cancel transporterId subscriber@(SignatureAuthResult signPayload _) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    logTagInfo "Cancel API Flow" "Reached"
    Esq.runTransaction $ do
      QBR.logBecknRequest (show $ encode req) (show $ signPayload.signature)
    dConfirmReq <- ACL.buildCancelReq req
    DCancel.cancel transporterId subscriber dConfirmReq
    return Ack
