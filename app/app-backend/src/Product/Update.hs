module Product.Update (onUpdate) where

import App.Types
import qualified Beckn.Storage.Esqueleto as DB
import qualified Beckn.Storage.Queries.BecknRequest as QBR
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Taxi.API.OnUpdate as OnUpdate
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Core.ACL.OnUpdate as ACL
import Data.Aeson (encode)
import qualified Domain.Action.Beckn.OnUpdate as DOnUpdate
import EulerHS.Prelude hiding (state)
import Utils.Common

onUpdate ::
  SignatureAuthResult ->
  OnUpdate.OnUpdateReq ->
  FlowHandler AckResponse
onUpdate (SignatureAuthResult signPayload _) req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  DB.runTransaction $
    QBR.logBecknRequest (show $ encode req) (show $ signPayload.signature)
  mbDOnUpdateReq <- ACL.buildOnUpdateReq req
  whenJust mbDOnUpdateReq DOnUpdate.onUpdate
  pure Ack
