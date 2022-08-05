module Product.Update (onUpdate) where

import App.Types
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Taxi.API.OnUpdate as OnUpdate
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Core.ACL.OnUpdate as ACL
import qualified Domain.Action.Beckn.OnUpdate as DOnUpdate
import EulerHS.Prelude hiding (state)
import Utils.Common

onUpdate ::
  SignatureAuthResult ->
  OnUpdate.OnUpdateReq ->
  FlowHandler AckResponse
onUpdate (SignatureAuthResult _ _ registryUrl) req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  mbDOnUpdateReq <- ACL.buildOnUpdateReq req
  whenJust mbDOnUpdateReq (DOnUpdate.onUpdate registryUrl)
  pure Ack
