module Product.OnSubscribe where

import App.Types (FlowHandler)
import qualified Beckn.Product.OnSubscribe as Flow
import qualified Beckn.Types.Registry.API as API
import Beckn.Utils.Error (withFlowHandlerAPI)
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import EulerHS.Prelude

onSubscribe :: SignatureAuthResult Text -> API.OnSubscribeRequest -> FlowHandler API.OnSubscribeResponse
onSubscribe (SignatureAuthResult _ registryEncPubKey) = withFlowHandlerAPI . Flow.onSubscribe registryEncPubKey
