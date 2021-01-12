module Utils.Servant.SignatureAuth where

import App.Types (AppEnv, FlowHandler)
import Beckn.Utils.Common (withFlowHandler)
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import qualified Beckn.Utils.SignatureAuth as HttpSig
import EulerHS.Prelude

withBecknAuthProxy ::
  ToJSON req =>
  (HttpSig.SignaturePayload -> HttpSig.LookupResult lookup -> req -> FlowHandler b) ->
  HttpSig.LookupAction lookup AppEnv ->
  HttpSig.SignaturePayload ->
  req ->
  FlowHandler b
withBecknAuthProxy handler lookupAction sign req = do
  lookupResult <- withFlowHandler $ HttpSig.verifySignature "Authorization" lookupAction sign req
  handler sign lookupResult req
