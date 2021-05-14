module Utils.Servant.SignatureAuth where

import App.Types (AppEnv, FlowHandler)
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import qualified Beckn.Utils.SignatureAuth as HttpSig
import EulerHS.Prelude
import Utils.Common (withFlowHandlerBecknAPI)

withBecknAuthProxy ::
  ToJSON req =>
  (HttpSig.SignaturePayload -> HttpSig.LookupResult lookup -> req -> FlowHandler b) ->
  HttpSig.LookupAction lookup AppEnv ->
  HttpSig.SignaturePayload ->
  req ->
  FlowHandler b
withBecknAuthProxy handler lookupAction sign req = do
  lookupResult <- withFlowHandlerBecknAPI $ HttpSig.verifySignature "Authorization" lookupAction sign req
  handler sign lookupResult req
