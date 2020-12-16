module Utils.Servant.SignatureAuth where

import App.Types (AppEnv, FlowHandler)
import Beckn.Utils.Common (withFlowHandler)
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import qualified Beckn.Utils.SignatureAuth as HttpSig
import EulerHS.Prelude

withBecknAuth' :: ToJSON req => (HttpSig.SignaturePayload -> HttpSig.LookupResult lookup -> req -> FlowHandler b) -> HttpSig.LookupAction lookup AppEnv -> HttpSig.SignaturePayload -> req -> FlowHandler b
withBecknAuth' handler lookupAction sign req = do
  lookupResult <- withFlowHandler $ HttpSig.verifySignature "WWW-Authenticate" lookupAction sign req
  handler sign lookupResult req
