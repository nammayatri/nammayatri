module API.Dashboard.InternalAuth where

import qualified Data.ByteArray as BA
import qualified Data.Text.Encoding as TE
import qualified Domain.Action.Dashboard.InternalAuth as DInternalAuth
import Environment
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant hiding (throwError)
import Storage.Beam.BeamFlow

type API =
  "internal"
    :> "auth"
    :> Header "api-key" Text
    :> ReqBody '[JSON] DInternalAuth.InternalAuthReq
    :> Post '[JSON] DInternalAuth.InternalAuthResp

handler :: BeamFlow' => FlowServer API
handler = internalAuth

internalAuth :: BeamFlow' => Maybe Text -> DInternalAuth.InternalAuthReq -> FlowHandler DInternalAuth.InternalAuthResp
internalAuth apiKey req = withFlowHandlerAPI' $ do
  expected <- asks (.internalAuthAPIKey)
  -- constEq is timing-safe; plain `==` on Text would short-circuit.
  let provided = TE.encodeUtf8 (fromMaybe "" apiKey)
      expectedBytes = TE.encodeUtf8 expected
  unless (BA.constEq provided expectedBytes) $
    throwError (InvalidRequest "Invalid API key")
  DInternalAuth.internalAuth req
