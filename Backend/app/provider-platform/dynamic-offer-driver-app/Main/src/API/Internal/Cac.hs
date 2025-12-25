module API.Internal.Cac
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.Cac as Domain
import Environment (FlowHandler, FlowServer)
import GHC.Base
import Kernel.Prelude
import Kernel.Utils.Common (withFlowHandlerAPI)
import Kernel.Utils.Error.Throwing (fromMaybeM, throwError)
import Servant hiding (throwError)
import System.Environment (lookupEnv)
import Tools.Error (CacAuthError (..))

type API =
  "typeCheck"
    :> Header "Authorization" String
    :> ReqBody '[JSON] Domain.CacTypeValidationReq
    :> Post '[JSON] Domain.CacTypeValidationResp

handler :: FlowServer API
handler = typeCheckHandler

typeCheckHandler :: Maybe String -> Domain.CacTypeValidationReq -> FlowHandler Domain.CacTypeValidationResp
typeCheckHandler token req = withFlowHandlerAPI $ do
  token' <- fromMaybeM CacAuthError token
  authtkn <- liftIO $ lookupEnv "AUTH_TOKEN"
  let tokenEnv = fromMaybe "Sample" authtkn
      mbAcl = eqString token' tokenEnv
  unless mbAcl $ throwError CacInvalidToken
  Domain.typeCheckHandler req
