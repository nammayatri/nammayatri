module API.UI.SafetySettings
  ( API,
    handler,
  )
where

import Domain.Action.UI.Profile ()
-- HasSafetySettingsHandle instance
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Common (withFlowHandlerAPI)
import qualified Safety.API.UI.SafetySettings as SafetyRoutes
import qualified Safety.Domain.Action.UI.SafetySettings as SafetySettingsLib
import Servant
import Storage.Beam.Sos ()
import Tools.Auth

type API = "profile" :> SafetyRoutes.SafetySettingsAPI TokenAuth

handler :: FlowServer API
handler =
  (withFlowHandlerAPI . SafetySettingsLib.getSafetySettings)
    :<|> (\authTuple req -> withFlowHandlerAPI $ SafetySettingsLib.updateSafetySettings authTuple req)
