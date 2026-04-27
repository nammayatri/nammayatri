module Safety.API.UI.SafetySettings
  ( SafetySettingsAPI,
  )
where

import qualified Kernel.Types.APISuccess as APISuccess
import qualified Safety.API.Types.UI.SafetySettings as API
import Servant

-- | Shared route type for both rider and driver safety-settings endpoints.
-- Each app instantiates with its own TokenAuth:
--   type API = "<prefix>" :> SafetySettingsAPI TokenAuth
type SafetySettingsAPI authToken =
  ( authToken :> "getEmergencySettings" :> Get '[JSON] API.GetSafetySettingsRes
      :<|> authToken
        :> "updateEmergencySettings"
        :> ReqBody '[JSON] API.UpdateSafetySettingsReq
        :> Put '[JSON] APISuccess.APISuccess
  )
