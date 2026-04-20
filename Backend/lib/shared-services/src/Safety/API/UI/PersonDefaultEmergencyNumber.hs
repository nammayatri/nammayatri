module Safety.API.UI.PersonDefaultEmergencyNumber
  ( PersonDefaultEmergencyNumberAPI,
  )
where

import qualified Kernel.Types.APISuccess as APISuccess
import qualified Safety.API.Types.UI.PersonDefaultEmergencyNumber as API
import Servant

-- | Shared Servant route type.  Each app instantiates with its own auth token
-- and URL prefix:
--
--   Driver:  type API = "driver"  :> PersonDefaultEmergencyNumberAPI TokenAuth
--   Rider:   type API = "profile" :> PersonDefaultEmergencyNumberAPI TokenAuth
type PersonDefaultEmergencyNumberAPI authToken =
  ( authToken :> "defaultEmergencyNumbers" :> Get '[JSON] API.GetEmergencyContactsRes
      :<|> authToken
        :> "defaultEmergencyNumbers"
        :> ReqBody '[JSON] API.UpdateEmergencyContactsReq
        :> Post '[JSON] APISuccess.APISuccess
  )
