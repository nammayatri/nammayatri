module ConfigPilotFrontend.API where

import ConfigPilotFrontend.Types
import qualified EulerHS.Types as ET
import Kernel.Types.Common
import Servant

type TSServiceValidateAPI =
  "provider"
    :> "config"
    :> "validate"
    :> ReqBody '[JSON] Value
    :> Post '[JSON] TSServiceValidateResp

tsServiceValidateAPI :: Proxy TSServiceValidateAPI
tsServiceValidateAPI = Proxy

tsServiceValidate :: Value -> ET.EulerClient TSServiceValidateResp
tsServiceValidate = ET.client tsServiceValidateAPI
