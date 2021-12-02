module Product.Auth where

import App.Types
import Beckn.InternalAPI.Auth.API
import Beckn.Utils.Common
import EulerHS.Prelude
import Utils.Auth

authAPI :: Token -> FlowHandler PersonId
authAPI token = withFlowHandlerAPI do
  verifyPerson token <&> (.getId)
