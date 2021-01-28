module Product.Call where

import App.Types
import Beckn.Types.Core.API.Call
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import EulerHS.Prelude
import External.Gateway.Flow as Gateway

initiateCall :: SR.RegistrationToken -> CallReq -> FlowHandler CallRes
initiateCall _ = withFlowHandler . Gateway.initiateCall
