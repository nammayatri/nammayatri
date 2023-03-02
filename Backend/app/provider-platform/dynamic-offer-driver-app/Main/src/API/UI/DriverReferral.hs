module API.UI.DriverReferral where

import qualified Domain.Action.UI.DriverReferral as Domain
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "driver"
    :> ( "linkReferralCode"
           :> TokenAuth
           :> ReqBody '[JSON] Domain.ReferralLinkReq
           :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler =
  updateDriverReferral

updateDriverReferral :: Id SP.Person -> Domain.ReferralLinkReq -> FlowHandler APISuccess
updateDriverReferral driverId = withFlowHandlerAPI . Domain.updateDriverReferral driverId
