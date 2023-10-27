module API.UI.DriverReferral where

import qualified Domain.Action.UI.DriverReferral as Domain
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
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
           :<|> "generateReferralCode"
             :> TokenAuth
             :> Post '[JSON] Domain.GenerateReferralCodeRes
       )

handler :: FlowServer API
handler =
  createDriverReferral
    :<|> generateReferralCode

createDriverReferral :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Domain.ReferralLinkReq -> FlowHandler APISuccess
createDriverReferral (driverId, merchantId, merchantOpCityId) = withFlowHandlerAPI . Domain.createDriverReferral (driverId, merchantId, merchantOpCityId) False

generateReferralCode :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler Domain.GenerateReferralCodeRes
generateReferralCode = withFlowHandlerAPI . Domain.generateReferralCode
