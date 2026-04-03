{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.DriverProfile 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.DriverProfile
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified API.Types.UI.DriverProfile
import qualified Kernel.Types.APISuccess
import qualified Domain.Types.MerchantOperatingCity



type API = (TokenAuth :> "driver" :> "profile" :> "updateAuthData" :> "triggerOTP" :> ReqBody ('[JSON]) API.Types.UI.DriverProfile.TriggerUpdateAuthOTPReq :> Post ('[JSON])
                                                                                                                                                                   Kernel.Types.APISuccess.APISuccess :<|> TokenAuth :> "driver" :> "profile" :> "updateAuthData" :> "verifyOTP" :> ReqBody ('[JSON])
                                                                                                                                                                                                                                                                                            API.Types.UI.DriverProfile.VerifyUpdateAuthOTPReq :> Post ('[JSON])
                                                                                                                                                                                                                                                                                                                                                      Kernel.Types.APISuccess.APISuccess)
handler :: Environment.FlowServer API
handler = postDriverProfileUpdateAuthDataTriggerOTP :<|> postDriverProfileUpdateAuthDataVerifyOTP
postDriverProfileUpdateAuthDataTriggerOTP :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                                               Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                                               Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> API.Types.UI.DriverProfile.TriggerUpdateAuthOTPReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverProfileUpdateAuthDataTriggerOTP a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverProfile.postDriverProfileUpdateAuthDataTriggerOTP (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
postDriverProfileUpdateAuthDataVerifyOTP :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                                              Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                                              Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> API.Types.UI.DriverProfile.VerifyUpdateAuthOTPReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverProfileUpdateAuthDataVerifyOTP a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverProfile.postDriverProfileUpdateAuthDataVerifyOTP (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1



