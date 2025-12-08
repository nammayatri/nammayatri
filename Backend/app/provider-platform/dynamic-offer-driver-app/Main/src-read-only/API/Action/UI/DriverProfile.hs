{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.DriverProfile
  ( API,
    handler,
  )
where

import qualified API.Types.UI.DriverProfile
import qualified Control.Lens
import qualified Domain.Action.UI.DriverProfile
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "driver" :> "profile" :> "updateAuthData" :> "triggerOTP" :> ReqBody ('[JSON]) API.Types.UI.DriverProfile.TriggerUpdateAuthOTPReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "driver"
      :> "profile"
      :> "updateAuthData"
      :> "verifyOTP"
      :> ReqBody
           ('[JSON])
           API.Types.UI.DriverProfile.VerifyUpdateAuthOTPReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = postDriverProfileUpdateAuthDataTriggerOTP :<|> postDriverProfileUpdateAuthDataVerifyOTP

postDriverProfileUpdateAuthDataTriggerOTP ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverProfile.TriggerUpdateAuthOTPReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDriverProfileUpdateAuthDataTriggerOTP a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverProfile.postDriverProfileUpdateAuthDataTriggerOTP (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postDriverProfileUpdateAuthDataVerifyOTP ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverProfile.VerifyUpdateAuthOTPReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDriverProfileUpdateAuthDataVerifyOTP a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverProfile.postDriverProfileUpdateAuthDataVerifyOTP (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
