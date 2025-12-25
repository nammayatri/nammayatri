{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.CRIS
  ( API,
    handler,
  )
where

import qualified API.Types.UI.CRIS
import qualified Control.Lens
import qualified Domain.Action.UI.CRIS
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Merchant
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
  ( TokenAuth :> "cris" :> "getSDKData" :> QueryParam "integratedBppConfigId" (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig)
      :> ReqBody
           '[JSON]
           API.Types.UI.CRIS.GetSDKDataRequest
      :> Post
           '[JSON]
           API.Types.UI.CRIS.GetSDKDataResponse
      :<|> TokenAuth
      :> "cris"
      :> "otp"
      :> "generation"
      :> QueryParam
           "integratedBppConfigId"
           (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig)
      :> Get
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "cris"
      :> "change"
      :> "device"
      :> QueryParam
           "integratedBppConfigId"
           (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig)
      :> ReqBody
           '[JSON]
           API.Types.UI.CRIS.CrisChangeDeviceRequest
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = postCrisGetSDKData :<|> getCrisOtpGeneration :<|> postCrisChangeDevice

postCrisGetSDKData ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig) ->
    API.Types.UI.CRIS.GetSDKDataRequest ->
    Environment.FlowHandler API.Types.UI.CRIS.GetSDKDataResponse
  )
postCrisGetSDKData a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CRIS.postCrisGetSDKData (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

getCrisOtpGeneration ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig) ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
getCrisOtpGeneration a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CRIS.getCrisOtpGeneration (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postCrisChangeDevice ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig) ->
    API.Types.UI.CRIS.CrisChangeDeviceRequest ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postCrisChangeDevice a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CRIS.postCrisChangeDevice (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
