{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.PersonDefaultEmergencyNumber where

import qualified API.Types.UI.PersonDefaultEmergencyNumber
import qualified Control.Lens
import qualified Domain.Action.UI.PersonDefaultEmergencyNumber as Domain.Action.UI.PersonDefaultEmergencyNumber
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
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
  ( TokenAuth :> "profile" :> "defaultEmergencyNumbers"
      :> Get
           '[JSON]
           API.Types.UI.PersonDefaultEmergencyNumber.DefaultEmergencyNumbersEntity
      :<|> TokenAuth
      :> "profile"
      :> "defaultEmergencyNumbers"
      :> ReqBody
           '[JSON]
           API.Types.UI.PersonDefaultEmergencyNumber.DefaultEmergencyNumbersEntity
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = getProfileDefaultEmergencyNumbers :<|> postProfileDefaultEmergencyNumbers

getProfileDefaultEmergencyNumbers ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler API.Types.UI.PersonDefaultEmergencyNumber.DefaultEmergencyNumbersEntity
  )
getProfileDefaultEmergencyNumbers a1 = withFlowHandlerAPI $ Domain.Action.UI.PersonDefaultEmergencyNumber.getProfileDefaultEmergencyNumbers (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postProfileDefaultEmergencyNumbers ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.PersonDefaultEmergencyNumber.DefaultEmergencyNumbersEntity ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postProfileDefaultEmergencyNumbers a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.PersonDefaultEmergencyNumber.postProfileDefaultEmergencyNumbers (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
