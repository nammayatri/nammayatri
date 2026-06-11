{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.CityInfo
  ( API,
    handler,
  )
where

import qualified API.Types.UI.CityInfo
import qualified Control.Lens
import qualified Domain.Action.UI.CityInfo
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = (TokenAuth :> "operatingCity" :> MandatoryQueryParam "stdCode" Kernel.Prelude.Text :> Get ('[JSON]) API.Types.UI.CityInfo.StdCodeResp)

handler :: Environment.FlowServer API
handler = getOperatingCity

getOperatingCity ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.FlowHandler API.Types.UI.CityInfo.StdCodeResp
  )
getOperatingCity a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CityInfo.getOperatingCity (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
