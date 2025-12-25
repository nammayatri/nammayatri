{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Merchant
  ( API,
    handler,
  )
where

import qualified API.Types.UI.Merchant
import qualified Control.Lens
import qualified Domain.Action.UI.Merchant
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = (TokenAuth :> "cityConfigs" :> Get '[JSON] API.Types.UI.Merchant.CityConfigs)

handler :: Environment.FlowServer API
handler = getCityConfigs

getCityConfigs ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler API.Types.UI.Merchant.CityConfigs
  )
getCityConfigs a1 = withFlowHandlerAPI $ Domain.Action.UI.Merchant.getCityConfigs (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
