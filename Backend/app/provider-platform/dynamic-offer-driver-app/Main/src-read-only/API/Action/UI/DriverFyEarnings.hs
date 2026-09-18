{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.DriverFyEarnings
  ( API,
    handler,
  )
where

import qualified API.Types.UI.DriverFyEarnings
import qualified Control.Lens
import qualified Domain.Action.UI.DriverFyEarnings
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

type API =
  ( TokenAuth :> "driver" :> "fyEarnings" :> QueryParam "quarter" Kernel.Prelude.Int :> MandatoryQueryParam "financialYear" Kernel.Prelude.Int
      :> Get
           ('[JSON])
           API.Types.UI.DriverFyEarnings.DriverFyEarningsResp
  )

handler :: Environment.FlowServer API
handler = getDriverFyEarnings

getDriverFyEarnings ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Int ->
    Environment.FlowHandler API.Types.UI.DriverFyEarnings.DriverFyEarningsResp
  )
getDriverFyEarnings a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverFyEarnings.getDriverFyEarnings (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
