{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.PayoutDriverStatus
  ( API,
    handler,
  )
where

import qualified API.Types.UI.PayoutDriverStatus
import qualified Control.Lens
import qualified Data.Text
import qualified Domain.Action.UI.PayoutDriverStatus
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

type API = (TokenAuth :> "payout" :> Capture "rideId" Data.Text.Text :> "status" :> Get '[JSON] API.Types.UI.PayoutDriverStatus.DriverPayoutStatusRespSuccess)

handler :: Environment.FlowServer API
handler = getPayoutStatus

getPayoutStatus ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    Environment.FlowHandler API.Types.UI.PayoutDriverStatus.DriverPayoutStatusRespSuccess
  )
getPayoutStatus a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.PayoutDriverStatus.getPayoutStatus (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
