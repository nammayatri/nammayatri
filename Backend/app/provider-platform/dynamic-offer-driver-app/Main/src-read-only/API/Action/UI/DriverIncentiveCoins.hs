{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.DriverIncentiveCoins
  ( API,
    handler,
  )
where

import qualified API.Types.UI.DriverIncentiveCoins
import qualified Control.Lens
import qualified Data.Text
import qualified Domain.Action.UI.DriverIncentiveCoins
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
  ( TokenAuth :> "coins" :> "incentiveConfig" :> Header "If-None-Match" Data.Text.Text
      :> Get
           '[JSON]
           (Headers '[Header "ETag" Data.Text.Text] [API.Types.UI.DriverIncentiveCoins.DriverIncentiveCoinConfigItem])
      :<|> TokenAuth
      :> "coins"
      :> "incentiveRideCount"
      :> Get
           '[JSON]
           API.Types.UI.DriverIncentiveCoins.DriverIncentiveRideCountRes
  )

handler :: Environment.FlowServer API
handler = getCoinsIncentiveConfig :<|> getCoinsIncentiveRideCount

getCoinsIncentiveConfig ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Data.Text.Text ->
    Environment.FlowHandler (Headers '[Header "ETag" Data.Text.Text] [API.Types.UI.DriverIncentiveCoins.DriverIncentiveCoinConfigItem])
  )
getCoinsIncentiveConfig a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverIncentiveCoins.getCoinsIncentiveConfig (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getCoinsIncentiveRideCount ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler API.Types.UI.DriverIncentiveCoins.DriverIncentiveRideCountRes
  )
getCoinsIncentiveRideCount a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverIncentiveCoins.getCoinsIncentiveRideCount (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
