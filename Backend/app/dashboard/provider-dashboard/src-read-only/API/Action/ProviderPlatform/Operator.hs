{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Operator where

import qualified API.Action.ProviderPlatform.Operator.Driver
import qualified API.Action.ProviderPlatform.Operator.FleetManagement
import qualified API.Action.ProviderPlatform.Operator.Registration
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.ProviderPlatform.Operator.Driver.API :<|> API.Action.ProviderPlatform.Operator.FleetManagement.API :<|> API.Action.ProviderPlatform.Operator.Registration.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.ProviderPlatform.Operator.Driver.handler merchantId city :<|> API.Action.ProviderPlatform.Operator.FleetManagement.handler merchantId city :<|> API.Action.ProviderPlatform.Operator.Registration.handler merchantId city
