{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Fleet where

import qualified API.Action.ProviderPlatform.Fleet.Driver
import qualified API.Action.ProviderPlatform.Fleet.LiveMap
import qualified API.Action.ProviderPlatform.Fleet.Onboarding
import qualified API.Action.ProviderPlatform.Fleet.RegistrationV2
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.ProviderPlatform.Fleet.Driver.API :<|> API.Action.ProviderPlatform.Fleet.LiveMap.API :<|> API.Action.ProviderPlatform.Fleet.Onboarding.API :<|> API.Action.ProviderPlatform.Fleet.RegistrationV2.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.ProviderPlatform.Fleet.Driver.handler merchantId city :<|> API.Action.ProviderPlatform.Fleet.LiveMap.handler merchantId city :<|> API.Action.ProviderPlatform.Fleet.Onboarding.handler merchantId city :<|> API.Action.ProviderPlatform.Fleet.RegistrationV2.handler merchantId city
