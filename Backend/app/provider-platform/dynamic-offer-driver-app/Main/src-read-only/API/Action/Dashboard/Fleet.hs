{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Fleet
  ( API,
    handler,
  )
where

import qualified API.Action.Dashboard.Fleet.Driver
import qualified API.Action.Dashboard.Fleet.LiveMap
import qualified API.Action.Dashboard.Fleet.Onboarding
import qualified API.Action.Dashboard.Fleet.PayoutAccount
import qualified API.Action.Dashboard.Fleet.RegistrationV2
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.Dashboard.Fleet.Driver.API :<|> API.Action.Dashboard.Fleet.LiveMap.API :<|> API.Action.Dashboard.Fleet.Onboarding.API :<|> API.Action.Dashboard.Fleet.PayoutAccount.API :<|> API.Action.Dashboard.Fleet.RegistrationV2.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.Dashboard.Fleet.Driver.handler merchantId city :<|> API.Action.Dashboard.Fleet.LiveMap.handler merchantId city :<|> API.Action.Dashboard.Fleet.Onboarding.handler merchantId city :<|> API.Action.Dashboard.Fleet.PayoutAccount.handler merchantId city :<|> API.Action.Dashboard.Fleet.RegistrationV2.handler merchantId city
