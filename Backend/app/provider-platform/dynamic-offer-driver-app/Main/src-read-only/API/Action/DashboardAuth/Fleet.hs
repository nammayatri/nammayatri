{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Fleet
  ( API,
    handler,
  )
where

import qualified API.Action.DashboardAuth.Fleet.Driver
import qualified API.Action.DashboardAuth.Fleet.LiveMap
import qualified API.Action.DashboardAuth.Fleet.Onboarding
import qualified API.Action.DashboardAuth.Fleet.PayoutAccount
import qualified API.Action.DashboardAuth.Fleet.RegistrationV2
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.DashboardAuth.Fleet.Driver.API :<|> API.Action.DashboardAuth.Fleet.LiveMap.API :<|> API.Action.DashboardAuth.Fleet.Onboarding.API :<|> API.Action.DashboardAuth.Fleet.PayoutAccount.API :<|> API.Action.DashboardAuth.Fleet.RegistrationV2.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.DashboardAuth.Fleet.Driver.handler merchantId city :<|> API.Action.DashboardAuth.Fleet.LiveMap.handler merchantId city :<|> API.Action.DashboardAuth.Fleet.Onboarding.handler merchantId city :<|> API.Action.DashboardAuth.Fleet.PayoutAccount.handler merchantId city :<|> API.Action.DashboardAuth.Fleet.RegistrationV2.handler merchantId city
