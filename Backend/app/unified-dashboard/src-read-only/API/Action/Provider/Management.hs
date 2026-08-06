{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Provider.Management where

import qualified API.Action.Provider.Management.HealthCheck
import qualified API.Action.Provider.Management.Person
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.Provider.Management.HealthCheck.API :<|> API.Action.Provider.Management.Person.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.Provider.Management.HealthCheck.handler merchantId city :<|> API.Action.Provider.Management.Person.handler merchantId city
