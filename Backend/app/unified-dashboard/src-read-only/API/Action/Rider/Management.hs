{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Rider.Management where

import qualified API.Action.Rider.Management.HealthCheck
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = API.Action.Rider.Management.HealthCheck.API

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.Rider.Management.HealthCheck.handler merchantId city
