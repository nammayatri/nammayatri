{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.AppManagement where

import qualified API.Action.RiderPlatform.AppManagement.Tickets
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = API.Action.RiderPlatform.AppManagement.Tickets.API

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.RiderPlatform.AppManagement.Tickets.handler merchantId city
