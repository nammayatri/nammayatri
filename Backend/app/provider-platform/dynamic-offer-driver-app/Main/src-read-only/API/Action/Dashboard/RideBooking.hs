{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.RideBooking
  ( API,
    handler,
  )
where

import qualified API.Action.Dashboard.RideBooking.Driver
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = API.Action.Dashboard.RideBooking.Driver.API

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.Dashboard.RideBooking.Driver.handler merchantId city
