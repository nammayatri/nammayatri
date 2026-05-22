{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Conductor
  ( API,
    handler,
  )
where

import qualified API.Action.Dashboard.Conductor.Registration
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = API.Action.Dashboard.Conductor.Registration.API

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.Dashboard.Conductor.Registration.handler merchantId city
