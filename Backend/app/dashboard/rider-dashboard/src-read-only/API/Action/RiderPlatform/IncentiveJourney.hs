{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.IncentiveJourney where

import qualified API.Action.RiderPlatform.IncentiveJourney.IncentiveJourney
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = API.Action.RiderPlatform.IncentiveJourney.IncentiveJourney.API

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.RiderPlatform.IncentiveJourney.IncentiveJourney.handler merchantId city
