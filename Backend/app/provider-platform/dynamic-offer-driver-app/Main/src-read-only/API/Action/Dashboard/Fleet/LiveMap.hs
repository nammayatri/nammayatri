{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Fleet.LiveMap
  ( API.Types.ProviderPlatform.Fleet.LiveMap.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.LiveMap
import qualified Domain.Action.Dashboard.Fleet.LiveMap as Domain.Action.Dashboard.Fleet.LiveMap
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Fleet.LiveMap.API)
handler merchantId city = getLiveMapDrivers merchantId city

getLiveMapDrivers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler [API.Types.ProviderPlatform.Fleet.LiveMap.MapDriverInfoRes])
getLiveMapDrivers a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.LiveMap.getLiveMapDrivers a2 a1
