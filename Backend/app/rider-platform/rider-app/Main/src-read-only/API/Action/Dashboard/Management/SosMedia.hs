{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.SosMedia
  ( API.Types.RiderPlatform.Management.SosMedia.API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.SosMedia
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.SosMedia
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.RiderPlatform.Management.SosMedia.API)
handler merchantId city = getSosMediaSosMedia merchantId city

getSosMediaSosMedia :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Customer -> Environment.FlowHandler [API.Types.RiderPlatform.Management.SosMedia.GetSosMediaResponse])
getSosMediaSosMedia a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.SosMedia.getSosMediaSosMedia a3 a2 a1
