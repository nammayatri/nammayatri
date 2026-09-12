{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.SosMedia
  ( API,
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
import Tools.Auth.DashboardUserAuth

type API = ("sos-media" :> (GetSosMediaSosMedia))

type GetSosMediaSosMedia = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/SOS_MEDIA/GET_SOS_MEDIA_SOS_MEDIA" :> API.Types.RiderPlatform.Management.SosMedia.GetSosMediaSosMedia)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getSosMediaSosMedia merchantId city

getSosMediaSosMedia :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Customer -> Environment.FlowHandler [API.Types.RiderPlatform.Management.SosMedia.GetSosMediaResponse])
getSosMediaSosMedia a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.SosMedia.getSosMediaSosMedia a4 a3 a1
