{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.Sos
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.Sos
import qualified Dashboard.Common
import qualified Domain.Action.RiderPlatform.Management.Sos
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("sos" :> GetSosTracking)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getSosTracking merchantId city

type GetSosTracking = API.Types.RiderPlatform.Management.Sos.GetSosTracking

getSosTracking :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Sos -> Environment.FlowHandler API.Types.RiderPlatform.Management.Sos.SosTrackingRes)
getSosTracking merchantShortId opCity sosId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Sos.getSosTracking merchantShortId opCity sosId
