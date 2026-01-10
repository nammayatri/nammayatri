{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Sos (getSosTracking) where

import qualified API.Types.RiderPlatform.Management.Sos
import qualified API.Types.UI.Sos as UISos
import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Domain.Action.UI.Sos as Sos
import qualified Domain.Types.Merchant
import qualified Domain.Types.Sos
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

getSosTracking :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Sos -> Environment.Flow API.Types.RiderPlatform.Management.Sos.SosTrackingRes)
getSosTracking _merchantShortId _opCity sosId = do
  let sosId' = Kernel.Types.Id.cast @Dashboard.Common.Sos @Domain.Types.Sos.Sos sosId
  res <- Sos.getSosTracking sosId'
  pure $ convertToApiRes res

-- Convert from rider-app SosTrackingRes to dashboard API SosTrackingRes
convertToApiRes :: UISos.SosTrackingRes -> API.Types.RiderPlatform.Management.Sos.SosTrackingRes
convertToApiRes r =
  API.Types.RiderPlatform.Management.Sos.SosTrackingRes
    { currentLocation = convertLocation <$> r.currentLocation,
      sosState = convertSosState <$> r.sosState,
      status = convertSosStatus r.status
    }

convertLocation :: UISos.SosLocationRes -> API.Types.RiderPlatform.Management.Sos.SosLocationRes
convertLocation loc =
  API.Types.RiderPlatform.Management.Sos.SosLocationRes
    { lat = loc.lat,
      lon = loc.lon,
      accuracy = loc.accuracy
    }

convertSosState :: Domain.Types.Sos.SosState -> API.Types.RiderPlatform.Management.Sos.SosState
convertSosState Domain.Types.Sos.LiveTracking = API.Types.RiderPlatform.Management.Sos.LiveTracking
convertSosState Domain.Types.Sos.SosActive = API.Types.RiderPlatform.Management.Sos.SosActive

convertSosStatus :: Domain.Types.Sos.SosStatus -> API.Types.RiderPlatform.Management.Sos.SosStatus
convertSosStatus Domain.Types.Sos.Resolved = API.Types.RiderPlatform.Management.Sos.Resolved
convertSosStatus Domain.Types.Sos.NotResolved = API.Types.RiderPlatform.Management.Sos.NotResolved
convertSosStatus Domain.Types.Sos.Pending = API.Types.RiderPlatform.Management.Sos.Pending
convertSosStatus Domain.Types.Sos.MockPending = API.Types.RiderPlatform.Management.Sos.MockPending
convertSosStatus Domain.Types.Sos.MockResolved = API.Types.RiderPlatform.Management.Sos.MockResolved
