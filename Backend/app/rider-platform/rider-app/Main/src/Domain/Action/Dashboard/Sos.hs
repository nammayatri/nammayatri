{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Sos (getSosTracking) where

import qualified API.Types.RiderPlatform.Management.Sos
import qualified API.Types.UI.Sos as UISos
import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Domain.Action.UI.Sos as Sos
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified Safety.Domain.Types.Sos as SafetyDSos
import Servant
import Tools.Auth

getSosTracking :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Sos -> Environment.Flow API.Types.RiderPlatform.Management.Sos.SosTrackingRes)
getSosTracking _merchantShortId _opCity sosId = do
  let sosId' = Kernel.Types.Id.cast @Dashboard.Common.Sos @SafetyDSos.Sos sosId
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

convertSosState :: SafetyDSos.SosState -> API.Types.RiderPlatform.Management.Sos.SosState
convertSosState SafetyDSos.LiveTracking = API.Types.RiderPlatform.Management.Sos.LiveTracking
convertSosState SafetyDSos.SosActive = API.Types.RiderPlatform.Management.Sos.SosActive

convertSosStatus :: SafetyDSos.SosStatus -> API.Types.RiderPlatform.Management.Sos.SosStatus
convertSosStatus SafetyDSos.Resolved = API.Types.RiderPlatform.Management.Sos.Resolved
convertSosStatus SafetyDSos.NotResolved = API.Types.RiderPlatform.Management.Sos.NotResolved
convertSosStatus SafetyDSos.Pending = API.Types.RiderPlatform.Management.Sos.Pending
convertSosStatus SafetyDSos.MockPending = API.Types.RiderPlatform.Management.Sos.MockPending
convertSosStatus SafetyDSos.MockResolved = API.Types.RiderPlatform.Management.Sos.MockResolved
