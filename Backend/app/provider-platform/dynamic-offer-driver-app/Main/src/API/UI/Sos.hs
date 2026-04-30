{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module API.UI.Sos
  ( API,
    handler,
  )
where

import Domain.Action.UI.Sos ()
-- HasSosHandle + BuildSosCtx instances
import Environment
import EulerHS.Prelude
import Kernel.Utils.Common
import qualified Safety.API.UI.Sos as SosRoutes
import qualified Safety.Domain.Action.UI.Sos as SharedSos
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = SosRoutes.SosAPI TokenAuth

handler :: Environment.FlowServer API
handler =
  (\auth rideId -> withFlowHandlerAPI $ SharedSos.sosGetDetails auth rideId)
    :<|> (\a b c d -> withFlowHandlerAPI $ SharedSos.sosIvrOutcome a b c d)
    :<|> (\auth req -> withFlowHandlerAPI $ SharedSos.sosCreate auth req)
    :<|> (\auth sosId req -> withFlowHandlerAPI $ SharedSos.sosStatus auth sosId req)
    :<|> (\auth sosId req -> withFlowHandlerAPI $ SharedSos.sosMarkRideAsSafe auth sosId req)
    :<|> (\auth req -> withFlowHandlerAPI $ SharedSos.sosCreateMockSos auth req)
    :<|> (\auth req -> withFlowHandlerAPI $ SharedSos.sosCallPolice auth req)
    :<|> (\auth sosId req -> withFlowHandlerAPI $ SharedSos.sosUpdateLocation auth sosId req)
    :<|> (\sosId -> withFlowHandlerAPI $ SharedSos.sosTracking sosId)
    :<|> (\auth req -> withFlowHandlerAPI $ SharedSos.sosStartTracking auth req)
    :<|> (\auth sosId req -> withFlowHandlerAPI $ SharedSos.sosUpdateState auth sosId req)
    :<|> (\auth sosId -> withFlowHandlerAPI $ SharedSos.sosTrackingDetails auth sosId)
    :<|> (\auth sosId req -> withFlowHandlerAPI $ SharedSos.sosUpdateToRide auth sosId req)
    :<|> (\auth status -> withFlowHandlerAPI $ SharedSos.sosGetDetailsByPerson auth status)
    :<|> (\req -> withFlowHandlerAPI $ SharedSos.sosErssStatusUpdate req)
    :<|> (\rideShortId -> withFlowHandlerAPI $ SharedSos.sosRideDetails rideShortId)
    :<|> (\auth sosId req -> withFlowHandlerAPI $ SharedSos.sosUploadMedia auth sosId req)
