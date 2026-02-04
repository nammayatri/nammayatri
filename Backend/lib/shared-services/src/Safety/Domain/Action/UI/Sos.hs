{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Safety.Domain.Action.UI.Sos where

import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Safety.Domain.Types.Common as Common
import qualified Safety.Domain.Types.Sos as DSos
import Safety.Storage.BeamFlow
import qualified Safety.Storage.Queries.Sos as QSos

-- | Update SOS entityType from NonRide to Ride and update rideId
-- Validates that the SOS exists and has entityType "NonRide" before updating
updateSosFromNonRideToRide ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m
  ) =>
  Id DSos.Sos ->
  Id Common.Ride ->
  m ()
updateSosFromNonRideToRide sosId newRideId = do
  sos <- QSos.findById sosId >>= fromMaybeM (InvalidRequest $ "SOS not found: " <> sosId.getId)

  unless (sos.entityType == Just DSos.NonRide) $
    throwError $ InvalidRequest $ "SOS entityType is not NonRide. Current entityType: " <> show sos.entityType

  let updatedSos =
        sos{entityType = Just DSos.Ride,
            rideId = newRideId
           }

  QSos.updateByPrimaryKey updatedSos
