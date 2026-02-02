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

import qualified IssueManagement.Domain.Types.MediaFile as DMF
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Safety.Domain.Types.Common as Common
import qualified Safety.Domain.Types.Sos as DSos
import Safety.Storage.BeamFlow
import qualified Safety.Storage.Queries.SafetySettings as QSafetySettings
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
            rideId = Just newRideId
           }

  QSos.updateByPrimaryKey updatedSos

-- | Create a new SOS record
createSos ::
  ( BeamFlow m r,
    CoreMetrics m
  ) =>
  DSos.Sos ->
  m (Id DSos.Sos)
createSos sos = do
  QSos.create sos
  return sos.id

-- | Update SOS status
updateSosStatus ::
  ( BeamFlow m r,
    CoreMetrics m
  ) =>
  DSos.SosStatus ->
  Id DSos.Sos ->
  m ()
updateSosStatus status sosId = QSos.updateStatus status sosId

-- | Update SOS state
updateSosState ::
  ( BeamFlow m r,
    CoreMetrics m
  ) =>
  Maybe DSos.SosState ->
  Id DSos.Sos ->
  m ()
updateSosState sosState sosId = QSos.updateState sosState sosId

-- | Update SOS media files
updateSosMediaFiles ::
  ( BeamFlow m r,
    CoreMetrics m
  ) =>
  [Id DMF.MediaFile] ->
  Id DSos.Sos ->
  m ()
updateSosMediaFiles mediaFiles sosId = QSos.updateMediaFiles mediaFiles sosId

-- | Update SOS tracking expiration time
updateSosTrackingExpiresAt ::
  ( BeamFlow m r,
    CoreMetrics m
  ) =>
  Maybe UTCTime ->
  Id DSos.Sos ->
  m ()
updateSosTrackingExpiresAt trackingExpiresAt sosId = QSos.updateTrackingExpiresAt trackingExpiresAt sosId

-- | Find SOS by ID
findSosById ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m
  ) =>
  Id DSos.Sos ->
  m (Maybe DSos.Sos)
findSosById sosId = QSos.findById sosId

-- | Find SOS by ticket ID
findSosByTicketId ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m
  ) =>
  Maybe Text ->
  m (Maybe DSos.Sos)
findSosByTicketId ticketId = QSos.findByTicketId ticketId

-- | Find SOS by person ID
findSosByPersonId ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m
  ) =>
  Id Common.Person ->
  m [DSos.Sos]
findSosByPersonId personId = QSos.findByPersonId personId

-- | Update mock safety drill status
updateMockSafetyDrillStatus ::
  ( BeamFlow m r,
    CoreMetrics m
  ) =>
  Maybe Bool ->
  Id Common.Person ->
  m ()
updateMockSafetyDrillStatus hasCompletedMockSafetyDrill personId = QSafetySettings.updateMockSafetyDrillStatus hasCompletedMockSafetyDrill personId
