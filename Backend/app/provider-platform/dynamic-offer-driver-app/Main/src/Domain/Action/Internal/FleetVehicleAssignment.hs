{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.FleetVehicleAssignment where

import qualified Domain.Types.FleetVehicleAssignment as DFVA
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Vehicle as DV
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import qualified Storage.Queries.FleetVehicleAssignment as QFVA

-- Create takes booking identifiers and optional assignee info
-- ticketBookingId is required; serviceId/placeId optional per schema

data CreateFleetVehicleAssignmentReq = CreateFleetVehicleAssignmentReq
  { ticketBookingId :: Text,
    ticketBookingServiceId :: Maybe Text,
    ticketPlaceId :: Maybe Text,
    fleetOwnerId :: Maybe (Id DP.Person),
    vehicleId :: Maybe (Id DV.Vehicle),
    amount :: Maybe HighPrecMoney
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

-- Update allows changing assignee and status

data UpdateFleetVehicleAssignmentReq = UpdateFleetVehicleAssignmentReq
  { id :: Id DFVA.FleetVehicleAssignment,
    fleetOwnerId :: Maybe (Id DP.Person),
    vehicleId :: Maybe (Id DV.Vehicle),
    assignmentStatus :: Maybe DFVA.AssignmentStatus
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

-- Create vehicle assignment, NEW status by default
createVehicleAssignment :: CreateFleetVehicleAssignmentReq -> Flow APISuccess
createVehicleAssignment req = do
  assignmentId <- generateGUID
  now <- getCurrentTime

  let assignment =
        DFVA.FleetVehicleAssignment
          { id = assignmentId,
            ticketBookingId = req.ticketBookingId,
            ticketBookingServiceId = req.ticketBookingServiceId,
            ticketPlaceId = req.ticketPlaceId,
            fleetOwnerId = req.fleetOwnerId,
            vehicleId = req.vehicleId,
            amount = req.amount,
            assignmentStatus = DFVA.NEW,
            merchantId = Nothing,
            merchantOperatingCityId = Nothing,
            assignedAt = now,
            createdAt = now,
            updatedAt = now
          }

  QFVA.create assignment
  pure Success

-- Update vehicle assignment by id with optional fields
updateVehicleAssignment :: UpdateFleetVehicleAssignmentReq -> Flow APISuccess
updateVehicleAssignment req = do
  -- auth via internal token
  -- internalAPIKey <- asks (.internalAPIKey)
  -- unless (Just internalAPIKey == apiKey) $ throwError $ AuthBlocked "Invalid internal API key"

  assignment <- QFVA.findById req.id >>= fromMaybeM (InvalidRequest $ "FleetVehicleAssignment not found: " <> req.id.getId)

  -- validations (optional fields)
  -- whenJust req.fleetOwnerId $ \foId -> void $ QP.findById foId >>= fromMaybeM (PersonNotFound foId.getId)
  -- whenJust req.vehicleId $ \vId -> void $ QV.findById vId >>= fromMaybeM (VehicleNotFound vId.getId)

  -- Only update provided fields; use generated updateAssignmentStatus for status and assignee
  case (req.fleetOwnerId, req.vehicleId, req.assignmentStatus) of
    (Nothing, Nothing, Nothing) -> pure ()
    _ -> QFVA.updateAssignmentStatus req.fleetOwnerId req.vehicleId (fromMaybe assignment.assignmentStatus req.assignmentStatus) req.id

  pure Success
