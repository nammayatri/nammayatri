{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Fleet.Access
  ( FleetOwnerInfo (..),
    checkRequestorAccessToFleet,
  )
where

import qualified Domain.Types.Person as DP
import Environment
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FleetOperatorAssociation as QFleetOperatorAssociation
import qualified Storage.Queries.Person as QP
import Tools.Error

data FleetOwnerInfo = FleetOwnerInfo
  { fleetOwner :: DP.Person,
    mbOperator :: Maybe DP.Person
  }

checkRequestorAccessToFleet ::
  Bool ->
  Maybe Text ->
  Text ->
  Flow FleetOwnerInfo
checkRequestorAccessToFleet allowOtherRoles mbRequestorId fleetOwnerId = do
  fleetOwner <- B.runInReplica $ QP.findById (Id fleetOwnerId :: Id DP.Person) >>= fromMaybeM (FleetOwnerNotFound fleetOwnerId)
  unless (fleetOwner.role == DP.FLEET_OWNER) $
    throwError (InvalidRequest "Invalid fleet owner")
  case mbRequestorId of
    Nothing -> pure $ FleetOwnerInfo {fleetOwner, mbOperator = Nothing} -- old flow
    Just requestorId -> do
      -- new flow
      requestor <- B.runInReplica $ QP.findById (Id requestorId :: Id DP.Person) >>= fromMaybeM (PersonNotFound requestorId)
      case requestor.role of
        DP.FLEET_OWNER -> do
          unless (fleetOwner.id == requestor.id) $
            throwError (InvalidRequest "Invalid fleet owner")
          pure $ FleetOwnerInfo {fleetOwner, mbOperator = Nothing}
        DP.OPERATOR -> do
          association <-
            QFleetOperatorAssociation.findByFleetIdAndOperatorId fleetOwner.id.getId requestor.id.getId True
              >>= fromMaybeM (InvalidRequest "FleetOperatorAssociation does not exist") -- TODO add error codes
          whenJust association.associatedTill $ \associatedTill -> do
            now <- getCurrentTime
            when (now > associatedTill) $
              throwError (InvalidRequest "FleetOperatorAssociation expired")
          pure $ FleetOwnerInfo {fleetOwner, mbOperator = Just requestor}
        DP.ADMIN -> pure $ FleetOwnerInfo {fleetOwner, mbOperator = Nothing}
        _ -> if allowOtherRoles then pure $ FleetOwnerInfo {fleetOwner, mbOperator = Nothing} else throwError AccessDenied
