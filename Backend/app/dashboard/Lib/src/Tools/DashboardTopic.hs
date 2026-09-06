{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.DashboardTopic
  ( resolveTopicForPerson,
    resolveAccessType,
    castAccessType,
  )
where

import qualified DashboardAlert.Domain.Action.InternalAuth as DAAuth
import qualified DashboardAlert.Domain.Types.Audience as DAAudience
import qualified Domain.Types.Capability as DC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Role as DRole
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.BeamFlow (BeamFlow')
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.PersonTier as QPT
import Tools.Error

adminAccessTypes :: [DRole.DashboardAccessType]
adminAccessTypes = [DRole.DASHBOARD_ADMIN]

adminTiers :: [Text]
adminTiers = [DC.superAdminTier, DC.dashboardAdminTier]

castAccessType :: DRole.DashboardAccessType -> DAAudience.DashboardAccessType
castAccessType = \case
  DRole.DASHBOARD_USER -> DAAudience.DASHBOARD_USER
  DRole.DASHBOARD_ADMIN -> DAAudience.DASHBOARD_ADMIN
  DRole.FLEET_OWNER -> DAAudience.FLEET_OWNER
  DRole.DASHBOARD_RELEASE_ADMIN -> DAAudience.DASHBOARD_RELEASE_ADMIN
  DRole.MERCHANT_ADMIN -> DAAudience.MERCHANT_ADMIN
  DRole.RENTAL_FLEET_OWNER -> DAAudience.RENTAL_FLEET_OWNER
  DRole.MERCHANT_MAKER -> DAAudience.MERCHANT_MAKER
  DRole.MERCHANT_SERVER -> DAAudience.MERCHANT_SERVER
  DRole.DASHBOARD_OPERATOR -> DAAudience.DASHBOARD_OPERATOR
  DRole.TICKET_DASHBOARD_USER -> DAAudience.TICKET_DASHBOARD_USER
  DRole.TICKET_DASHBOARD_MERCHANT -> DAAudience.TICKET_DASHBOARD_MERCHANT
  DRole.TICKET_DASHBOARD_ADMIN -> DAAudience.TICKET_DASHBOARD_ADMIN
  DRole.TICKET_DASHBOARD_APPROVER -> DAAudience.TICKET_DASHBOARD_APPROVER

resolveAccessType :: Maybe DRole.DashboardAccessType -> Maybe Text -> Maybe DAAudience.DashboardAccessType
resolveAccessType mbAccessType mbAdminTier
  | maybe False (`elem` adminAccessTypes) mbAccessType || maybe False (`elem` adminTiers) mbAdminTier =
    Just DAAudience.DASHBOARD_ADMIN
  | otherwise = castAccessType <$> mbAccessType

resolveTopicForPerson :: BeamFlow' => (Text -> Flow [Text]) -> Id DP.Person -> Flow (DAAudience.Topic, [Text])
resolveTopicForPerson lookupFleetOwners personId = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mbTier <- QPT.findByPersonId personId
  fleetOwnerIds <- try @_ @SomeException (lookupFleetOwners personId.getId)
  let resolvedFleetOwnerIds = either (const []) identity fleetOwnerIds
  topic <-
    DAAuth.resolveTopic
      (const $ pure resolvedFleetOwnerIds)
      DAAuth.ResolveTopicReq
        { personId = personId.getId,
          accessType = resolveAccessType person.dashboardAccessType ((.adminTier) <$> mbTier)
        }
  pure (topic, resolvedFleetOwnerIds)
