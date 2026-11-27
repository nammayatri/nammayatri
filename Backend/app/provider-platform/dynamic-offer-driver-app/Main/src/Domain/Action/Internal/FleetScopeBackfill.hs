{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | One-off, re-runnable job that copies today's access into @person_role_assignment@, a merchant at
-- a time. It runs in driver-app because it needs both databases: the dashboard one for people, roles
-- and city access, and this one for fleets and member associations.
--
-- What becomes an assignment:
--
--   * a dashboard person's role on each city they have access to (CITY scope), except fleet owners,
--     whose city access only routes requests, and operators, whose fleet access is read from the
--     operator association;
--   * a fleet owner's role on their own fleet (FLEET scope, the fleet id is the owner's person id);
--   * every member association row: that member's role on that fleet. The @enabled@ flag on the row
--     only says which fleets the member had selected, not whether they have access, so it is not copied.
--
-- Running it twice gives the same rows: an assignment that already exists is left as it is, and a
-- revoked one is never brought back. With @dryRun@ nothing is written.
module Domain.Action.Internal.FleetScopeBackfill
  ( FleetScopeBackfillRes (..),
    fleetScopeBackfill,
  )
where

import Data.List (nub, nubBy)
import qualified Domain.Types.FleetOwnerInformation as DFOI
import qualified Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Merchant as DDashMerchant
import qualified "lib-dashboard" Domain.Types.Person as DDashPerson
import qualified "lib-dashboard" Domain.Types.PersonRoleAssignment as DPRA
import qualified "lib-dashboard" Domain.Types.Role as DDashRole
import Environment
import Kernel.Beam.Functions (runInDashboardDb)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import "lib-dashboard" Storage.Beam.SchemaInstances ()
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.FleetMemberAssociationExtra as QFMAExtra
import qualified Storage.Queries.FleetOwnerInformationExtra as QFOIExtra
import qualified "lib-dashboard" Storage.Queries.Merchant as QDashMerchant
import qualified "lib-dashboard" Storage.Queries.MerchantAccess as QDashAccess
import qualified "lib-dashboard" Storage.Queries.Person as QDashPerson
import qualified "lib-dashboard" Storage.Queries.PersonRoleAssignment as QDashAssignment
import qualified "lib-dashboard" Storage.Queries.Role as QDashRole
import Tools.Error

data FleetScopeBackfillRes = FleetScopeBackfillRes
  { dryRun :: Bool,
    -- | Assignments created, or that would be created on a dry run.
    cityAssignments :: Int,
    fleetOwnerAssignments :: Int,
    memberAssignments :: Int,
    -- | Assignments that were already there and were left alone.
    alreadyPresent :: Int,
    skippedOperators :: Int,
    -- | Audit: fleets whose owner has no dashboard person. Expected to be none.
    fleetsWithoutDashboardPerson :: [Text],
    -- | Audit: members with no dashboard account.
    membersWithoutDashboardAccount :: [Text],
    -- | Audit: member rows that are not selected. They still become assignments.
    disabledMemberRows :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CandidateKind = CityAccess | FleetOwner | FleetMember
  deriving (Eq)

data Candidate = Candidate
  { personId :: Id DDashPerson.Person,
    roleId :: Id DDashRole.Role,
    scopeType :: DPRA.ScopeType,
    scopeId :: Text,
    kind :: CandidateKind
  }

sameAssignment :: Candidate -> Candidate -> Bool
sameAssignment a b = a.personId == b.personId && a.roleId == b.roleId && a.scopeType == b.scopeType && a.scopeId == b.scopeId

data PersonPlan = PlanCities [Candidate] | PlanFleetRole | PlanOperator | PlanSkip

-- | Fleet owners and members hold their role on a fleet, so their city access is not turned into
-- city assignments.
isFleetAccessType :: DDashRole.DashboardAccessType -> Bool
isFleetAccessType accessType = accessType `elem` [DDashRole.FLEET_OWNER, DDashRole.RENTAL_FLEET_OWNER]

fleetScopeBackfill :: ShortId DM.Merchant -> Bool -> Maybe Text -> Flow FleetScopeBackfillRes
fleetScopeBackfill merchantShortId dryRun apiKey = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantNotFound merchantShortId.getShortId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  dashMerchant <-
    runInDashboardDb $
      QDashMerchant.findByShortId (ShortId merchantShortId.getShortId :: ShortId DDashMerchant.Merchant)
        >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)

  -- People with access to this merchant: their role on each city they can reach.
  accesses <- runInDashboardDb $ QDashAccess.findAllUserAccountForMerchant dashMerchant.id
  plans <- forM accesses $ \access -> runInDashboardDb $ do
    mbPerson <- QDashPerson.findById access.personId
    mbRole <- maybe (pure Nothing) (QDashRole.findById . (.roleId)) mbPerson
    case (mbPerson, mbRole) of
      (Just person, Just role)
        | isFleetAccessType role.dashboardAccessType -> pure PlanFleetRole
        | role.dashboardAccessType == DDashRole.DASHBOARD_OPERATOR -> pure PlanOperator
        | otherwise -> do
          cityAccess <- filter (\a -> a.merchantId == dashMerchant.id) <$> QDashAccess.findAllMerchantAccessByPersonId person.id
          pure . PlanCities $ map (\a -> Candidate person.id person.roleId DPRA.CITY (show a.operatingCity) CityAccess) cityAccess
      _ -> pure PlanSkip
  let cityCandidates = concat [cs | PlanCities cs <- plans]
      skippedOperators = length [() | PlanOperator <- plans]

  -- Fleets of this merchant: the owner's role on their fleet, and every member row.
  fleetOwners <- allFleetOwners merchant.id
  let fleetOwnerIds = map (.fleetOwnerPersonId.getId) fleetOwners
  memberRows <- concat <$> mapM QFMAExtra.findAllByFleetOwnerIdsDb (chunksOf 500 fleetOwnerIds)
  ownerResults <- forM fleetOwnerIds $ \ownerId -> do
    mbPerson <- runInDashboardDb $ QDashPerson.findById (Id ownerId)
    pure $ maybe (Left ownerId) (\p -> Right (Candidate p.id p.roleId DPRA.FLEET ownerId FleetOwner)) mbPerson
  memberResults <- forM memberRows $ \row -> do
    mbPerson <- runInDashboardDb $ QDashPerson.findById (Id row.fleetMemberId)
    pure $ maybe (Left row.fleetMemberId) (\p -> Right (Candidate p.id p.roleId DPRA.FLEET row.fleetOwnerId FleetMember)) mbPerson
  let ownerCandidates = [c | Right c <- ownerResults]
      memberCandidates = [c | Right c <- memberResults]

  -- Write what is not there yet. An assignment that exists is left as it is, so a revoked one stays revoked.
  let candidates = nubBy sameAssignment (cityCandidates <> ownerCandidates <> memberCandidates)
  created <- forM candidates $ \c -> do
    existing <- runInDashboardDb $ QDashAssignment.findByPersonRoleScope c.personId c.roleId c.scopeType c.scopeId dashMerchant.id
    case existing of
      Just _ -> pure (c, False)
      Nothing -> do
        unless dryRun $ do
          assignmentId <- generateGUID
          now <- getCurrentTime
          runInDashboardDb $
            QDashAssignment.create
              DPRA.PersonRoleAssignment
                { id = assignmentId,
                  personId = c.personId,
                  roleId = c.roleId,
                  scopeType = c.scopeType,
                  scopeId = c.scopeId,
                  merchantId = dashMerchant.id,
                  status = DPRA.ACTIVE,
                  grantedBy = Nothing,
                  manuallyGranted = False,
                  handoverFrom = Nothing,
                  createdAt = now,
                  updatedAt = now
                }
        pure (c, True)
  let newOfKind k = length [() | (c, True) <- created, c.kind == k]
  pure
    FleetScopeBackfillRes
      { cityAssignments = newOfKind CityAccess,
        fleetOwnerAssignments = newOfKind FleetOwner,
        memberAssignments = newOfKind FleetMember,
        alreadyPresent = length [() | (_, False) <- created],
        fleetsWithoutDashboardPerson = [i | Left i <- ownerResults],
        membersWithoutDashboardAccount = nub [i | Left i <- memberResults],
        disabledMemberRows = length (filter (not . (.enabled)) memberRows),
        ..
      }

-- | Every fleet owner of the merchant, a page at a time.
allFleetOwners :: Id DM.Merchant -> Flow [DFOI.FleetOwnerInformation]
allFleetOwners merchantId = go 0 []
  where
    pageSize = 500
    go offset acc = do
      page <- QFOIExtra.findAllByMerchantIdPaged merchantId pageSize offset
      let acc' = acc <> page
      if length page < pageSize then pure acc' else go (offset + pageSize) acc'

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (a, b) = splitAt n xs in a : chunksOf n b
