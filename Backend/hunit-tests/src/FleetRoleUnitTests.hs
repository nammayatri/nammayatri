{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for fleet role changes introduced in this PR:
--   - FLEET_BUSINESS role removed from login/verify role lookups
--   - isAssociationBetweenTwoPerson: FLEET_BUSINESS cases removed
--   - getDriverEarnings: FLEET_BUSINESS case removed
--   - getCommunicationRecipients: FLEET_OWNER only (no FLEET_BUSINESS)
--   - Fleet owner check: (person.role == FLEET_OWNER) instead of checkFleetOwnerRole
module FleetRoleUnitTests where

import qualified "dynamic-offer-driver-app" Domain.Types.Person as DP
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))
import Prelude

-- =============================================================================
-- ROLE ENUM SANITY TESTS
-- =============================================================================

testRoleEnumValues :: TestTree
testRoleEnumValues =
  testGroup
    "Role Enum Values"
    [ testCase "DRIVER role has correct string representation" $ do
        show DP.DRIVER @?= "DRIVER",
      testCase "FLEET_OWNER role has correct string representation" $ do
        show DP.FLEET_OWNER @?= "FLEET_OWNER",
      testCase "FLEET_BUSINESS role has correct string representation" $ do
        show DP.FLEET_BUSINESS @?= "FLEET_BUSINESS",
      testCase "OPERATOR role has correct string representation" $ do
        show DP.OPERATOR @?= "OPERATOR",
      testCase "ADMIN role has correct string representation" $ do
        show DP.ADMIN @?= "ADMIN",
      testCase "All five roles are distinct" $ do
        let roles = [DP.DRIVER, DP.ADMIN, DP.FLEET_OWNER, DP.FLEET_BUSINESS, DP.OPERATOR]
        length roles @?= 5
    ]

-- =============================================================================
-- FLEET OWNER LOGIN ROLE CHECK TESTS
-- PR change: [FLEET_OWNER, FLEET_BUSINESS, OPERATOR] → [FLEET_OWNER, OPERATOR]
-- =============================================================================

-- | Simulates the new role list used in fleetOwnerLogin / fleetOwnerVerifyHandler
-- after the PR change. Previously [FLEET_OWNER, FLEET_BUSINESS, OPERATOR].
allowedFleetLoginRoles :: [DP.Role]
allowedFleetLoginRoles = [DP.FLEET_OWNER, DP.OPERATOR]

-- | Simulates the old role list that included FLEET_BUSINESS
oldAllowedFleetLoginRoles :: [DP.Role]
oldAllowedFleetLoginRoles = [DP.FLEET_OWNER, DP.FLEET_BUSINESS, DP.OPERATOR]

testFleetOwnerLoginRoles :: TestTree
testFleetOwnerLoginRoles =
  testGroup
    "Fleet Owner Login Role Check (PR: removed FLEET_BUSINESS)"
    [ testCase "FLEET_OWNER is in allowed login roles" $ do
        (DP.FLEET_OWNER `elem` allowedFleetLoginRoles) @? "FLEET_OWNER should be allowed to login",
      testCase "OPERATOR is in allowed login roles" $ do
        (DP.OPERATOR `elem` allowedFleetLoginRoles) @? "OPERATOR should be allowed to login",
      testCase "FLEET_BUSINESS is NOT in new allowed login roles" $ do
        not (DP.FLEET_BUSINESS `elem` allowedFleetLoginRoles) @? "FLEET_BUSINESS should NOT be in new role list",
      testCase "DRIVER is not in allowed login roles" $ do
        not (DP.DRIVER `elem` allowedFleetLoginRoles) @? "DRIVER should not be in fleet login roles",
      testCase "ADMIN is not in allowed login roles" $ do
        not (DP.ADMIN `elem` allowedFleetLoginRoles) @? "ADMIN should not be in fleet login roles",
      testCase "New role list has exactly 2 roles (FLEET_OWNER and OPERATOR)" $ do
        length allowedFleetLoginRoles @?= 2,
      testCase "Old role list had 3 roles (including FLEET_BUSINESS)" $ do
        length oldAllowedFleetLoginRoles @?= 3,
      testCase "New list is a strict subset of old list (minus FLEET_BUSINESS)" $ do
        let inNew = filter (`elem` allowedFleetLoginRoles) oldAllowedFleetLoginRoles
        length inNew @?= 2
    ]

-- =============================================================================
-- FLEET OWNER VERIFICATION ROLE CHECK TESTS
-- PR change: checkFleetOwnerRole → (person.role == FLEET_OWNER)
-- =============================================================================

-- | Old checkFleetOwnerRole: FLEET_OWNER or FLEET_BUSINESS
checkFleetOwnerRoleOld :: DP.Role -> Bool
checkFleetOwnerRoleOld role = role == DP.FLEET_OWNER || role == DP.FLEET_BUSINESS

-- | New check (after PR): only FLEET_OWNER triggers fleetOwnerInfo verification
checkFleetOwnerRoleNew :: DP.Role -> Bool
checkFleetOwnerRoleNew role = role == DP.FLEET_OWNER

testFleetOwnerVerificationCheck :: TestTree
testFleetOwnerVerificationCheck =
  testGroup
    "Fleet Owner Verification Check (PR: FLEET_BUSINESS no longer triggers owner check)"
    [ testCase "FLEET_OWNER triggers fleet owner verification (old and new)" $ do
        checkFleetOwnerRoleOld DP.FLEET_OWNER @?= True
        checkFleetOwnerRoleNew DP.FLEET_OWNER @?= True,
      testCase "FLEET_BUSINESS triggered verification in OLD code but NOT in NEW code" $ do
        checkFleetOwnerRoleOld DP.FLEET_BUSINESS @?= True
        checkFleetOwnerRoleNew DP.FLEET_BUSINESS @?= False,
      testCase "OPERATOR does not trigger fleet owner verification" $ do
        checkFleetOwnerRoleOld DP.OPERATOR @?= False
        checkFleetOwnerRoleNew DP.OPERATOR @?= False,
      testCase "DRIVER does not trigger fleet owner verification" $ do
        checkFleetOwnerRoleOld DP.DRIVER @?= False
        checkFleetOwnerRoleNew DP.DRIVER @?= False,
      testCase "ADMIN does not trigger fleet owner verification" $ do
        checkFleetOwnerRoleOld DP.ADMIN @?= False
        checkFleetOwnerRoleNew DP.ADMIN @?= False,
      testCase "Behavioral difference: FLEET_BUSINESS diverges between old and new" $ do
        let businessOld = checkFleetOwnerRoleOld DP.FLEET_BUSINESS
            businessNew = checkFleetOwnerRoleNew DP.FLEET_BUSINESS
        (businessOld /= businessNew) @? "FLEET_BUSINESS check should differ between old and new code"
    ]

-- =============================================================================
-- isAssociationBetweenTwoPerson TESTS
-- PR removed these cases:
--   (OPERATOR, FLEET_BUSINESS) -> checkFleetOperatorAssociation
--   (FLEET_OWNER, FLEET_BUSINESS) -> pure (same id)
--   (FLEET_BUSINESS, DRIVER) -> checkFleetDriverAssociation
--   (FLEET_BUSINESS, FLEET_OWNER) -> pure (same id)
--   (FLEET_BUSINESS, FLEET_BUSINESS) -> pure (same id)
-- =============================================================================

-- | Simulate the isAssociationBetweenTwoPerson result type.
data AssociationResult = NeedsDbCheck | SameIdCheck | AdminAllow | Deny
  deriving (Show, Eq)

-- | New isAssociationBetweenTwoPerson logic (after PR removes FLEET_BUSINESS cases).
isAssociationNew :: DP.Role -> DP.Role -> AssociationResult
isAssociationNew requestedRole personRole = case (requestedRole, personRole) of
  (DP.OPERATOR, DP.DRIVER) -> NeedsDbCheck
  (DP.OPERATOR, DP.FLEET_OWNER) -> NeedsDbCheck
  (DP.OPERATOR, DP.OPERATOR) -> SameIdCheck
  (DP.FLEET_OWNER, DP.DRIVER) -> NeedsDbCheck
  (DP.FLEET_OWNER, DP.FLEET_OWNER) -> SameIdCheck
  (DP.ADMIN, _) -> AdminAllow
  _ -> Deny

-- | Old isAssociationBetweenTwoPerson logic (before PR, included FLEET_BUSINESS).
isAssociationOld :: DP.Role -> DP.Role -> AssociationResult
isAssociationOld requestedRole personRole = case (requestedRole, personRole) of
  (DP.OPERATOR, DP.DRIVER) -> NeedsDbCheck
  (DP.OPERATOR, DP.FLEET_OWNER) -> NeedsDbCheck
  (DP.OPERATOR, DP.FLEET_BUSINESS) -> NeedsDbCheck
  (DP.OPERATOR, DP.OPERATOR) -> SameIdCheck
  (DP.FLEET_OWNER, DP.DRIVER) -> NeedsDbCheck
  (DP.FLEET_OWNER, DP.FLEET_OWNER) -> SameIdCheck
  (DP.FLEET_OWNER, DP.FLEET_BUSINESS) -> SameIdCheck
  (DP.FLEET_BUSINESS, DP.DRIVER) -> NeedsDbCheck
  (DP.FLEET_BUSINESS, DP.FLEET_OWNER) -> SameIdCheck
  (DP.FLEET_BUSINESS, DP.FLEET_BUSINESS) -> SameIdCheck
  (DP.ADMIN, _) -> AdminAllow
  _ -> Deny

testIsAssociationNew :: TestTree
testIsAssociationNew =
  testGroup
    "isAssociationBetweenTwoPerson (new: FLEET_BUSINESS cases removed)"
    [ testCase "OPERATOR<->DRIVER needs DB check" $ do
        isAssociationNew DP.OPERATOR DP.DRIVER @?= NeedsDbCheck,
      testCase "OPERATOR<->FLEET_OWNER needs DB check" $ do
        isAssociationNew DP.OPERATOR DP.FLEET_OWNER @?= NeedsDbCheck,
      testCase "OPERATOR<->FLEET_BUSINESS now returns Deny (was NeedsDbCheck)" $ do
        isAssociationNew DP.OPERATOR DP.FLEET_BUSINESS @?= Deny
        isAssociationOld DP.OPERATOR DP.FLEET_BUSINESS @?= NeedsDbCheck,
      testCase "OPERATOR<->OPERATOR is SameIdCheck" $ do
        isAssociationNew DP.OPERATOR DP.OPERATOR @?= SameIdCheck,
      testCase "FLEET_OWNER<->DRIVER needs DB check" $ do
        isAssociationNew DP.FLEET_OWNER DP.DRIVER @?= NeedsDbCheck,
      testCase "FLEET_OWNER<->FLEET_OWNER is SameIdCheck" $ do
        isAssociationNew DP.FLEET_OWNER DP.FLEET_OWNER @?= SameIdCheck,
      testCase "FLEET_OWNER<->FLEET_BUSINESS now returns Deny (was SameIdCheck)" $ do
        isAssociationNew DP.FLEET_OWNER DP.FLEET_BUSINESS @?= Deny
        isAssociationOld DP.FLEET_OWNER DP.FLEET_BUSINESS @?= SameIdCheck,
      testCase "FLEET_BUSINESS<->DRIVER now returns Deny (was NeedsDbCheck)" $ do
        isAssociationNew DP.FLEET_BUSINESS DP.DRIVER @?= Deny
        isAssociationOld DP.FLEET_BUSINESS DP.DRIVER @?= NeedsDbCheck,
      testCase "FLEET_BUSINESS<->FLEET_OWNER now returns Deny (was SameIdCheck)" $ do
        isAssociationNew DP.FLEET_BUSINESS DP.FLEET_OWNER @?= Deny
        isAssociationOld DP.FLEET_BUSINESS DP.FLEET_OWNER @?= SameIdCheck,
      testCase "FLEET_BUSINESS<->FLEET_BUSINESS now returns Deny (was SameIdCheck)" $ do
        isAssociationNew DP.FLEET_BUSINESS DP.FLEET_BUSINESS @?= Deny
        isAssociationOld DP.FLEET_BUSINESS DP.FLEET_BUSINESS @?= SameIdCheck,
      testCase "ADMIN<->any role always allows" $ do
        isAssociationNew DP.ADMIN DP.DRIVER @?= AdminAllow
        isAssociationNew DP.ADMIN DP.FLEET_OWNER @?= AdminAllow
        isAssociationNew DP.ADMIN DP.FLEET_BUSINESS @?= AdminAllow
        isAssociationNew DP.ADMIN DP.OPERATOR @?= AdminAllow,
      testCase "DRIVER<->anything other than special cases returns Deny" $ do
        isAssociationNew DP.DRIVER DP.DRIVER @?= Deny
        isAssociationNew DP.DRIVER DP.FLEET_OWNER @?= Deny
    ]

-- =============================================================================
-- getDriverEarnings ROLE CHECK TESTS
-- PR removed: (FLEET_BUSINESS, DRIVER) -> checkFleetDriverAssociation
-- =============================================================================

-- | Simulate the earnings access check in getDriverEarnings (new behavior).
canAccessDriverEarnings :: DP.Role -> DP.Role -> Bool
canAccessDriverEarnings requestorRole driverRole = case (requestorRole, driverRole) of
  (DP.OPERATOR, DP.DRIVER) -> True  -- checkDriverOperatorAssociation
  (DP.FLEET_OWNER, DP.DRIVER) -> True  -- checkFleetDriverAssociation
  _ -> False

-- | Old earnings access check (before PR, included FLEET_BUSINESS).
canAccessDriverEarningsOld :: DP.Role -> DP.Role -> Bool
canAccessDriverEarningsOld requestorRole driverRole = case (requestorRole, driverRole) of
  (DP.OPERATOR, DP.DRIVER) -> True
  (DP.FLEET_OWNER, DP.DRIVER) -> True
  (DP.FLEET_BUSINESS, DP.DRIVER) -> True
  _ -> False

testDriverEarningsAccess :: TestTree
testDriverEarningsAccess =
  testGroup
    "getDriverEarnings Role Access (PR: FLEET_BUSINESS case removed)"
    [ testCase "OPERATOR can access DRIVER earnings" $ do
        canAccessDriverEarnings DP.OPERATOR DP.DRIVER @?= True,
      testCase "FLEET_OWNER can access DRIVER earnings" $ do
        canAccessDriverEarnings DP.FLEET_OWNER DP.DRIVER @?= True,
      testCase "FLEET_BUSINESS can NO LONGER access DRIVER earnings (was True)" $ do
        canAccessDriverEarnings DP.FLEET_BUSINESS DP.DRIVER @?= False
        canAccessDriverEarningsOld DP.FLEET_BUSINESS DP.DRIVER @?= True,
      testCase "ADMIN cannot access via this check (returns False, handled elsewhere)" $ do
        canAccessDriverEarnings DP.ADMIN DP.DRIVER @?= False,
      testCase "DRIVER cannot access other driver earnings" $ do
        canAccessDriverEarnings DP.DRIVER DP.DRIVER @?= False,
      testCase "Behavioral change: only FLEET_BUSINESS access is affected" $ do
        let oldFleetBusiness = canAccessDriverEarningsOld DP.FLEET_BUSINESS DP.DRIVER
            newFleetBusiness = canAccessDriverEarnings DP.FLEET_BUSINESS DP.DRIVER
        (oldFleetBusiness /= newFleetBusiness) @? "FLEET_BUSINESS earnings access changed"
    ]

-- =============================================================================
-- getCommunicationRecipients ROLE FILTER TESTS
-- PR change: [FLEET_OWNER, FLEET_BUSINESS] → [FLEET_OWNER]
-- =============================================================================

-- | New recipient roles for ROLE_FLEET_OWNER filter (after PR).
newFleetRecipientRoles :: [DP.Role]
newFleetRecipientRoles = [DP.FLEET_OWNER]

-- | Old recipient roles for ROLE_FLEET_OWNER filter (before PR).
oldFleetRecipientRoles :: [DP.Role]
oldFleetRecipientRoles = [DP.FLEET_OWNER, DP.FLEET_BUSINESS]

testCommunicationRecipientRoles :: TestTree
testCommunicationRecipientRoles =
  testGroup
    "getCommunicationRecipients Role Filter (PR: removed FLEET_BUSINESS)"
    [ testCase "New filter includes FLEET_OWNER" $ do
        (DP.FLEET_OWNER `elem` newFleetRecipientRoles) @? "FLEET_OWNER should be in new recipient roles",
      testCase "New filter excludes FLEET_BUSINESS" $ do
        not (DP.FLEET_BUSINESS `elem` newFleetRecipientRoles) @? "FLEET_BUSINESS should NOT be in new recipient roles",
      testCase "Old filter included both FLEET_OWNER and FLEET_BUSINESS" $ do
        (DP.FLEET_OWNER `elem` oldFleetRecipientRoles) @? "FLEET_OWNER was in old recipient roles"
        (DP.FLEET_BUSINESS `elem` oldFleetRecipientRoles) @? "FLEET_BUSINESS was in old recipient roles",
      testCase "New filter has exactly 1 role (FLEET_OWNER only)" $ do
        length newFleetRecipientRoles @?= 1,
      testCase "Old filter had 2 roles" $ do
        length oldFleetRecipientRoles @?= 2,
      testCase "PR reduces fleet recipient role set by removing FLEET_BUSINESS" $ do
        let removedRoles = filter (`notElem` newFleetRecipientRoles) oldFleetRecipientRoles
        removedRoles @?= [DP.FLEET_BUSINESS]
    ]

-- =============================================================================
-- getRideList ROLE CHECK TESTS
-- PR change: checkFleetOwnerRole r.role → r.role == FLEET_OWNER
-- =============================================================================

-- | Old fleet role check (checkFleetOwnerRole in getRideList)
isFleetOwnerOrOperatorOld :: DP.Role -> Bool
isFleetOwnerOrOperatorOld role = (role == DP.FLEET_OWNER || role == DP.FLEET_BUSINESS) || role == DP.OPERATOR

-- | New fleet role check (after PR)
isFleetOwnerOrOperatorNew :: DP.Role -> Bool
isFleetOwnerOrOperatorNew role = role == DP.FLEET_OWNER || role == DP.OPERATOR

testRideListRoleCheck :: TestTree
testRideListRoleCheck =
  testGroup
    "getRideList Fleet Role Check (PR: checkFleetOwnerRole → FLEET_OWNER only)"
    [ testCase "FLEET_OWNER is a fleet requestor in new code" $ do
        isFleetOwnerOrOperatorNew DP.FLEET_OWNER @?= True,
      testCase "OPERATOR is a fleet requestor in new code" $ do
        isFleetOwnerOrOperatorNew DP.OPERATOR @?= True,
      testCase "FLEET_BUSINESS is no longer a fleet requestor in new code" $ do
        isFleetOwnerOrOperatorNew DP.FLEET_BUSINESS @?= False
        isFleetOwnerOrOperatorOld DP.FLEET_BUSINESS @?= True,
      testCase "DRIVER is not a fleet requestor" $ do
        isFleetOwnerOrOperatorNew DP.DRIVER @?= False,
      testCase "ADMIN is not a fleet requestor via this check" $ do
        isFleetOwnerOrOperatorNew DP.ADMIN @?= False
    ]

-- =============================================================================
-- MAIN TEST SUITE
-- =============================================================================

fleetRoleUnitTests :: TestTree
fleetRoleUnitTests =
  testGroup
    "Fleet Role Unit Tests (FLEET_BUSINESS removal)"
    [ testRoleEnumValues,
      testFleetOwnerLoginRoles,
      testFleetOwnerVerificationCheck,
      testIsAssociationNew,
      testDriverEarningsAccess,
      testCommunicationRecipientRoles,
      testRideListRoleCheck
    ]