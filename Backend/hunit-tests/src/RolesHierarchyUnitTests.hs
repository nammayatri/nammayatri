{-# LANGUAGE OverloadedStrings #-}

module RolesHierarchyUnitTests where

import Data.Functor ((<&>))
import qualified SharedLogic.Roles as Roles
import SharedLogic.Roles (RoleHierarchy (..))
import qualified Domain.Types.Role as DRole
import Data.List (find)
import Data.Time (UTCTime (..), fromGregorian, getCurrentTime, diffUTCTime)
import System.CPUTime (getCPUTime)
import Kernel.Types.Id (Id (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import "mobility-core" Kernel.Prelude
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as M

-- Helper to create test roles
mkRole :: Id DRole.Role -> Maybe (Id DRole.Role) -> DRole.Role
mkRole rid parent =
  DRole.Role
    { DRole.id = rid,
      DRole.name = T.pack $ show rid,
      DRole.dashboardAccessType = DRole.DASHBOARD_USER,
      DRole.parentRoleId = parent,
      DRole.description = "",
      DRole.createdAt = t,
      DRole.updatedAt = t
    }
  where
    t = UTCTime (fromGregorian 2020 1 1) 0

-- Helper to find hierarchy by role id
findById :: [RoleHierarchy] -> Id DRole.Role -> Maybe RoleHierarchy
findById hierarchies rid = find (\h -> h.role.id == rid) hierarchies

-- Helper to extract role ids from ancestors/descendants
extractIds :: [DRole.Role] -> [Id DRole.Role]
extractIds = map (.id)

-- Helper to convert role ID to String for error messages
showId :: Id DRole.Role -> String
showId = T.unpack . (.getId)

-- -----------------------------------------------------------------------------
-- Comparison helpers (order-independent)
-- -----------------------------------------------------------------------------

-- | Compare two lists of RoleIds ignoring order
assertEqualIdSets :: [Id DRole.Role] -> [Id DRole.Role] -> IO ()
assertEqualIdSets expected actual =
  let expectedSet = Set.fromList expected
      actualSet = Set.fromList actual
   in if expectedSet == actualSet
        then pure ()
        else assertFailure $ "Expected ID set: " ++ show (Set.toList expectedSet) ++ ", but got: " ++ show (Set.toList actualSet)

-- | Compare two RoleHierarchy values (ancestors and descendants compared as sets)
assertEqualHierarchy :: RoleHierarchy -> RoleHierarchy -> IO ()
assertEqualHierarchy expected actual = do
  -- Compare role
  if expected.role.id /= actual.role.id
    then assertFailure $ "Role ID mismatch: expected " ++ showId expected.role.id ++ ", got " ++ showId actual.role.id
    else pure ()

  -- Compare ancestors as sets
  let expectedAncestorIds = Set.fromList $ extractIds expected.roleAncestors
      actualAncestorIds = Set.fromList $ extractIds actual.roleAncestors
  if expectedAncestorIds /= actualAncestorIds
    then assertFailure $ "Ancestors mismatch for role " ++ showId expected.role.id ++ ": expected " ++ show (map showId $ Set.toList expectedAncestorIds) ++ ", got " ++ show (map showId $ Set.toList actualAncestorIds)
    else pure ()

  -- Compare descendants as sets
  let expectedDescendantIds = Set.fromList $ extractIds expected.roleDescendants
      actualDescendantIds = Set.fromList $ extractIds actual.roleDescendants
  if expectedDescendantIds /= actualDescendantIds
    then assertFailure $ "Descendants mismatch for role " ++ showId expected.role.id ++ ": expected " ++ show (map showId $ Set.toList expectedDescendantIds) ++ ", got " ++ show (map showId $ Set.toList actualDescendantIds)
    else pure ()

-- | Compare two lists of RoleHierarchy ignoring order
assertEqualHierarchies :: [RoleHierarchy] -> [RoleHierarchy] -> IO ()
assertEqualHierarchies expected actual = do
  let expectedMap = M.fromList [(h.role.id, h) | h <- expected]
      actualMap = M.fromList [(h.role.id, h) | h <- actual]
      expectedIds = Set.fromList $ map (.role.id) expected
      actualIds = Set.fromList $ map (.role.id) actual

  -- Check if sets of IDs match
  if expectedIds /= actualIds
    then assertFailure $ "Hierarchy ID sets don't match: expected " ++ show (map showId $ Set.toList expectedIds) ++ ", got " ++ show (map showId $ Set.toList actualIds)
    else do
      -- Compare each hierarchy
      forM_ (Set.toList expectedIds) $ \rid -> do
        let Just expectedH = M.lookup rid expectedMap
            Just actualH = M.lookup rid actualMap
        assertEqualHierarchy expectedH actualH

-- -----------------------------------------------------------------------------
-- Test 1: Simple three-level tree
-- -----------------------------------------------------------------------------
testCalculateRoleHierarchySimple :: TestTree
testCalculateRoleHierarchySimple =
  testCase "simple three-level tree" $ do
    let rootId = Id "root"
        childId = Id "child"
        grandChildId = Id "grand-child"

        rootRole = mkRole rootId Nothing
        childRole = mkRole childId (Just rootId)
        grandChildRole = mkRole grandChildId (Just childId)
        roles = [rootRole, childRole, grandChildRole]

    case Roles.calculateRoleHierarchy roles of
      Left cycle -> assertFailure $ "Unexpected cycle: " ++ show cycle
      Right hierarchies -> do
        let Just rootH = findById hierarchies rootId
            Just childH = findById hierarchies childId
            Just grandChildH = findById hierarchies grandChildId

        assertEqualIdSets [] $ extractIds rootH.roleAncestors
        assertEqualIdSets [childId, grandChildId] $ extractIds rootH.roleDescendants

        assertEqualIdSets [rootId] $ extractIds childH.roleAncestors
        assertEqualIdSets [grandChildId] $ extractIds childH.roleDescendants

        assertEqualIdSets [childId, rootId] $ extractIds grandChildH.roleAncestors
        assertEqualIdSets [] $ extractIds grandChildH.roleDescendants

-- -----------------------------------------------------------------------------
-- Test 2: Multiple roots (forest)
-- -----------------------------------------------------------------------------
testCalculateRoleHierarchyMultipleRoots :: TestTree
testCalculateRoleHierarchyMultipleRoots =
  testCase "multiple roots (forest) - two separate trees" $ do
    -- Tree 1: A -> B -> C
    let aId = Id "A"
        bId = Id "B"
        cId = Id "C"

    -- Tree 2: X -> Y
    let xId = Id "X"
        yId = Id "Y"

        aRole = mkRole aId Nothing
        bRole = mkRole bId (Just aId)
        cRole = mkRole cId (Just bId)
        xRole = mkRole xId Nothing
        yRole = mkRole yId (Just xId)

        roles = [aRole, bRole, cRole, xRole, yRole]

    case Roles.calculateRoleHierarchy roles of
      Left cycle -> assertFailure $ "Unexpected cycle: " ++ show cycle
      Right hierarchies -> do
        -- Tree 1 checks
        let Just aH = findById hierarchies aId
            Just bH = findById hierarchies bId
            Just cH = findById hierarchies cId

        assertEqualIdSets [] $ extractIds aH.roleAncestors
        assertEqualIdSets [bId, cId] $ extractIds aH.roleDescendants

        assertEqualIdSets [aId] $ extractIds bH.roleAncestors
        assertEqualIdSets [cId] $ extractIds bH.roleDescendants

        assertEqualIdSets [bId, aId] $ extractIds cH.roleAncestors
        assertEqualIdSets [] $ extractIds cH.roleDescendants

        -- Tree 2 checks
        let Just xH = findById hierarchies xId
            Just yH = findById hierarchies yId

        assertEqualIdSets [] $ extractIds xH.roleAncestors
        assertEqualIdSets [yId] $ extractIds xH.roleDescendants

        assertEqualIdSets [xId] $ extractIds yH.roleAncestors
        assertEqualIdSets [] $ extractIds yH.roleDescendants

-- -----------------------------------------------------------------------------
-- Test 3: Mixed tree (deep and wide)
-- -----------------------------------------------------------------------------
testCalculateRoleHierarchyMixed :: TestTree
testCalculateRoleHierarchyMixed =
  testCase "mixed tree - depth and width combined" $ do
    --         ROOT
    --      /    |    \
    --     A     B     C
    --    / \         / \
    --   D   E       F   G
    --  /               / \
    -- H               I   J
    let rootId = Id "ROOT"
        aId = Id "A"
        bId = Id "B"
        cId = Id "C"
        dId = Id "D"
        eId = Id "E"
        fId = Id "F"
        gId = Id "G"
        hId = Id "H"
        iId = Id "I"
        jId = Id "J"

        rootRole = mkRole rootId Nothing
        aRole = mkRole aId (Just rootId)
        bRole = mkRole bId (Just rootId)
        cRole = mkRole cId (Just rootId)
        dRole = mkRole dId (Just aId)
        eRole = mkRole eId (Just aId)
        fRole = mkRole fId (Just cId)
        gRole = mkRole gId (Just cId)
        hRole = mkRole hId (Just dId)
        iRole = mkRole iId (Just gId)
        jRole = mkRole jId (Just gId)

        roles = [rootRole, aRole, bRole, cRole, dRole, eRole, fRole, gRole, hRole, iRole, jRole]

    case Roles.calculateRoleHierarchy roles of
      Left cycle -> assertFailure $ "Unexpected cycle: " ++ show cycle
      Right hierarchies -> do
        -- ROOT checks
        let Just rootH = findById hierarchies rootId
        assertEqualIdSets [] $ extractIds rootH.roleAncestors
        assertEqualIdSets [aId, bId, cId, dId, eId, fId, gId, hId, iId, jId] $ extractIds rootH.roleDescendants

        -- A checks (has D, E, H as descendants)
        let Just aH = findById hierarchies aId
        assertEqualIdSets [rootId] $ extractIds aH.roleAncestors
        assertEqualIdSets [dId, eId, hId] $ extractIds aH.roleDescendants

        -- C checks (has F, G, I, J as descendants)
        let Just cH = findById hierarchies cId
        assertEqualIdSets [rootId] $ extractIds cH.roleAncestors
        assertEqualIdSets [fId, gId, iId, jId] $ extractIds cH.roleDescendants

        -- H checks (leaf under D)
        let Just hH = findById hierarchies hId
        assertEqualIdSets [dId, aId, rootId] $ extractIds hH.roleAncestors
        assertEqualIdSets [] $ extractIds hH.roleDescendants

        -- G checks (has I, J as descendants)
        let Just gH = findById hierarchies gId
        assertEqualIdSets [cId, rootId] $ extractIds gH.roleAncestors
        assertEqualIdSets [iId, jId] $ extractIds gH.roleDescendants

-- -----------------------------------------------------------------------------
-- Test 4: Self-reference cycle
-- -----------------------------------------------------------------------------
testCalculateRoleHierarchySelfCycle :: TestTree
testCalculateRoleHierarchySelfCycle =
  testCase "cycle detection - self-reference" $ do
    let roleId = Id "self-cycle"
        role = mkRole roleId (Just roleId)  -- Points to itself
        roles = [role]

    case Roles.calculateRoleHierarchy roles of
      Left (Roles.CycleDetected path) -> do
        let expectedPath = [roleId, roleId]
        if path == expectedPath
          then pure ()
          else assertFailure $ "Expected cycle path " ++ show (map showId expectedPath) ++ ", but got " ++ show (map showId path)
      Right _ ->
        assertFailure "Expected cycle detection, but succeeded"

-- -----------------------------------------------------------------------------
-- Test 5: Two-node cycle
-- -----------------------------------------------------------------------------
testCalculateRoleHierarchyTwoNodeCycle :: TestTree
testCalculateRoleHierarchyTwoNodeCycle =
  testCase "cycle detection - two nodes" $ do
    let aId = Id "A"
        bId = Id "B"
        aRole = mkRole aId (Just bId)
        bRole = mkRole bId (Just aId)
        roles = [aRole, bRole]

    case Roles.calculateRoleHierarchy roles of
      Left (Roles.CycleDetected path) -> do
        -- Could be either A->B->A or B->A->B depending on traversal order
        -- We just check that it's a cycle of length 3 (start, middle, start)
        let pathLength = length path
        if pathLength == 3
          then pure ()
          else assertFailure $ "Expected cycle path length 3, but got " ++ show pathLength ++ ". Path: " ++ show (map showId path)

        let first = head path
            last_ = last path
        if first == last_
          then pure ()
          else assertFailure $ "Expected cycle path to start and end with same node, but got start=" ++ showId first ++ ", end=" ++ showId last_ ++ ". Full path: " ++ show (map showId path)
      Right _ ->
        assertFailure "Expected cycle detection, but succeeded"

-- -----------------------------------------------------------------------------
-- Test 6: Three-node cycle
-- -----------------------------------------------------------------------------
testCalculateRoleHierarchyThreeNodeCycle :: TestTree
testCalculateRoleHierarchyThreeNodeCycle =
  testCase "cycle detection - three nodes" $ do
    let aId = Id "A"
        bId = Id "B"
        cId = Id "C"
        aRole = mkRole aId (Just bId)
        bRole = mkRole bId (Just cId)
        cRole = mkRole cId (Just aId)
        roles = [aRole, bRole, cRole]

    case Roles.calculateRoleHierarchy roles of
      Left (Roles.CycleDetected path) -> do
        let pathLength = length path
        if pathLength == 4
          then pure ()
          else assertFailure $ "Expected cycle path length 4 (A,B,C,A), but got " ++ show pathLength ++ ". Path: " ++ show (map showId path)

        let first = head path
            last_ = last path
        if first == last_
          then pure ()
          else assertFailure $ "Expected cycle path to start and end with same node, but got start=" ++ showId first ++ ", end=" ++ showId last_ ++ ". Full path: " ++ show (map showId path)

        -- Verify all three roles are in the cycle
        let uniqueRoles = take 3 path
            expectedRoles = [aId, bId, cId]
        if all (`elem` expectedRoles) uniqueRoles
          then pure ()
          else assertFailure $ "Expected all three roles [A, B, C] in cycle, but got unique roles: " ++ show (map showId uniqueRoles) ++ ". Full path: " ++ show (map showId path) ++ ". Expected roles: " ++ show (map showId expectedRoles)
      Right _ ->
        assertFailure "Expected cycle detection, but succeeded"

-- -----------------------------------------------------------------------------
-- Test 7: Cycle with detached nodes (tree + cycle)
-- -----------------------------------------------------------------------------
testCalculateRoleHierarchyTreeWithCycle :: TestTree
testCalculateRoleHierarchyTreeWithCycle =
  testCase "cycle detection - tree with cycle" $ do
    -- Valid tree: ROOT -> A -> B
    -- Cycle: X -> Y -> Z -> X (separate)
    let rootId = Id "ROOT"
        aId = Id "A"
        bId = Id "B"
        xId = Id "X"
        yId = Id "Y"
        zId = Id "Z"

        rootRole = mkRole rootId Nothing
        aRole = mkRole aId (Just rootId)
        bRole = mkRole bId (Just aId)
        xRole = mkRole xId (Just yId)
        yRole = mkRole yId (Just zId)
        zRole = mkRole zId (Just xId)

        roles = [rootRole, aRole, bRole, xRole, yRole, zRole]

    case Roles.calculateRoleHierarchy roles of
      Left (Roles.CycleDetected path) -> do
        -- Should detect the cycle (X,Y,Z)
        let pathLength = length path
        if pathLength >= 3
          then pure ()
          else assertFailure $ "Expected cycle path length >= 3, but got " ++ show pathLength ++ ". Path: " ++ show (map showId path)

        let first = head path
            last_ = last path
        if first == last_
          then pure ()
          else assertFailure $ "Expected cycle path to start and end with same node, but got start=" ++ showId first ++ ", end=" ++ showId last_ ++ ". Full path: " ++ show (map showId path) ++ ". Expected cycle: X->Y->Z->X"
      Right _ ->
        assertFailure "Expected cycle detection, but succeeded"

-- -----------------------------------------------------------------------------
-- Test 8: Empty list
-- -----------------------------------------------------------------------------
testCalculateRoleHierarchyEmpty :: TestTree
testCalculateRoleHierarchyEmpty =
  testCase "empty role list" $ do
    let roles = []

    case Roles.calculateRoleHierarchy roles of
      Left cycle -> assertFailure $ "Unexpected cycle: " ++ show cycle
      Right hierarchies -> do
        assertEqualHierarchies [] hierarchies

-- -----------------------------------------------------------------------------
-- Test 9: Single role (root)
-- -----------------------------------------------------------------------------
testCalculateRoleHierarchySingleRoot :: TestTree
testCalculateRoleHierarchySingleRoot =
  testCase "single root role" $ do
    let roleId = Id "single"
        role = mkRole roleId Nothing
        roles = [role]

    case Roles.calculateRoleHierarchy roles of
      Left cycle -> assertFailure $ "Unexpected cycle: " ++ show cycle
      Right hierarchies -> do
        let Just h = findById hierarchies roleId
        assertEqualIdSets [] $ extractIds h.roleAncestors
        assertEqualIdSets [] $ extractIds h.roleDescendants

-- -----------------------------------------------------------------------------
-- Test 10: Single role with missing parent (orphan)
-- -----------------------------------------------------------------------------
testCalculateRoleHierarchyOrphan :: TestTree
testCalculateRoleHierarchyOrphan =
  testCase "orphan role - parent doesn't exist" $ do
    let roleId = Id "orphan"
        missingParentId = Id "missing"
        role = mkRole roleId (Just missingParentId)
        roles = [role]

    case Roles.calculateRoleHierarchy roles of
      Left cycle -> assertFailure $ "Unexpected cycle: " ++ show cycle
      Right hierarchies -> do
        let Just h = findById hierarchies roleId
        -- Orphan should have empty ancestors (parent missing treated as no parent?)
        -- This depends on implementation decision
        assertEqualIdSets [] $ extractIds h.roleAncestors
        assertEqualIdSets [] $ extractIds h.roleDescendants

-- -----------------------------------------------------------------------------
-- Test 11: Deep chain (performance test)
-- -----------------------------------------------------------------------------
testCalculateRoleHierarchyDeepChain :: TestTree
testCalculateRoleHierarchyDeepChain =
  testCase "deep chain - 50 levels" $ do
    let depth = 50
        rootId = Id "root"
        -- Create chain: root -> level1 -> level2 -> ... -> level50
        mkChainRole i prevId = mkRole (Id $ "level-" <> T.pack (show i)) (Just prevId)
        rootRole = mkRole rootId Nothing
        roles = rootRole : [mkChainRole i (if i == 1 then rootId else Id $ "level-" <> T.pack (show (i-1))) | i <- [1..depth]]

    -- Measure execution time of calculateRoleHierarchy
    startTime <- getCurrentTime
    startCPUTime <- getCPUTime

    let hierarchyResult = Roles.calculateRoleHierarchy roles

    endTime <- getCurrentTime
    endCPUTime <- getCPUTime

    let realTime = diffUTCTime endTime startTime
        cpuTime = fromIntegral (endCPUTime - startCPUTime) / 1e12 :: Double  -- Convert picoseconds to seconds

    putStrLn @String $ "  [Performance] Deep chain (50 levels):"
    putStrLn @String $ "    Real time: " ++ show realTime ++ "s"
    putStrLn @String $ "    CPU time: " ++ show cpuTime ++ "s"

    -- Now run the assertions
    case hierarchyResult of
      Left cycle -> assertFailure $ "Unexpected cycle: " ++ show cycle
      Right hierarchies -> do
        let Just rootH = findById hierarchies rootId
        -- Root has all 50 descendants
        length rootH.roleDescendants @?= depth

        -- Last level has all 50 ancestors
        let lastId = Id $ "level-" <> T.pack (show depth)
        let Just lastH = findById hierarchies lastId
        length lastH.roleAncestors @?= depth

-- -----------------------------------------------------------------------------
-- Test 12: Very wide tree (performance)
-- -----------------------------------------------------------------------------
testCalculateRoleHierarchyWideTree :: TestTree
testCalculateRoleHierarchyWideTree =
  testCase "wide tree - one root with 100 children" $ do
    let rootId = Id "root"
        childCount = 100
        rootRole = mkRole rootId Nothing
        childRoles = [mkRole (Id $ "child-" <> T.pack (show i)) (Just rootId) | i <- [1..childCount]]
        roles = rootRole : childRoles

    -- Measure execution time of calculateRoleHierarchy
    startTime <- getCurrentTime
    startCPUTime <- getCPUTime

    let hierarchyResult = Roles.calculateRoleHierarchy roles

    endTime <- getCurrentTime
    endCPUTime <- getCPUTime

    let realTime = diffUTCTime endTime startTime
        cpuTime = fromIntegral (endCPUTime - startCPUTime) / 1e12 :: Double  -- Convert picoseconds to seconds

    putStrLn @String $ "  [Performance] Wide tree (100 children):"
    putStrLn @String $ "    Real time: " ++ show realTime ++ "s"
    putStrLn @String $ "    CPU time: " ++ show cpuTime ++ "s"

    -- Now run the assertions
    case hierarchyResult of
      Left cycle -> assertFailure $ "Unexpected cycle: " ++ show cycle
      Right hierarchies -> do
        let Just rootH = findById hierarchies rootId

        -- Root has no ancestors
        assertEqualIdSets [] $ extractIds rootH.roleAncestors

        -- Root has all children as descendants (performance test - check length)
        length rootH.roleDescendants @?= childCount

        -- Each child has root as ancestor and no descendants
        forM_ [1..childCount] $ \i -> do
          let childId = Id $ "child-" <> T.pack (show i)
          let Just childH = findById hierarchies childId
          assertEqualIdSets [rootId] $ extractIds childH.roleAncestors
          assertEqualIdSets [] $ extractIds childH.roleDescendants

-- -----------------------------------------------------------------------------
-- Test 13: Complex performance test (1 -> 5 -> 25 -> 125 -> 625)
-- -----------------------------------------------------------------------------
testCalculateRoleHierarchyComplexPerformance :: TestTree
testCalculateRoleHierarchyComplexPerformance =
  testCase "complex performance - branching factor 5, 4 levels deep" $ do
    let branchingFactor = 5
        maxDepth = 4

        -- Generate role IDs for each level
        -- Level 0: root (1 role) - path [0]
        -- Level 1: 5 roles - paths [0,1], [0,2], [0,3], [0,4], [0,5]
        -- Level 2: 25 roles - paths [0,1,1], [0,1,2], ..., [0,5,5]
        -- Level 3: 125 roles
        -- Level 4: 625 roles
        -- Total: 1 + 5 + 25 + 125 + 625 = 781 roles

        -- Helper to generate role ID for a given path
        -- path is a list of indices: [level0_index, level1_index, ..., levelN_index]
        mkRoleId :: [Int] -> Id DRole.Role
        mkRoleId path = Id $ "role-" <> T.intercalate "-" (map (T.pack . show) path)

        rootPath = [0]
        rootId = mkRoleId rootPath

        -- Generate all role paths recursively
        -- For each level, generate all children of all parents from previous level
        generateAllPaths :: Int -> Int -> [[Int]]
        generateAllPaths currentLevel maxLevel
          | currentLevel == 0 = [[0]]  -- root
          | otherwise =
              let parentPaths = generateAllPaths (currentLevel - 1) maxLevel
               in [parentPath ++ [childIdx]
                   | parentPath <- parentPaths
                   , childIdx <- [1..branchingFactor]]

        allRolePaths = concatMap (\level -> generateAllPaths level maxDepth) [0..maxDepth]

        -- Build parent-child relationships
        buildRoles :: [[Int]] -> [DRole.Role]
        buildRoles [] = []
        buildRoles (path:paths) =
          let roleId = mkRoleId path
              parentId = if path == rootPath
                        then Nothing  -- root has no parent
                        else Just $ mkRoleId (init path)  -- parent is path without last element
           in mkRole roleId parentId : buildRoles paths

        roles = buildRoles allRolePaths
        totalRoles = length roles
        expectedTotal = sum $ map (branchingFactor ^) [0..maxDepth]  -- 1 + 5 + 25 + 125 + 625

    -- Measure execution time of calculateRoleHierarchy
    startTime <- getCurrentTime
    startCPUTime <- getCPUTime

    let hierarchyResult = Roles.calculateRoleHierarchy roles

    endTime <- getCurrentTime
    endCPUTime <- getCPUTime

    let realTime = diffUTCTime endTime startTime
        cpuTime = fromIntegral (endCPUTime - startCPUTime) / 1e12 :: Double  -- Convert picoseconds to seconds

    putStrLn @String $ "  [Performance] Complex hierarchy (branching factor 5, 4 levels):"
    putStrLn @String $ "    Total roles: " ++ show totalRoles ++ " (expected: " ++ show expectedTotal ++ ")"
    putStrLn @String $ "    Real time: " ++ show realTime ++ "s"
    putStrLn @String $ "    CPU time: " ++ show cpuTime ++ "s"

    -- Now run the assertions
    case hierarchyResult of
      Left cycle -> assertFailure $ "Unexpected cycle: " ++ show cycle
      Right hierarchies -> do
        -- Verify total count
        length hierarchies @?= totalRoles

        -- Verify root has all descendants
        let Just rootH = findById hierarchies rootId
        length rootH.roleDescendants @?= (totalRoles - 1)  -- All except root itself

        -- Verify root has no ancestors
        assertEqualIdSets [] $ extractIds rootH.roleAncestors

        -- Verify a sample role from each level
        -- Helper to create role ID from path (same as mkRoleId but accessible here)
        let mkId path = Id $ "role-" <> T.intercalate "-" (map (T.pack . show) path)

        -- Level 1: role-0-1 (first child of root)
        let level1Id = mkId [0, 1]
        let Just level1H = findById hierarchies level1Id
        assertEqualIdSets [rootId] $ extractIds level1H.roleAncestors
        length level1H.roleDescendants @?= branchingFactor + (branchingFactor ^ 2) + (branchingFactor ^ 3)  -- 5 + 25 + 125 = 155

        -- Level 2: role-0-1-1 (first child of first level-1 role)
        let level2Id = mkId [0, 1, 1]
        let Just level2H = findById hierarchies level2Id
        assertEqualIdSets [level1Id, rootId] $ extractIds level2H.roleAncestors
        length level2H.roleDescendants @?= branchingFactor + (branchingFactor ^ 2)  -- 5 + 25 = 30

        -- Level 3: role-0-1-1-1
        let level3Id = mkId [0, 1, 1, 1]
        let Just level3H = findById hierarchies level3Id
        assertEqualIdSets [level2Id, level1Id, rootId] $ extractIds level3H.roleAncestors
        length level3H.roleDescendants @?= branchingFactor  -- 5

        -- Level 4: role-0-1-1-1-1 (leaf)
        let level4Id = mkId [0, 1, 1, 1, 1]
        let Just level4H = findById hierarchies level4Id
        assertEqualIdSets [level3Id, level2Id, level1Id, rootId] $ extractIds level4H.roleAncestors
        assertEqualIdSets [] $ extractIds level4H.roleDescendants  -- Leaf has no descendants

-- -----------------------------------------------------------------------------
-- Main test group
-- -----------------------------------------------------------------------------
rolesHierarchyUnitTests :: TestTree
rolesHierarchyUnitTests =
  testGroup
    "SharedLogic.Roles.calculateRoleHierarchy"
    [ testCalculateRoleHierarchySimple
    , testCalculateRoleHierarchyMultipleRoots
    , testCalculateRoleHierarchyMixed
    , testCalculateRoleHierarchySelfCycle
    , testCalculateRoleHierarchyTwoNodeCycle
    , testCalculateRoleHierarchyThreeNodeCycle
    , testCalculateRoleHierarchyTreeWithCycle
    , testCalculateRoleHierarchyEmpty
    , testCalculateRoleHierarchySingleRoot
    , testCalculateRoleHierarchyOrphan
    , testCalculateRoleHierarchyDeepChain
    , testCalculateRoleHierarchyWideTree
    , testCalculateRoleHierarchyComplexPerformance
    ]