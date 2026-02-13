module SharedLogic.Roles where -----

import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Domain.Types.Role as DRole
import Kernel.Prelude
import Kernel.Types.Id

-- TODO add time metrics for recalculate hierarchy, pure code, queries etc
-- TODO unit tests

-- -----------------------------------------------------------------------------
-- Type synonyms for clarity and maintainability
-- -----------------------------------------------------------------------------

type RoleId = Id DRole.Role

type RoleMap = M.Map RoleId DRole.Role

type ParentMap = M.Map RoleId RoleId

type AncestorCache = M.Map RoleId [RoleId]

type DescendantCache = M.Map RoleId [RoleId]

type ChildrenMap = M.Map RoleId [RoleId]

type HierarchyM = Either CycleDetection

-- | Cycle detection result
data CycleDetection
  = NoCycle
  | CycleDetected [RoleId] -- The path that forms a cycle
  -- deriving (Show, Eq)

-- | Complete role hierarchy information
data RoleHierarchy = RoleHierarchy
  { role :: DRole.Role,
    -- | All parents (up the tree), root has empty list
    roleAncestors :: [DRole.Role],
    -- | All children (down the tree), leaf has empty list
    roleDescendants :: [DRole.Role]
  }

-- | Calculate complete hierarchy for all roles
--   Time complexity: O(n * depth) for ancestor detection, O(n²) worst case for descendants
--   Space complexity: O(n²) for caches
--
--   Returns:
--     - Left CycleDetected if a cycle is found in the parent relationships
--     - Right [RoleHierarchy] for all roles, with ancestors and descendants precomputed
calculateRoleHierarchy :: [DRole.Role] -> Either CycleDetection [RoleHierarchy]
calculateRoleHierarchy allRoles = do
  -- Build essential indexes for O(log n) lookups
  let roleMap = M.fromList [(role.id, role) | role <- allRoles]
  let parentMap =
        M.fromList
          [ (role.id, parentId)
            | role <- allRoles,
              Just parentId <- [role.parentRoleId] -- Nothing values are automatically filtered
          ]

  -- Step 1: Build ancestor cache with integrated cycle detection
  (ancestorCache, childrenMap) <- buildAncestorCache parentMap allRoles

  -- Step 2: Build descendant cache by reversing ancestor relationships
  let descendantCache = buildDescendantCache ancestorCache childrenMap

  -- Step 3: Build final hierarchies for all roles
  mapM (buildRoleHierarchy ancestorCache descendantCache roleMap) allRoles

-- -----------------------------------------------------------------------------
-- Ancestor cache construction (with cycle detection)
-- -----------------------------------------------------------------------------

-- | Build ancestor cache for all roles in a single pass
--   Returns (AncestorCache, ChildrenMap) or fails with CycleDetected
buildAncestorCache :: ParentMap -> [DRole.Role] -> HierarchyM (AncestorCache, ChildrenMap)
buildAncestorCache parentMap = foldM processRole (M.empty, M.empty)
  where
    -- | Process a single role: compute its ancestors and update caches
    --   Skips already processed roles using memoization
    processRole ::
      (AncestorCache, ChildrenMap) ->
      DRole.Role ->
      HierarchyM (AncestorCache, ChildrenMap)
    processRole (cache, childrenAcc) role = do
      case M.lookup role.id cache of
        -- Already processed - skip
        Just _ -> pure (cache, childrenAcc)
        -- Need to compute ancestors
        Nothing -> do
          -- Get all ancestors (automatically fails if cycle detected)
          ancestors <- getAncestors role.id parentMap cache Set.empty
          let newCache = writeAncestorsToCache role.id ancestors cache
          let newChildrenAcc = case role.parentRoleId of
                Nothing -> childrenAcc
                Just parentId -> M.insertWith (++) parentId [role.id] childrenAcc -- M.adjust will not work if key does not exist in Map

          pure (newCache, newChildrenAcc)

    -- | Write ancestors to cache for a role AND recursively for all its ancestors.
    --   This implements cross-caching: computing for one role also caches all roles
    --   in its ancestor chain, maximizing cache reuse.
    writeAncestorsToCache :: RoleId -> [RoleId] -> AncestorCache -> AncestorCache
    writeAncestorsToCache roleId [] cache = M.insert roleId [] cache
    writeAncestorsToCache roleId ancestors@(a:as) cache = writeAncestorsToCache a as (M.insert roleId ancestors cache)

-- | Recursively get all ancestors of a role, detecting cycles
--   Uses Set for O(log n) visited checks instead of O(n) for lists
--   Uses memoization via AncestorCache to avoid redundant computations
getAncestors ::
  RoleId ->
  ParentMap ->
  AncestorCache ->
  -- | Visited roles in current path
  Set.Set RoleId ->
  HierarchyM [RoleId]
getAncestors roleId parentMap cache visited = do
  -- Cycle detection: we've seen this role before in current path
  when (Set.member roleId visited) $
    Left $ CycleDetected (roleId : Set.toList visited)

  case M.lookup roleId parentMap of
    -- Root role: no parent
    Nothing -> pure []
    -- Has parent
    Just parentId -> do
      -- Self-reference cycle
      when (roleId == parentId) $
        Left $ CycleDetected [roleId, parentId]

      -- Try to get from cache first (memoization)
      case M.lookup parentId cache of
        -- Cache hit: parent's ancestors already computed
        Just parentAncestors -> do
          -- Check if current role is in parent's ancestors (cycle)
          when (roleId `elem` parentAncestors) $
            Left $ CycleDetected (roleId : parentId : parentAncestors)
          pure $ parentId : parentAncestors

        -- Cache miss: recursively compute parent's ancestors
        Nothing -> do
          parentAncestors <- getAncestors parentId parentMap cache (Set.insert roleId visited)
          -- Check for cycle after recursion
          when (roleId `elem` parentAncestors) $
            Left $ CycleDetected (roleId : parentId : parentAncestors)
          pure $ parentId : parentAncestors

-- -----------------------------------------------------------------------------
-- Descendant cache construction (reverse index)
-- -----------------------------------------------------------------------------

-- | Build descendant cache by reversing ancestor relationships
--   Time: O(n * avg_ancestors) typically O(n * depth)
--   Space: O(n²) worst case for deep hierarchies
buildDescendantCache :: AncestorCache -> ChildrenMap -> DescendantCache
buildDescendantCache ancestorCache childrenMap =
  let allRoleIds = M.keys ancestorCache

      -- Build reverse index: for each ancestor, add this role as its descendant
      addDescendant :: DescendantCache -> RoleId -> DescendantCache
      addDescendant acc descendantId =
        case M.lookup descendantId ancestorCache of
          Nothing -> acc
          Just ancestors ->
            foldl'
              ( \acc' ancestor ->
                  M.insertWith (++) ancestor [descendantId] acc'
              )
              acc
              ancestors

      -- All transitive descendants (roles that have this role as ancestor)
      transitiveDescendants = foldl' addDescendant M.empty allRoleIds

      -- Merge direct children with transitive descendants, remove duplicates and self
      mergeDescendants :: DescendantCache -> RoleId -> DescendantCache
      mergeDescendants acc roleId =
        let direct = fromMaybe [] $ M.lookup roleId childrenMap
            transitive = fromMaybe [] $ M.lookup roleId transitiveDescendants
            -- Use Set for O(1) duplicate removal
            allDescendantsSet = Set.fromList $ direct ++ transitive
            -- Remove self reference (a role is not its own descendant)
            withoutSelf = Set.delete roleId allDescendantsSet
         in M.insert roleId (Set.toList withoutSelf) acc
   in foldl' mergeDescendants M.empty allRoleIds

-- -----------------------------------------------------------------------------
-- RoleHierarchy construction
-- -----------------------------------------------------------------------------

-- | Build complete RoleHierarchy for a single role
--   Converts cached ancestor/descendant IDs to actual Role objects
buildRoleHierarchy ::
  AncestorCache ->
  DescendantCache ->
  RoleMap ->
  DRole.Role ->
  HierarchyM RoleHierarchy
buildRoleHierarchy ancestorCache descendantCache roleMap role = do
  let ancestorIds = fromMaybe [] $ M.lookup role.id ancestorCache
  let descendantIds = fromMaybe [] $ M.lookup role.id descendantCache

  -- Convert IDs to actual Role objects (O(k) where k = |ancestors|)
  let ancestors = mapMaybe (`M.lookup` roleMap) ancestorIds
  let descendants = mapMaybe (`M.lookup` roleMap) descendantIds

  pure
    RoleHierarchy
      { role = role,
        roleAncestors = ancestors,
        roleDescendants = descendants
      }
