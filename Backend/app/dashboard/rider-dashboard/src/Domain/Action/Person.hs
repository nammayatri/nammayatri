module Domain.Action.Person
  ( BulkUpsertPerson (..),
    BulkUpsertPersonReq (..),
    BulkUpsertPersonResp (..),
    bulkUpsert,
  )
where

import Data.Containers.ListUtils (nubOrd)
import qualified Data.HashSet as HS
import Data.List ((\\))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified "lib-dashboard" Domain.Types.AccessMatrix as DMatrix
import qualified "lib-dashboard" Domain.Types.Entity as DE
import qualified "lib-dashboard" Domain.Types.EntityAccess as DEA
import qualified "lib-dashboard" Domain.Types.Merchant as DMerchant
import qualified "lib-dashboard" Domain.Types.MerchantAccess as DAccess
import qualified "lib-dashboard" Domain.Types.Person.Type as PT
import qualified "lib-dashboard" Domain.Types.Role as DRole
import qualified "lib-dashboard" Domain.Types.ServerName as DSN
import Kernel.External.Encryption (DbHash, EncKind (..), EncryptedHashedField, encrypt)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified "lib-dashboard" Storage.Beam.BeamFlow as BeamFlow
import Storage.Beam.CommonInstances ()
import qualified "lib-dashboard" Storage.Queries.Entity as QE
import qualified "lib-dashboard" Storage.Queries.EntityAccess as QEA
import qualified "lib-dashboard" Storage.Queries.Merchant as QMerchant
import qualified "lib-dashboard" Storage.Queries.MerchantAccess as QAccess
import qualified "lib-dashboard" Storage.Queries.Person as QP
import qualified "lib-dashboard" Storage.Queries.Role as QRole
import "lib-dashboard" Tools.Auth (TokenInfo (..))
import qualified "lib-dashboard" Tools.Auth.Api as ApiAuth
import "lib-dashboard" Tools.Error

data BulkUpsertPerson = BulkUpsertPerson
  { firstName :: Maybe Text,
    lastName :: Maybe Text,
    mobileNumber :: Maybe Text,
    mobileCountryCode :: Maybe Text,
    email :: Maybe Text,
    roleName :: Maybe Text,
    -- | Deprecated single-depot field, still accepted so existing CSV payloads keep working.
    -- Merged into 'entityIds'; with one depot the two are equivalent.
    entityId :: Maybe Text,
    -- | Three-state: absent leaves existing grants alone, [] revokes every grant under this
    -- merchant, non-empty replaces them. Absent-means-unchanged is what keeps a payload that
    -- names neither field behaving exactly as it did before entity grants existed.
    entityIds :: Maybe [Text],
    tokenNo :: Maybe Text,
    vpa :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data BulkUpsertPersonReq = BulkUpsertPersonReq
  { operatingCity :: City.City,
    persons :: [BulkUpsertPerson]
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data BulkUpsertPersonResp = BulkUpsertPersonResp
  { totalCount :: Int,
    createdPersonIds :: [Id PT.Person],
    updatedPersonIds :: [Id PT.Person]
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- The entity payload is the desired grant set for this merchant; Nothing means the row said
-- nothing about depots and existing grants must survive untouched.
data PersonOp
  = InsertNewPerson PT.Person DAccess.MerchantAccess [Id DE.Entity]
  | UpdateExistingPerson PT.Person MerchantAccessAction (Maybe [Id DE.Entity])

data MerchantAccessAction
  = AccessCreate DAccess.MerchantAccess
  | AccessUnchanged

maxBulkPersons :: Int
maxBulkPersons = 500

nonBlank :: Text -> Maybe Text
nonBlank t = case T.strip t of
  "" -> Nothing
  s -> Just s

-- email is lowercased so storage-side hash (raw bytes) and findByEmail (lowercases before hash) agree.
sanitizeBulkPerson :: BulkUpsertPerson -> BulkUpsertPerson
sanitizeBulkPerson p =
  BulkUpsertPerson
    { firstName = p.firstName >>= nonBlank,
      lastName = p.lastName >>= nonBlank,
      mobileNumber = p.mobileNumber >>= nonBlank,
      mobileCountryCode = p.mobileCountryCode >>= nonBlank,
      email = p.email >>= nonBlank <&> T.toLower,
      roleName = p.roleName >>= nonBlank,
      entityId = p.entityId >>= nonBlank,
      entityIds = p.entityIds <&> (nubOrd . mapMaybe nonBlank),
      tokenNo = p.tokenNo >>= nonBlank,
      vpa = p.vpa >>= nonBlank
    }

requireBulkPersonFields :: MonadFlow m => Int -> BulkUpsertPerson -> m ()
requireBulkPersonFields idx p = do
  let rowTag = "Row " <> T.pack (show idx) <> ": "
      require label = maybe (throwError (InvalidRequest $ rowTag <> label <> " is missing or blank")) (const (pure ()))
  require "mobileNumber" p.mobileNumber
  require "mobileCountryCode" p.mobileCountryCode
  require "roleName" p.roleName

bulkUpsert ::
  ( BeamFlow.BeamFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["passwordExpiryDays" ::: Maybe Int]
  ) =>
  TokenInfo ->
  ShortId DMerchant.Merchant ->
  BulkUpsertPersonReq ->
  m BulkUpsertPersonResp
bulkUpsert tokenInfo merchantShortId req = do
  let actorPersonId = tokenInfo.personId
  actorPerson <-
    ApiAuth.verifyAccessLevel
      DMatrix.ApiAccessLevel
        { serverName = DSN.APP_BACKEND_MANAGEMENT,
          apiEntity = DMatrix.DSL,
          userActionType = DMatrix.DASHBOARD_USER_BULK_CREATE
        }
      actorPersonId
  let total = length req.persons
  when (total == 0) $
    throwError (InvalidRequest "persons array is empty")
  when (total > maxBulkPersons) $
    throwError (InvalidRequest $ "persons exceeds per-request cap of " <> T.pack (show maxBulkPersons) <> " rows; split the CSV")
  -- CSV upload wire: every field is Maybe; sanitize collapses `Just ""` -> Nothing so required-field checks work.
  persons <- forM (zip [0 :: Int ..] req.persons) $ \(idx, p) -> do
    let p' = sanitizeBulkPerson p
    requireBulkPersonFields idx p'
    pure p'
  merchant <-
    QMerchant.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  ApiAuth.verifyCity merchant req.operatingCity
  actorAccess <- QAccess.findByPersonIdAndMerchantId actorPersonId merchant.id
  when (null actorAccess) $
    throwError AccessDenied
  actorRole <- QRole.findById actorPerson.roleId >>= fromMaybeM (RoleDoesNotExist actorPerson.roleId.getId)
  -- Batch role lookup: one findAllByName instead of 2*N (was called both here and inside resolvePersonOp per row).
  let requestedRoleNames = HS.toList $ HS.fromList $ mapMaybe (.roleName) persons
  foundRoles <- QRole.findAllByName requestedRoleNames
  let rolesByName = M.fromList [(r.name, r) | r <- foundRoles]
  -- Admin bypasses accessibleRoles gate; enumerating every assignable role doesn't scale (same policy as Roles.listV2).
  let isAdmin = actorRole.dashboardAccessType == DRole.DASHBOARD_ADMIN
      allowedRoleIds = actorRole.accessibleRoles
  forM_ (zip [0 :: Int ..] persons) $ \(idx, p) -> do
    let rowTag = "Row " <> T.pack (show idx) <> ": "
    rn <- fromMaybeM (InvalidRequest $ rowTag <> "roleName is missing or blank") p.roleName
    r <- M.lookup rn rolesByName & fromMaybeM (InvalidRequest $ rowTag <> "role " <> rn <> " does not exist")
    unless (isAdmin || r.id `elem` allowedRoleIds) $
      throwError (InvalidRequest $ rowTag <> "role " <> rn <> " is not assignable by your account")
  -- HashSet dedup: O(N) vs Data.List.nub's O(N^2).
  let phoneKeys = mapMaybe (\p -> (,) <$> p.mobileCountryCode <*> p.mobileNumber) persons
      emails = mapMaybe (.email) persons
      tokenNos = mapMaybe (.tokenNo) persons
  when (length phoneKeys /= HS.size (HS.fromList phoneKeys)) $
    throwError (InvalidRequest "Duplicate mobileCountryCode+mobileNumber within the batch")
  when (length emails /= HS.size (HS.fromList emails)) $
    throwError (InvalidRequest "Duplicate email within the batch")
  when (length tokenNos /= HS.size (HS.fromList tokenNos)) $
    throwError (InvalidRequest "Duplicate tokenNo within the batch")
  -- Encrypt before taking the lock: a passetto failure mid-batch would otherwise strand the
  -- lock until its TTL, since the release handler is installed further down. The conflict
  -- IN-query below consumes these hashes.
  encryptedTokenNos <- forM persons $ \p -> forM p.tokenNo encrypt
  encryptedVpas <- forM persons $ \p -> forM p.vpa encrypt
  -- Per-merchant cross-app lock: keeps validation+write in one critical section across replicas. TTL sized for a 500-row batch.
  -- Key kept as bulkCreate: renaming it would stop an old binary mid-rollout from excluding a new one.
  let lockKey = "Person:bulkCreate:merchant:" <> merchantShortId.getShortId
      lockTtl = 300
  gotLock <- Redis.withCrossAppRedis $ Redis.tryLockRedis lockKey lockTtl
  unless gotLock $
    throwError (InvalidRequest "Another bulkUpsert for this merchant is in progress; retry shortly")
  ops <-
    finally
      ( do
          now <- getCurrentTime
          conflicts <- QP.findTokenNoConflictsForMerchant merchant.id $ mapMaybe (fmap (.hash)) encryptedTokenNos
          builtOps <- forM (zip3 [0 :: Int ..] persons (zip encryptedTokenNos encryptedVpas)) $ \(idx, p, (mbTokenEnc, mbVpaEnc)) ->
            resolvePersonOp merchant rolesByName conflicts req.operatingCity now idx p mbTokenEnc mbVpaEnc
          let inserts = [(pers, acc) | InsertNewPerson pers acc _ <- builtOps]
          QP.createPersonsWithAccessAtomic inserts
          forM_ builtOps $ \case
            InsertNewPerson _ _ _ -> pure ()
            UpdateExistingPerson pers accAction _ -> do
              QP.updatePersonUpsertableFields pers
              case accAction of
                AccessCreate acc -> QAccess.create acc
                AccessUnchanged -> pure ()
          syncEntityGrants merchant.id now builtOps
          pure builtOps
      )
      (Redis.withCrossAppRedis $ Redis.unlockRedis lockKey)
  let createdIds = [pers.id | InsertNewPerson pers _ _ <- ops]
      updatedIds = [pers.id | UpdateExistingPerson pers _ _ <- ops]
  logInfo $
    "[Person.bulkUpsert] actor=" <> actorPersonId.getId
      <> " merchant="
      <> merchantShortId.getShortId
      <> " created="
      <> T.pack (show (length createdIds))
      <> " updated="
      <> T.pack (show (length updatedIds))
  pure BulkUpsertPersonResp {totalCount = length createdIds + length updatedIds, createdPersonIds = createdIds, updatedPersonIds = updatedIds}

buildMerchantAccess :: MonadFlow m => DMerchant.Merchant -> City.City -> UTCTime -> PT.Person -> m DAccess.MerchantAccess
buildMerchantAccess merchant city now person = do
  accessId <- generateGUID
  pure
    DAccess.MerchantAccess
      { id = accessId,
        merchantId = merchant.id,
        merchantShortId = merchant.shortId,
        personId = person.id,
        createdAt = now,
        operatingCity = city
      }

resolvePersonOp ::
  (BeamFlow.BeamFlow m r, EncFlow m r) =>
  DMerchant.Merchant ->
  M.Map Text DRole.Role ->
  [(DbHash, Id PT.Person)] ->
  City.City ->
  UTCTime ->
  Int ->
  BulkUpsertPerson ->
  Maybe (EncryptedHashedField 'AsEncrypted Text) ->
  Maybe (EncryptedHashedField 'AsEncrypted Text) ->
  m PersonOp
resolvePersonOp merchant rolesByName conflicts reqCity now idx p mbTokenEncrypted mbVpaEncrypted = do
  let rowTag = "Row " <> T.pack (show idx) <> ": "
      require label = fromMaybeM (InvalidRequest $ rowTag <> label <> " is missing or blank")
  mobileNumber <- require "mobileNumber" p.mobileNumber
  mobileCountryCode <- require "mobileCountryCode" p.mobileCountryCode
  roleName <- require "roleName" p.roleName
  role <- M.lookup roleName rolesByName & fromMaybeM (InvalidRequest (rowTag <> "role " <> roleName <> " does not exist"))
  -- Legacy entityId first, so it stays the primary entity behind the deprecated scalars.
  let mbRequestedEntityIds = case (p.entityId, p.entityIds) of
        (Nothing, mbIds) -> mbIds
        (Just eid, mbIds) -> Just (nubOrd (eid : fromMaybe [] mbIds))
  mbEntityIdsTyped <- forM mbRequestedEntityIds (resolveEntities rowTag)
  -- findByMobileNumber is global; tenant check below prevents a caller from silently mutating another merchant's user.
  mbExistingByMobile <- QP.findByMobileNumber mobileNumber mobileCountryCode
  case mbExistingByMobile of
    Just existing -> do
      existingAccess <- QAccess.findByPersonIdAndMerchantId existing.id merchant.id
      when (null existingAccess) $
        throwError
          ( InvalidRequest $
              rowTag <> "mobileNumber " <> mobileNumber
                <> " is already registered under a different merchant"
          )
      whenJust p.email $ \email -> do
        mbEmailOwner <- QP.findByEmail email
        whenJust mbEmailOwner $ \owner ->
          when (owner.id /= existing.id) $
            throwError (InvalidRequest (rowTag <> "email " <> email <> " is registered to a different person"))
      whenJust mbTokenEncrypted $ \tokenEnc ->
        QP.requireTokenNoFree conflicts tokenEnc.hash (Just existing.id) rowTag
      encryptedEmail <- forM p.email encrypt
      let updated =
            existing
              { PT.firstName = fromMaybe existing.firstName p.firstName,
                PT.lastName = fromMaybe existing.lastName p.lastName,
                PT.roleId = role.id,
                PT.email = maybe existing.email Just encryptedEmail,
                PT.dashboardAccessType = Just role.dashboardAccessType,
                PT.tokenNo = maybe existing.tokenNo Just mbTokenEncrypted,
                PT.vpa = maybe existing.vpa Just mbVpaEncrypted,
                PT.verified = Just True,
                PT.updatedAt = now
              }
      accessAction <- resolveAccessAction existing.id reqCity
      pure (UpdateExistingPerson updated accessAction mbEntityIdsTyped)
    Nothing -> do
      whenJust p.email $ \email -> do
        mbEmailOwner <- QP.findByEmail email
        whenJust mbEmailOwner $ \_ ->
          throwError (InvalidRequest (rowTag <> "email " <> email <> " is already registered"))
      whenJust mbTokenEncrypted $ \tokenEnc ->
        QP.requireTokenNoFree conflicts tokenEnc.hash Nothing rowTag
      personId <- generateGUID
      encryptedMobileNumber <- encrypt mobileNumber
      encryptedEmail <- forM p.email encrypt
      let fresh =
            PT.Person
              { id = personId,
                firstName = fromMaybe "" p.firstName,
                lastName = fromMaybe "" p.lastName,
                roleId = role.id,
                email = encryptedEmail,
                mobileNumber = encryptedMobileNumber,
                mobileCountryCode = mobileCountryCode,
                passwordHash = Nothing,
                dashboardAccessType = Just role.dashboardAccessType,
                dashboardType = PT.DEFAULT_DASHBOARD,
                receiveNotification = Nothing,
                createdAt = now,
                updatedAt = now,
                verified = Just True,
                rejectionReason = Nothing,
                rejectedAt = Nothing,
                passwordUpdatedAt = Nothing,
                forcePasswordChange = Nothing,
                merchantId = Just merchant.id,
                approvedBy = Nothing,
                rejectedBy = Nothing,
                language = Nothing,
                secretKey = Nothing,
                is2faEnabled = False,
                tokenNo = mbTokenEncrypted,
                vpa = mbVpaEncrypted
              }
      access <- buildMerchantAccess merchant reqCity now fresh
      pure (InsertNewPerson fresh access (fromMaybe [] mbEntityIdsTyped))
  where
    -- One row's depots are bounded by the entity table (~35 per merchant), so validating each
    -- individually is fine; an unknown or foreign depot must fail the row rather than be dropped.
    resolveEntities rowTag = mapM $ \eid -> do
      let entityIdTyped = Id eid :: Id DE.Entity
      entity <-
        QE.findById entityIdTyped
          >>= fromMaybeM (InvalidRequest (rowTag <> "entity " <> eid <> " does not exist"))
      unless (entity.merchantId == merchant.id) $
        throwError (InvalidRequest (rowTag <> "entity " <> eid <> " does not belong to merchant " <> merchant.shortId.getShortId))
      when entity.deleted $
        throwError (InvalidRequest (rowTag <> "entity " <> eid <> " is soft-deleted; cannot attach new persons to a retired depot"))
      pure entityIdTyped
    -- Grant is per (person, merchant, city): a person may hold access to multiple cities on the same merchant.
    resolveAccessAction existingPersonId reqCity' = do
      mbExistingAccess <- QAccess.findByPersonIdAndMerchantIdAndCity existingPersonId merchant.id reqCity'
      case mbExistingAccess of
        Just _ -> pure AccessUnchanged
        Nothing -> do
          acc <- buildMerchantAccessForExisting merchant reqCity' now existingPersonId
          pure (AccessCreate acc)

-- Diffed against what the person already holds under THIS merchant: a row naming a depot they
-- already manage is a no-op, and grants under other merchants are never read nor revoked. Runs
-- inside the per-merchant lock, so the read-modify-write cannot lose a concurrent grant.
syncEntityGrants :: BeamFlow.BeamFlow m r => Id DMerchant.Merchant -> UTCTime -> [PersonOp] -> m ()
syncEntityGrants merchantId now ops = do
  let targets = mapMaybe desiredGrants ops
  existing <- QEA.findAllByPersonIdsAndMerchantId (fst <$> targets) merchantId
  let heldByPerson = M.fromListWith (<>) [(g.personId, [g.entityId]) | g <- existing]
  forM_ targets $ \(personId, desired) -> do
    let held = M.findWithDefault [] personId heldByPerson
    QEA.deleteByPersonIdAndEntityIds personId merchantId (held \\ desired)
    forM_ (desired \\ held) $ \entityId -> do
      grantId <- generateGUID
      QEA.create DEA.EntityAccess {id = grantId, personId = personId, entityId = entityId, merchantId = merchantId, createdAt = now}
  where
    desiredGrants = \case
      InsertNewPerson pers _ entityIds -> Just (pers.id, entityIds)
      UpdateExistingPerson pers _ mbEntityIds -> (pers.id,) <$> mbEntityIds

buildMerchantAccessForExisting :: MonadFlow m => DMerchant.Merchant -> City.City -> UTCTime -> Id PT.Person -> m DAccess.MerchantAccess
buildMerchantAccessForExisting merchant city now personId = do
  accessId <- generateGUID
  pure
    DAccess.MerchantAccess
      { id = accessId,
        merchantId = merchant.id,
        merchantShortId = merchant.shortId,
        personId = personId,
        createdAt = now,
        operatingCity = city
      }
