module Domain.Action.Person
  ( BulkCreatePerson (..),
    BulkCreatePersonReq (..),
    BulkCreatePersonResp (..),
    bulkCreate,
  )
where

import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified "lib-dashboard" Domain.Types.AccessMatrix as DMatrix
import qualified "lib-dashboard" Domain.Types.Entity as DE
import qualified "lib-dashboard" Domain.Types.Merchant as DMerchant
import qualified "lib-dashboard" Domain.Types.MerchantAccess as DAccess
import qualified "lib-dashboard" Domain.Types.Person.Type as PT
import qualified "lib-dashboard" Domain.Types.Role as DRole
import qualified "lib-dashboard" Domain.Types.ServerName as DSN
import Kernel.External.Encryption (encrypt, getDbHash)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified "lib-dashboard" Storage.Beam.BeamFlow as BeamFlow
import Storage.Beam.CommonInstances ()
import qualified "lib-dashboard" Storage.Queries.Entity as QE
import qualified "lib-dashboard" Storage.Queries.Merchant as QMerchant
import qualified "lib-dashboard" Storage.Queries.MerchantAccess as QAccess
import qualified "lib-dashboard" Storage.Queries.Person as QP
import qualified "lib-dashboard" Storage.Queries.Role as QRole
import "lib-dashboard" Tools.Auth (TokenInfo (..))
import qualified "lib-dashboard" Tools.Auth.Api as ApiAuth
import "lib-dashboard" Tools.Error

data BulkCreatePerson = BulkCreatePerson
  { firstName :: Maybe Text,
    lastName :: Maybe Text,
    mobileNumber :: Maybe Text,
    mobileCountryCode :: Maybe Text,
    email :: Maybe Text,
    roleName :: Maybe Text,
    entityId :: Maybe Text,
    tokenNo :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data BulkCreatePersonReq = BulkCreatePersonReq
  { operatingCity :: City.City,
    persons :: [BulkCreatePerson]
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data BulkCreatePersonResp = BulkCreatePersonResp
  { totalCount :: Int,
    createdPersonIds :: [Id PT.Person],
    updatedPersonIds :: [Id PT.Person]
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data PersonOp
  = InsertNewPerson PT.Person DAccess.MerchantAccess
  | UpdateExistingPerson PT.Person MerchantAccessAction

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
sanitizeBulkPerson :: BulkCreatePerson -> BulkCreatePerson
sanitizeBulkPerson p =
  BulkCreatePerson
    { firstName = p.firstName >>= nonBlank,
      lastName = p.lastName >>= nonBlank,
      mobileNumber = p.mobileNumber >>= nonBlank,
      mobileCountryCode = p.mobileCountryCode >>= nonBlank,
      email = p.email >>= nonBlank <&> T.toLower,
      roleName = p.roleName >>= nonBlank,
      entityId = p.entityId >>= nonBlank,
      tokenNo = p.tokenNo >>= nonBlank
    }

requireBulkPersonFields :: MonadFlow m => Int -> BulkCreatePerson -> m ()
requireBulkPersonFields idx p = do
  let rowTag = "Row " <> T.pack (show idx) <> ": "
      require label = maybe (throwError (InvalidRequest $ rowTag <> label <> " is missing or blank")) (const (pure ()))
  require "mobileNumber" p.mobileNumber
  require "mobileCountryCode" p.mobileCountryCode
  require "roleName" p.roleName

bulkCreate ::
  ( BeamFlow.BeamFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["passwordExpiryDays" ::: Maybe Int]
  ) =>
  TokenInfo ->
  ShortId DMerchant.Merchant ->
  BulkCreatePersonReq ->
  m BulkCreatePersonResp
bulkCreate tokenInfo merchantShortId req = do
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
  when (length phoneKeys /= HS.size (HS.fromList phoneKeys)) $
    throwError (InvalidRequest "Duplicate mobileCountryCode+mobileNumber within the batch")
  when (length emails /= HS.size (HS.fromList emails)) $
    throwError (InvalidRequest "Duplicate email within the batch")
  -- Per-merchant cross-app lock: keeps validation+write in one critical section across replicas. TTL sized for a 500-row batch.
  let lockKey = "Person:bulkCreate:merchant:" <> merchantShortId.getShortId
      lockTtl = 300
  gotLock <- Redis.withCrossAppRedis $ Redis.tryLockRedis lockKey lockTtl
  unless gotLock $
    throwError (InvalidRequest "Another bulkCreate for this merchant is in progress; retry shortly")
  ops <-
    finally
      ( do
          now <- getCurrentTime
          builtOps <- forM (zip [0 :: Int ..] persons) $ \(idx, p) ->
            resolvePersonOp merchant rolesByName req.operatingCity now idx p
          let inserts = [(pers, acc) | InsertNewPerson pers acc <- builtOps]
          QP.createPersonsWithAccessAtomic inserts
          forM_ builtOps $ \case
            InsertNewPerson _ _ -> pure ()
            UpdateExistingPerson pers accAction -> do
              QP.updatePersonUpsertableFields pers
              case accAction of
                AccessCreate acc -> QAccess.create acc
                AccessUnchanged -> pure ()
          pure builtOps
      )
      (Redis.withCrossAppRedis $ Redis.unlockRedis lockKey)
  let createdIds = [pers.id | InsertNewPerson pers _ <- ops]
      updatedIds = [pers.id | UpdateExistingPerson pers _ <- ops]
  logInfo $
    "[Person.bulkCreate] actor=" <> actorPersonId.getId
      <> " merchant="
      <> merchantShortId.getShortId
      <> " created="
      <> T.pack (show (length createdIds))
      <> " updated="
      <> T.pack (show (length updatedIds))
  pure BulkCreatePersonResp {totalCount = length createdIds + length updatedIds, createdPersonIds = createdIds, updatedPersonIds = updatedIds}

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

resolvePersonOp :: (BeamFlow.BeamFlow m r, EncFlow m r) => DMerchant.Merchant -> M.Map Text DRole.Role -> City.City -> UTCTime -> Int -> BulkCreatePerson -> m PersonOp
resolvePersonOp merchant rolesByName reqCity now idx p = do
  let rowTag = "Row " <> T.pack (show idx) <> ": "
      require label = fromMaybeM (InvalidRequest $ rowTag <> label <> " is missing or blank")
  mobileNumber <- require "mobileNumber" p.mobileNumber
  mobileCountryCode <- require "mobileCountryCode" p.mobileCountryCode
  roleName <- require "roleName" p.roleName
  role <- M.lookup roleName rolesByName & fromMaybeM (InvalidRequest (rowTag <> "role " <> roleName <> " does not exist"))
  mbEntityIdTyped <- resolveEntity rowTag p.entityId
  mbTokenHash <- forM p.tokenNo getDbHash
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
      encryptedEmail <- forM p.email encrypt
      let updated =
            existing
              { PT.firstName = fromMaybe existing.firstName p.firstName,
                PT.lastName = fromMaybe existing.lastName p.lastName,
                PT.roleId = role.id,
                PT.email = maybe existing.email Just encryptedEmail,
                PT.dashboardAccessType = Just role.dashboardAccessType,
                PT.tokenNoHash = maybe existing.tokenNoHash Just mbTokenHash,
                PT.entityId = maybe existing.entityId Just mbEntityIdTyped,
                PT.verified = Just True,
                PT.updatedAt = now
              }
      accessAction <- resolveAccessAction existing.id reqCity
      pure (UpdateExistingPerson updated accessAction)
    Nothing -> do
      whenJust p.email $ \email -> do
        mbEmailOwner <- QP.findByEmail email
        whenJust mbEmailOwner $ \_ ->
          throwError (InvalidRequest (rowTag <> "email " <> email <> " is already registered"))
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
                approvedBy = Nothing,
                rejectedBy = Nothing,
                language = Nothing,
                secretKey = Nothing,
                is2faEnabled = False,
                tokenNoHash = mbTokenHash,
                entityId = mbEntityIdTyped
              }
      access <- buildMerchantAccess merchant reqCity now fresh
      pure (InsertNewPerson fresh access)
  where
    resolveEntity rowTag = \case
      Nothing -> pure Nothing
      Just eid -> do
        let entityIdTyped = Id eid :: Id DE.Entity
        entity <-
          QE.findById entityIdTyped
            >>= fromMaybeM (InvalidRequest (rowTag <> "entity " <> eid <> " does not exist"))
        unless (entity.merchantId == merchant.id) $
          throwError (InvalidRequest (rowTag <> "entity " <> eid <> " does not belong to merchant " <> merchant.shortId.getShortId))
        when entity.deleted $
          throwError (InvalidRequest (rowTag <> "entity " <> eid <> " is soft-deleted; cannot attach new persons to a retired depot"))
        pure (Just entityIdTyped)
    -- Grant is per (person, merchant, city): a person may hold access to multiple cities on the same merchant.
    resolveAccessAction existingPersonId reqCity' = do
      mbExistingAccess <- QAccess.findByPersonIdAndMerchantIdAndCity existingPersonId merchant.id reqCity'
      case mbExistingAccess of
        Just _ -> pure AccessUnchanged
        Nothing -> do
          acc <- buildMerchantAccessForExisting merchant reqCity' now existingPersonId
          pure (AccessCreate acc)

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
