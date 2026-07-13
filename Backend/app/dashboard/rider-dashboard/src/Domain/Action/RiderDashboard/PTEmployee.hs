module Domain.Action.RiderDashboard.PTEmployee
  ( BulkCreatePerson (..),
    BulkCreatePersonReq (..),
    BulkCreatePersonResp (..),
    bulkCreate,
  )
where

import Data.List (nub)
import qualified Data.Text as T
import qualified "lib-dashboard" Domain.Types.Entity as DE
import qualified "lib-dashboard" Domain.Types.Merchant as DMerchant
import qualified "lib-dashboard" Domain.Types.MerchantAccess as DAccess
import qualified "lib-dashboard" Domain.Types.Person as DP
import qualified "lib-dashboard" Domain.Types.Person.Type as PT
import qualified "lib-dashboard" Domain.Types.Role as DRole
import Kernel.External.Encryption (encrypt, getDbHash)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import "lib-dashboard" Storage.Beam.BeamFlow
import qualified "lib-dashboard" Storage.Queries.Entity as QE
import qualified "lib-dashboard" Storage.Queries.Merchant as QMerchant
import qualified "lib-dashboard" Storage.Queries.MerchantAccess as QAccess
import qualified "lib-dashboard" Storage.Queries.Person as QP
import qualified "lib-dashboard" Storage.Queries.Role as QRole
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
  { merchantId :: ShortId DMerchant.Merchant,
    operatingCity :: City.City,
    persons :: [BulkCreatePerson]
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data BulkCreatePersonResp = BulkCreatePersonResp
  { totalCount :: Int,
    createdPersonIds :: [Id PT.Person]
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

maxBulkPersons :: Int
maxBulkPersons = 500

nonBlank :: Text -> Maybe Text
nonBlank t = case T.strip t of
  "" -> Nothing
  s -> Just s

-- Strip + collapse Just "" → Nothing. Mirrors sanitizeLoginReq semantics.
sanitizeBulkPerson :: BulkCreatePerson -> BulkCreatePerson
sanitizeBulkPerson p =
  BulkCreatePerson
    { firstName = p.firstName >>= nonBlank,
      lastName = p.lastName >>= nonBlank,
      mobileNumber = p.mobileNumber >>= nonBlank,
      mobileCountryCode = p.mobileCountryCode >>= nonBlank,
      email = p.email >>= nonBlank,
      roleName = p.roleName >>= nonBlank,
      entityId = p.entityId >>= nonBlank,
      tokenNo = p.tokenNo >>= nonBlank
    }

-- Only auth-critical fields are required; firstName/lastName/entityId/email are optional.
requireBulkPersonFields :: MonadFlow m => Int -> BulkCreatePerson -> m ()
requireBulkPersonFields idx p = do
  let rowTag = "Row " <> T.pack (show idx) <> ": "
      require label = maybe (throwError (InvalidRequest $ rowTag <> label <> " is missing or blank")) (const (pure ()))
  require "mobileNumber" p.mobileNumber         -- login composite key
  require "mobileCountryCode" p.mobileCountryCode -- login composite key
  require "tokenNo" p.tokenNo                   -- login credential
  require "roleName" p.roleName                 -- Person.roleId is NOT NULL

bulkCreate :: (BeamFlow m r, EncFlow m r) => Id DP.Person -> BulkCreatePersonReq -> m BulkCreatePersonResp
bulkCreate actorPersonId req = do
  let total = length req.persons
  when (total == 0) $
    throwError (InvalidRequest "persons array is empty")
  when (total > maxBulkPersons) $
    throwError (InvalidRequest $ "persons exceeds per-request cap of " <> T.pack (show maxBulkPersons) <> " rows; split the CSV")
  -- Wire type accepts every field as Maybe. Sanitize (strip + Just "" → Nothing)
  -- and immediately enforce required fields so the row is fully-formed before
  -- any DB work — mirrors sanitizeLoginReq's semantics.
  persons <- forM (zip [0 :: Int ..] req.persons) $ \(idx, p) -> do
    let p' = sanitizeBulkPerson p
    requireBulkPersonFields idx p'
    pure p'
  merchant <-
    QMerchant.findByShortId req.merchantId
      >>= fromMaybeM (MerchantDoesNotExist req.merchantId.getShortId)
  -- S1: authorization — actor must have merchant_access to this merchant AND
  -- their role must be permitted to assign each row's requested role.
  actorAccess <- QAccess.findByPersonIdAndMerchantId actorPersonId merchant.id
  when (null actorAccess) $
    throwError AccessDenied
  actorPerson <- QP.findById actorPersonId >>= fromMaybeM (PersonDoesNotExist actorPersonId.getId)
  actorRole <- QRole.findById actorPerson.roleId >>= fromMaybeM (RoleDoesNotExist actorPerson.roleId.getId)
  -- DASHBOARD_ADMIN is a superuser tier; enumerating every assignable role in
  -- accessibleRoles doesn't scale as new roles are added, so admins bypass the
  -- role-scope gate. Same policy as Roles.listV2.
  let isAdmin = actorRole.dashboardAccessType == DRole.DASHBOARD_ADMIN
      allowedRoleIds = actorRole.accessibleRoles
  forM_ (zip [0 :: Int ..] persons) $ \(idx, p) -> do
    let rowTag = "Row " <> T.pack (show idx) <> ": "
    rn <- fromMaybeM (InvalidRequest $ rowTag <> "roleName is missing or blank") p.roleName
    r <- QRole.findByName rn >>= fromMaybeM (InvalidRequest $ rowTag <> "role " <> rn <> " does not exist")
    unless (isAdmin || r.id `elem` allowedRoleIds) $
      throwError (InvalidRequest $ rowTag <> "role " <> rn <> " is not assignable by your account")
  -- Required fields are Just at this point (requireBulkPersonFields threw otherwise);
  -- the Applicative extraction below drops nothing.
  let phoneKeys = mapMaybe (\p -> (,) <$> p.mobileCountryCode <*> p.mobileNumber) persons
      emails = mapMaybe (.email) persons
  when (length phoneKeys /= length (nub phoneKeys)) $
    throwError (InvalidRequest "Duplicate mobileCountryCode+mobileNumber within the batch")
  when (length emails /= length (nub (map T.toLower emails))) $
    throwError (InvalidRequest "Duplicate email within the batch")
  -- Serialize concurrent bulkCreate for the SAME MERCHANT so validation +
  -- write happen inside one critical section; TTL sized to worst-case 500-row
  -- batch; withCrossAppRedis so the lock is visible across replicas.
  let lockKey = "PTEmployee:bulkCreate:merchant:" <> req.merchantId.getShortId
      lockTtl = 300 -- seconds
  gotLock <- Redis.withCrossAppRedis $ Redis.tryLockRedis lockKey lockTtl
  unless gotLock $
    throwError (InvalidRequest "Another bulkCreate for this merchant is in progress; retry shortly")
  pairs <-
    finally
      ( do
          -- Validation MUST live inside the lock so a second caller waiting
          -- for the lock sees the first caller's committed writes when it
          -- re-runs findByMobileNumber / findByEmail.
          now <- getCurrentTime
          builtPairs <- forM (zip [0 :: Int ..] persons) $ \(idx, p) -> do
            person <- validateAndBuildPerson merchant now idx p
            access <- buildMerchantAccess merchant req.operatingCity now person
            pure (person, access)
          QP.createPersonsWithAccessAtomic builtPairs
          pure builtPairs
      )
      (Redis.withCrossAppRedis $ Redis.unlockRedis lockKey)
  let ids = map ((.id) . fst) pairs
  logInfo $
    "[PTEmployee.bulkCreate] actor=" <> actorPersonId.getId
      <> " merchant=" <> req.merchantId.getShortId
      <> " count=" <> T.pack (show (length ids))
  pure BulkCreatePersonResp {totalCount = length ids, createdPersonIds = ids}

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

-- Preconditions: sanitized + required-field-checked. firstName/lastName default to "".
validateAndBuildPerson :: (BeamFlow m r, EncFlow m r) => DMerchant.Merchant -> UTCTime -> Int -> BulkCreatePerson -> m PT.Person
validateAndBuildPerson merchant now idx p = do
  let rowTag = "Row " <> T.pack (show idx) <> ": "
      require label = fromMaybeM (InvalidRequest $ rowTag <> label <> " is missing or blank")
  mobileNumber <- require "mobileNumber" p.mobileNumber
  mobileCountryCode <- require "mobileCountryCode" p.mobileCountryCode
  roleName <- require "roleName" p.roleName
  tokenNo <- require "tokenNo" p.tokenNo
  whenJust p.email $ \email -> do
    mbExisting <- QP.findByEmail email
    whenJust mbExisting $ \_ ->
      throwError (InvalidRequest (rowTag <> "email " <> email <> " is already registered"))
  mbExistingByMobile <- QP.findByMobileNumber mobileNumber mobileCountryCode
  whenJust mbExistingByMobile $ \_ ->
    throwError (InvalidRequest (rowTag <> "mobileNumber " <> mobileNumber <> " is already registered"))
  role <- QRole.findByName roleName >>= fromMaybeM (InvalidRequest (rowTag <> "role " <> roleName <> " does not exist"))
  mbEntityIdTyped <- case p.entityId of
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
  personId <- generateGUID
  encryptedMobileNumber <- encrypt mobileNumber
  encryptedEmail <- forM p.email encrypt
  tokenHash <- getDbHash tokenNo
  pure
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
        tokenNoHash = Just tokenHash,
        entityId = mbEntityIdTyped
      }
