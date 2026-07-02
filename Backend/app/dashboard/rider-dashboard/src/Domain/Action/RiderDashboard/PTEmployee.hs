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
import Kernel.External.Encryption (EncFlow, encrypt, getDbHash)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import "lib-dashboard" Storage.Beam.BeamFlow
import qualified "lib-dashboard" Storage.Queries.Entity as QE
import qualified "lib-dashboard" Storage.Queries.Merchant as QMerchant
import qualified "lib-dashboard" Storage.Queries.Person as QP
import qualified "lib-dashboard" Storage.Queries.Role as QRole
import "lib-dashboard" Tools.Error

data BulkCreatePerson = BulkCreatePerson
  { firstName :: Text,
    lastName :: Text,
    mobileNumber :: Text,
    mobileCountryCode :: Text,
    email :: Maybe Text,
    roleName :: Text,
    entityId :: Text,
    tokenNo :: Text
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

-- Mirror of sanitizeLoginReq's semantics: strip every string field so what
-- gets persisted matches what login-time lookup will search for. Also collapses
-- email = Just "" to Nothing so blank emails don't participate in the dup check.
sanitizeBulkPerson :: BulkCreatePerson -> BulkCreatePerson
sanitizeBulkPerson p =
  p
    { firstName = T.strip p.firstName,
      lastName = T.strip p.lastName,
      mobileNumber = T.strip p.mobileNumber,
      mobileCountryCode = T.strip p.mobileCountryCode,
      email = p.email >>= nonBlank,
      roleName = T.strip p.roleName,
      entityId = T.strip p.entityId,
      tokenNo = T.strip p.tokenNo
    }
  where
    nonBlank t = case T.strip t of
      "" -> Nothing
      s -> Just s

bulkCreate :: (BeamFlow m r, EncFlow m r) => Id DP.Person -> BulkCreatePersonReq -> m BulkCreatePersonResp
bulkCreate actorPersonId req = do
  let total = length req.persons
  when (total == 0) $
    throwError (InvalidRequest "persons array is empty")
  when (total > maxBulkPersons) $
    throwError (InvalidRequest $ "persons exceeds per-request cap of " <> T.pack (show maxBulkPersons) <> " rows; split the CSV")
  -- Strip whitespace on every string field so the persisted value matches what
  -- sanitizeLoginReq will look up at login time. Reject rows whose critical
  -- fields collapse to empty post-strip — otherwise they'd persist as
  -- unreachable accounts.
  persons <- forM (zip [0 :: Int ..] req.persons) $ \(idx, p) -> do
    let p' = sanitizeBulkPerson p
        rowTag = "Row " <> T.pack (show idx) <> ": "
    when (T.null p'.mobileNumber) $ throwError (InvalidRequest $ rowTag <> "mobileNumber is empty after trim")
    when (T.null p'.mobileCountryCode) $ throwError (InvalidRequest $ rowTag <> "mobileCountryCode is empty after trim")
    when (T.null p'.tokenNo) $ throwError (InvalidRequest $ rowTag <> "tokenNo is empty after trim")
    when (T.null p'.roleName) $ throwError (InvalidRequest $ rowTag <> "roleName is empty after trim")
    when (T.null p'.entityId) $ throwError (InvalidRequest $ rowTag <> "entityId is empty after trim")
    pure p'
  merchant <-
    QMerchant.findByShortId req.merchantId
      >>= fromMaybeM (MerchantDoesNotExist req.merchantId.getShortId)
  let phoneKeys = map (\p -> (p.mobileCountryCode, p.mobileNumber)) persons
      emails = mapMaybe (.email) persons
  when (length phoneKeys /= length (nub phoneKeys)) $
    throwError (InvalidRequest "Duplicate mobileCountryCode+mobileNumber within the batch")
  when (length emails /= length (nub (map T.toLower emails))) $
    throwError (InvalidRequest "Duplicate email within the batch")
  now <- getCurrentTime
  pairs <- forM (zip [0 :: Int ..] persons) $ \(idx, p) -> do
    person <- validateAndBuildPerson now idx p
    access <- buildMerchantAccess merchant req.operatingCity now person
    pure (person, access)
  -- One Postgres transaction covers all N person rows + N access rows.
  -- Either every row lands or Postgres rolls the whole batch back — no orphan
  -- rows are possible, no best-effort cleanup needed, no distinction between
  -- "clean rollback" and "partial rollback" for the caller to worry about.
  QP.createPersonsWithAccessAtomic pairs
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
        secretKey = Nothing,
        is2faEnabled = False,
        createdAt = now,
        operatingCity = city
      }

validateAndBuildPerson :: (BeamFlow m r, EncFlow m r) => UTCTime -> Int -> BulkCreatePerson -> m PT.Person
validateAndBuildPerson now idx p = do
  let rowTag = "Row " <> T.pack (show idx) <> ": "
  whenJust p.email $ \email -> do
    mbExisting <- QP.findByEmail email
    whenJust mbExisting $ \_ ->
      throwError (InvalidRequest (rowTag <> "email " <> email <> " is already registered"))
  mbExistingByMobile <- QP.findByMobileNumber p.mobileNumber p.mobileCountryCode
  whenJust mbExistingByMobile $ \_ ->
    throwError (InvalidRequest (rowTag <> "mobileNumber " <> p.mobileNumber <> " is already registered"))
  role <- QRole.findByName p.roleName >>= fromMaybeM (InvalidRequest (rowTag <> "role " <> p.roleName <> " does not exist"))
  let entityIdTyped = Id p.entityId :: Id DE.Entity
  entity <-
    QE.findById entityIdTyped
      >>= fromMaybeM (InvalidRequest (rowTag <> "entity " <> p.entityId <> " does not exist"))
  when entity.deleted $
    throwError (InvalidRequest (rowTag <> "entity " <> p.entityId <> " is soft-deleted; cannot attach new persons to a retired depot"))
  personId <- generateGUID
  encryptedMobileNumber <- encrypt p.mobileNumber
  encryptedEmail <- forM p.email encrypt
  tokenHash <- getDbHash p.tokenNo
  pure
    PT.Person
      { id = personId,
        firstName = p.firstName,
        lastName = p.lastName,
        roleId = role.id,
        email = encryptedEmail,
        mobileNumber = encryptedMobileNumber,
        mobileCountryCode = p.mobileCountryCode,
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
        tokenNoHash = Just tokenHash,
        entityId = Just entityIdTyped
      }
