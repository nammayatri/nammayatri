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
import qualified "lib-dashboard" Storage.Queries.MerchantAccess as QAccess
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

bulkCreate :: (BeamFlow m r, EncFlow m r) => Id DP.Person -> BulkCreatePersonReq -> m BulkCreatePersonResp
bulkCreate actorPersonId req = do
  let total = length req.persons
  when (total == 0) $
    throwError (InvalidRequest "persons array is empty")
  when (total > maxBulkPersons) $
    throwError (InvalidRequest $ "persons exceeds per-request cap of " <> T.pack (show maxBulkPersons) <> " rows; split the CSV")
  merchant <-
    QMerchant.findByShortId req.merchantId
      >>= fromMaybeM (MerchantDoesNotExist req.merchantId.getShortId)
  let phoneKeys = map (\p -> (p.mobileCountryCode, p.mobileNumber)) req.persons
      emails = mapMaybe (.email) req.persons
  when (length phoneKeys /= length (nub phoneKeys)) $
    throwError (InvalidRequest "Duplicate mobileCountryCode+mobileNumber within the batch")
  when (length emails /= length (nub (map T.toLower emails))) $
    throwError (InvalidRequest "Duplicate email within the batch")
  now <- getCurrentTime
  pairs <- forM (zip [0 :: Int ..] req.persons) $ \(idx, p) -> do
    person <- validateAndBuildPerson now idx p
    access <- buildMerchantAccess merchant req.operatingCity now person
    pure (person, access)
  written <- writeWithRollback pairs
  let ids = map fst written
  logInfo $
    "[PTEmployee.bulkCreate] actor=" <> actorPersonId.getId
      <> " merchant=" <> req.merchantId.getShortId
      <> " count=" <> T.pack (show (length ids))
  pure BulkCreatePersonResp {totalCount = length ids, createdPersonIds = ids}

-- KV/PG has no transaction primitive exposed at this layer; on a mid-batch
-- failure we undo every row already written so the admin can re-upload the
-- same CSV without a "mobile already registered" cleanup pass.
writeWithRollback ::
  BeamFlow m r =>
  [(PT.Person, DAccess.MerchantAccess)] ->
  m [(Id PT.Person, Id DAccess.MerchantAccess)]
writeWithRollback = go []
  where
    go acc [] = pure (reverse acc)
    go acc ((person, access) : rest) = do
      catchAny (QP.create person >> QAccess.create access) $ \e -> do
        logError $ "[PTEmployee.bulkCreate] write failed on personId=" <> person.id.getId <> "; rolling back " <> T.pack (show (length acc)) <> " prior rows"
        forM_ acc $ \(pId, aId) -> do
          catchAny (QAccess.deleteById aId) $ \de ->
            logError $ "[PTEmployee.bulkCreate] rollback: failed to delete merchantAccess " <> aId.getId <> ": " <> T.pack (show de)
          catchAny (QP.deletePerson pId) $ \de ->
            logError $ "[PTEmployee.bulkCreate] rollback: failed to delete person " <> pId.getId <> ": " <> T.pack (show de)
        throwM e
      go ((person.id, access.id) : acc) rest

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
        passwordUpdatedAt = Just now,
        approvedBy = Nothing,
        rejectedBy = Nothing,
        language = Nothing,
        tokenNo = Just tokenHash,
        entityId = Just entityIdTyped
      }
