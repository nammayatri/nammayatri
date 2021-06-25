module Storage.Queries.Organization where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import qualified Beckn.Types.Storage.Organization as Storage
import Data.Time (UTCTime)
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.Error
import qualified Types.Storage.DB as DB
import Utils.Common

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.OrganizationT))
getDbTable =
  DB.organization . DB.transporterDb <$> getSchemaName

create :: DBFlow m r => Storage.Organization -> m ()
create Storage.Organization {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Storage.Organization {..})

verifyToken :: DBFlow m r => RegToken -> m Storage.Organization
verifyToken regToken = do
  logInfo "Verifying Token"
  dbTable <- getDbTable
  DB.findOne dbTable (predicate regToken) >>= fromMaybeM (InvalidToken regToken)
  where
    predicate token Storage.Organization {..} = apiKey ==. B.val_ (Just token)

findOrganizationById :: DBFlow m r => Id Storage.Organization -> m (Maybe Storage.Organization)
findOrganizationById orgId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Organization {..} = id ==. B.val_ orgId

findOrganizationByShortId :: DBFlow m r => ShortId Storage.Organization -> m (Maybe Storage.Organization)
findOrganizationByShortId shortId_ = do
  dbTable <- getDbTable
  DB.findOne dbTable (\Storage.Organization {..} -> shortId ==. B.val_ shortId_)

listOrganizations ::
  DBFlow m r =>
  Maybe Int ->
  Maybe Int ->
  [Storage.OrganizationType] ->
  [Storage.Status] ->
  m [Storage.Organization]
listOrganizations mlimit moffset oType status_ = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) predicate
  where
    limit = toInteger $ fromMaybe 100 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    orderByDesc Storage.Organization {..} = B.desc_ createdAt
    predicate Storage.Organization {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ status `B.in_` (B.val_ <$> status_) ||. complementVal status_,
          domain ==. B.val_ (Just Storage.MOBILITY),
          _type `B.in_` (B.val_ <$> oType) ||. complementVal oType,
          enabled ==. B.val_ True
        ]

loadAllProviders :: DBFlow m r => m [Storage.Organization]
loadAllProviders = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Organization {..} =
      status ==. B.val_ Storage.APPROVED
        &&. domain ==. B.val_ (Just Storage.MOBILITY)
        &&. _type ==. B.val_ Storage.PROVIDER
        &&. enabled ==. B.val_ True

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

update ::
  DBFlow m r =>
  Id Storage.Organization ->
  Storage.Status ->
  m ()
update orgId status_ = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrentTime
  DB.update
    dbTable
    (setClause status_ currTime)
    (predicate orgId)
  where
    predicate oid Storage.Organization {..} = id ==. B.val_ oid
    setClause scStatus currTime Storage.Organization {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ scStatus
        ]

updateOrganizationRec :: DBFlow m r => Storage.Organization -> m ()
updateOrganizationRec org = do
  dbTable <- getDbTable
  DB.update dbTable (setClause org) (predicate $ org.id)
  where
    setClause sOrg Storage.Organization {..} =
      mconcat
        [ name <-. B.val_ (Storage.name sOrg),
          description <-. B.val_ (Storage.description sOrg),
          headCount <-. B.val_ (Storage.headCount sOrg),
          enabled <-. B.val_ (Storage.enabled sOrg),
          updatedAt <-. B.val_ (Storage.updatedAt sOrg),
          fromTime <-. B.val_ (Storage.fromTime sOrg)
        ]
    predicate orgId Storage.Organization {..} = id ==. B.val_ orgId

findOrgByApiKey :: DBFlow m r => APIKey -> m (Maybe Storage.Organization)
findOrgByApiKey apiKey_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Organization {..} =
      apiKey ==. B.val_ (Just apiKey_)

findOrgByCbUrl :: DBFlow m r => BaseUrl -> m (Maybe Storage.Organization)
findOrgByCbUrl url = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Organization {..} = callbackUrl ==. B.val_ (Just url)

findOrgByShortId :: DBFlow m r => ShortId Storage.Organization -> m (Maybe Storage.Organization)
findOrgByShortId shortId_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Organization {..} = shortId ==. B.val_ shortId_

findOrgByMobileNumber :: DBFlow m r => Text -> Text -> m (Maybe Storage.Organization)
findOrgByMobileNumber countryCode mobileNumber_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Organization {..} =
      mobileCountryCode ==. B.val_ (Just countryCode)
        &&. mobileNumber ==. B.val_ (Just mobileNumber_)
