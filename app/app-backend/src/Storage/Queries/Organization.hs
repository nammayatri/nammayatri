module Storage.Queries.Organization where

import qualified Beckn.Storage.Common as Storage
import Beckn.Storage.DB.Config
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import Data.Time (UTCTime)
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.Error
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Organization as Storage
import Utils.Common

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.OrganizationT))
getDbTable =
  DB.organization . DB.appDb <$> getSchemaName

create :: DBFlow m r => Storage.Organization -> m ()
create Storage.Organization {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertValue Storage.Organization {..})

verifyApiKey :: DBFlow m r => RegToken -> m Storage.Organization
verifyApiKey regToken = do
  dbTable <- getDbTable
  DB.findOne dbTable (predicate regToken)
    >>= fromMaybeM Unauthorized
  where
    predicate token Storage.Organization {..} = apiKey ==. B.val_ (Just token)

findOrganizationById ::
  DBFlow m r =>
  Id Storage.Organization ->
  m (Maybe Storage.Organization)
findOrganizationById orgId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Organization {..} = id ==. B.val_ orgId

findOrganizationByCallbackUri ::
  DBFlow m r =>
  Maybe BaseUrl ->
  Storage.OrganizationType ->
  m (Maybe Storage.Organization)
findOrganizationByCallbackUri url oType = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Organization {..} =
      callbackUrl ==. B.val_ url
        &&. _type ==. B.val_ oType

listOrganizations ::
  DBFlow m r =>
  Maybe Int ->
  Maybe Int ->
  [Storage.OrganizationType] ->
  [Storage.Status] ->
  m [Storage.Organization]
listOrganizations mlimit moffset oType status_ = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) (predicate oType status_)
  where
    limit = toInteger $ fromMaybe 100 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    orderByDesc Storage.Organization {..} = B.desc_ createdAt
    predicate pOType pStatus Storage.Organization {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ status `B.in_` (B.val_ <$> status_) ||. complementVal pStatus,
          _type `B.in_` (B.val_ <$> oType) ||. complementVal pOType
        ]

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
    setClause pStatus currTime Storage.Organization {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ pStatus
        ]

findOrgByShortId :: DBFlow m r => ShortId Storage.Organization -> m (Maybe Storage.Organization)
findOrgByShortId shortId_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Organization {..} = shortId ==. B.val_ shortId_
