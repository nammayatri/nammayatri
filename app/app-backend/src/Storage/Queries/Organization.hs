module Storage.Queries.Organization where

import App.Types
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

getDbTable :: Flow (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.OrganizationT))
getDbTable =
  DB.organization . DB.appDb <$> getSchemaName

create :: Storage.Organization -> Flow ()
create Storage.Organization {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Storage.Organization {..})

verifyApiKey :: RegToken -> Flow Storage.Organization
verifyApiKey regToken = do
  dbTable <- getDbTable
  DB.findOne dbTable (predicate regToken)
    >>= fromMaybeM Unauthorized
  where
    predicate token Storage.Organization {..} = apiKey ==. B.val_ (Just token)

findOrganizationById ::
  Id Storage.Organization -> Flow (Maybe Storage.Organization)
findOrganizationById orgId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Organization {..} = id ==. B.val_ orgId

findOrganizationByCallbackUri ::
  Maybe BaseUrl -> Storage.OrganizationType -> Flow (Maybe Storage.Organization)
findOrganizationByCallbackUri url oType = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Organization {..} =
      callbackUrl ==. B.val_ url
        &&. _type ==. B.val_ oType

listOrganizations ::
  Maybe Int ->
  Maybe Int ->
  [Storage.OrganizationType] ->
  [Storage.Status] ->
  Flow [Storage.Organization]
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
  Id Storage.Organization ->
  Storage.Status ->
  Flow ()
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

findOrgByShortId :: ShortId Storage.Organization -> Flow (Maybe Storage.Organization)
findOrgByShortId shortId_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Organization {..} = shortId ==. B.val_ shortId_
