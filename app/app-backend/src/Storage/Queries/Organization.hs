module Storage.Queries.Organization where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import Beckn.Types.Error
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Organization as Storage
import Beckn.Utils.Common (fromMaybeM400, getCurrTime, getSchemaName)
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Types.Storage.DB as DB

getDbTable :: Flow (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.OrganizationT))
getDbTable =
  DB._organization . DB.appDb <$> getSchemaName

create :: Storage.Organization -> Flow ()
create Storage.Organization {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Storage.Organization {..})
    >>= either DB.throwDBError pure

verifyApiKey :: RegToken -> Flow Storage.Organization
verifyApiKey regToken = do
  dbTable <- getDbTable
  DB.findOne dbTable (predicate regToken)
    >>= either DB.throwDBError pure
    >>= fromMaybeM400 Unauthorized
  where
    predicate token Storage.Organization {..} = _apiKey ==. B.val_ (Just token)

findOrganizationById ::
  Id Storage.Organization -> Flow (Maybe Storage.Organization)
findOrganizationById id = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Organization {..} = _id ==. B.val_ id

findOrganizationByCallbackUri ::
  Maybe BaseUrl -> Storage.OrganizationType -> Flow (Maybe Storage.Organization)
findOrganizationByCallbackUri url oType = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Organization {..} =
      _callbackUrl ==. B.val_ url
        &&. _type ==. B.val_ oType

listOrganizations ::
  Maybe Int ->
  Maybe Int ->
  [Storage.OrganizationType] ->
  [Storage.Status] ->
  Flow [Storage.Organization]
listOrganizations mlimit moffset oType status = do
  dbTable <- getDbTable
  DB.findAllWithLimitOffsetWhere dbTable (predicate oType status) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    limit = toInteger $ fromMaybe 100 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    orderByDesc Storage.Organization {..} = B.desc_ _createdAt
    predicate pOType pStatus Storage.Organization {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _status `B.in_` (B.val_ <$> status) ||. complementVal pStatus,
          _type `B.in_` (B.val_ <$> oType) ||. complementVal pOType
        ]

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

update ::
  Id Storage.Organization ->
  Storage.Status ->
  Flow (T.DBResult ())
update id status = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrTime
  DB.update
    dbTable
    (setClause status currTime)
    (predicate id)
  where
    predicate pid Storage.Organization {..} = _id ==. B.val_ pid
    setClause pStatus currTime Storage.Organization {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ pStatus
        ]

findOrgByShortId :: ShortId Storage.Organization -> Flow (Maybe Storage.Organization)
findOrgByShortId shortId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Organization {..} = _shortId ==. B.val_ shortId
