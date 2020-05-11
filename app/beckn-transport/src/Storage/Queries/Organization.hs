module Storage.Queries.Organization where

import           Database.Beam                    ((&&.), (<-.), (==.), (||.))
import           EulerHS.Prelude                  hiding (id)

import qualified Storage.Queries            as DB
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Types.Storage.DB                 as DB
import qualified Beckn.Types.Storage.Organization as Storage
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import Data.ByteString.Base64 as DBB
import qualified Data.Text.Encoding as DT
import qualified Data.Text as DT
dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.OrganizationT)
dbTable = DB._organization DB.transporterDb

create :: Storage.Organization -> L.Flow ()
create Storage.Organization {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Organization {..})
    >>= either DB.throwDBError pure

verifyAuth :: Maybe Text -> L.Flow Storage.Organization
verifyAuth auth = do
  L.logInfo "verifying auth" $ show auth
  let apiKey = DT.reverse <$> DT.drop 1
               <$> DT.reverse <$> DT.decodeUtf8
               <$> (rightToMaybe =<< DBB.decode <$> DT.encodeUtf8 <$> DT.drop 6 <$> auth)
  -- did atob of auth by removing basic in front and after atob, `:` in the end
  L.logInfo "verifying apikey" $ show apiKey
  DB.findOne dbTable (predicate apiKey)
    >>= either DB.throwDBError pure
    >>= fromMaybeM400 "UNAUTHENTICATED_USER"
  where
    predicate apiKey Storage.Organization {..} = (_apiKey ==. B.val_ apiKey)

findOrganizationById ::
  OrganizationId -> L.Flow (Maybe Storage.Organization)
findOrganizationById id = do
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Organization {..} = (_id ==. B.val_ id)

listOrganizations ::
  Maybe Int ->
  Maybe Int ->
  [Storage.OrganizationType] ->
  [Storage.Status] ->
  L.Flow [Storage.Organization]
listOrganizations mlimit moffset oType status =
  DB.findAllWithLimitOffsetWhere dbTable (predicate oType status) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    limit = (toInteger $ fromMaybe 100 mlimit)
    offset = (toInteger $ fromMaybe 0 moffset)
    orderByDesc Storage.Organization {..} = B.desc_ _createdAt
    predicate oType status Storage.Organization {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _status `B.in_` (B.val_ <$> status) ||. complementVal status,
          _type `B.in_` (B.val_ <$> oType) ||. complementVal oType
        ]

complementVal l
  | (null l) = B.val_ True
  | otherwise = B.val_ False

update ::
  OrganizationId ->
  Storage.Status ->
  L.Flow (T.DBResult ())
update id status = do
  (currTime :: LocalTime) <- getCurrTime
  DB.update
    dbTable
    (setClause status currTime)
    (predicate id)
  where
    predicate id Storage.Organization {..} = _id ==. B.val_ id
    setClause status currTime Storage.Organization {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ status
        ]
