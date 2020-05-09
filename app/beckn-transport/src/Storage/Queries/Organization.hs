module Storage.Queries.Organization where

import           Database.Beam                    ((&&.), (<-.), (==.), (||.))
import           EulerHS.Prelude                  hiding (id)

import qualified Beckn.Storage.Queries            as DB
import           Types.App
import           Beckn.Types.Common
import qualified Types.Storage.DB                 as DB
import qualified Types.Storage.Organization       as Storage
import           Beckn.Utils.Common
import           Data.Time
import qualified Database.Beam                    as B
import qualified EulerHS.Language                 as L
import qualified EulerHS.Types                    as T

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.OrganizationT)
dbTable = DB._organization DB.transporterDb

create :: Storage.Organization -> L.Flow ()
create Storage.Organization {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Organization {..}) >>=
  either DB.throwDBError pure

findOrganizationById ::
     OrganizationId -> L.Flow (Maybe Storage.Organization)
findOrganizationById id = do
  DB.findOne dbTable predicate >>=
    either DB.throwDBError pure
  where
    predicate Storage.Organization {..} = (_id ==. B.val_ id)

listOrganizations :: Maybe Int -> Maybe Int -> [Storage.Status] -> L.Flow [Storage.Organization]
listOrganizations mlimit moffset status =
  DB.findAllWithLimitOffsetWhere dbTable (predicate status) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    limit = (toInteger $ fromMaybe 100 mlimit)
    offset = (toInteger $ fromMaybe 0 moffset)
    orderByDesc Storage.Organization {..} = B.desc_ _createdAt

    predicate status Storage.Organization {..} =
        foldl (&&.)
          (B.val_ True)
          [ _status `B.in_` (B.val_ <$> status) ||. complementVal status
          ]

complementVal l
  | (null l) = B.val_ True
  | otherwise = B.val_ False

update ::
  OrganizationId
  -> Storage.Status
  -> L.Flow (T.DBResult ())
update id status = do
  (currTime :: LocalTime) <- getCurrTime
  DB.update dbTable
    (setClause status currTime)
    (predicate id)
  where
    predicate id Storage.Organization {..} = _id ==. B.val_ id
    setClause status currTime Storage.Organization {..} =
      mconcat
      [_updatedAt <-. B.val_ currTime
      , _status <-. B.val_ status ]
