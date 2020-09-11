module Storage.Queries.Organization where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App as App
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Common
import Database.Beam ((&&.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB

getDbTable :: Flow (B.DatabaseEntity be DB.AppDb (B.TableEntity Org.OrganizationT))
getDbTable =
  DB._organization . DB.appDb <$> getSchemaName

findOrgByApiKey :: App.APIKey -> Flow (Maybe Org.Organization)
findOrgByApiKey apiKey = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Org.Organization {..} =
      _apiKey ==. B.val_ (Just apiKey)

listOrganizations ::
  Maybe Int ->
  Maybe Int ->
  [Org.OrganizationType] ->
  [Org.Status] ->
  Flow [Org.Organization]
listOrganizations mlimit moffset oType status = do
  dbTable <- getDbTable
  DB.findAllWithLimitOffsetWhere dbTable (predicate status) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    complementVal l
      | null l = B.val_ True
      | otherwise = B.val_ False
    limit = toInteger $ fromMaybe 100 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    orderByDesc Org.Organization {..} = B.desc_ _createdAt
    predicate pstatus Org.Organization {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _status `B.in_` (B.val_ <$> pstatus) ||. complementVal pstatus,
          _type `B.in_` (B.val_ <$> oType) ||. complementVal oType
        ]

findByBapUrl :: BaseUrl -> Flow (Maybe Org.Organization)
findByBapUrl bapUrl = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Org.Organization {..} =
      _callbackUrl ==. B.val_ (Just bapUrl)
        &&. _verified ==. B.val_ True
        &&. _enabled ==. B.val_ True
