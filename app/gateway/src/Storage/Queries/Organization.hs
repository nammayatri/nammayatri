module Storage.Queries.Organization where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App as App
import Beckn.Types.Id
import Beckn.Types.Schema
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Common
import Database.Beam ((&&.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB

getDbTable :: Flow (B.DatabaseEntity be DB.AppDb (B.TableEntity Org.OrganizationT))
getDbTable =
  DB._organization . DB.appDb <$> getSchemaName

findOrgById :: Text -> Flow (Maybe Org.Organization)
findOrgById oId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= checkDBError
  where
    predicate Org.Organization {..} =
      _id ==. B.val_ (Id oId)

findOrgByShortId :: ShortId Org.Organization -> Flow (Maybe Org.Organization)
findOrgByShortId shortOrgId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= checkDBError
  where
    predicate Org.Organization {..} =
      _shortId ==. B.val_ shortOrgId

findOrgByApiKey ::
  Org.OrganizationType -> App.APIKey -> Flow (Maybe Org.Organization)
findOrgByApiKey oType apiKey = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= checkDBError
  where
    predicate Org.Organization {..} =
      _apiKey ==. B.val_ (Just apiKey)
        &&. _type ==. B.val_ oType

listOrganizations ::
  Maybe Int ->
  Maybe Int ->
  [Org.OrganizationType] ->
  [Org.OrganizationDomain] ->
  Flow [Org.Organization]
listOrganizations mlimit moffset oType oDomain = do
  dbTable <- getDbTable
  DB.findAllWithLimitOffsetWhere dbTable predicate limit offset orderByDesc
    >>= checkDBError
  where
    limit = toInteger $ fromMaybe 100 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    orderByDesc Org.Organization {..} = B.desc_ _createdAt
    predicate Org.Organization {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _enabled ==. B.val_ True,
          _verified ==. B.val_ True,
          _type `B.in_` (B.val_ <$> oType),
          _domain `B.in_` (B.val_ . Just <$> oDomain)
        ]
