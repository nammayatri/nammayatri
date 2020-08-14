module Storage.Queries.Organization where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App as App
import qualified Beckn.Types.Storage.Organization as Org
import Database.Beam ((&&.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.AppDb (B.TableEntity Org.OrganizationT)
dbTable = DB._organization DB.appDb

findOrgByApiKey ::
  Org.OrganizationType -> App.APIKey -> Flow (Maybe Org.Organization)
findOrgByApiKey oType apiKey =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
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
listOrganizations mlimit moffset oType oDomain =
  DB.findAllWithLimitOffsetWhere dbTable predicate limit offset orderByDesc
    >>= either DB.throwDBError pure
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
