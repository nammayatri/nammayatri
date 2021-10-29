module Storage.Queries.Organization where

import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import Database.Beam ((&&.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Organization as Org

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.AppDb (B.TableEntity Org.OrganizationT))
getDbTable =
  DB.organization . DB.appDb <$> getSchemaName

findOrgById :: DBFlow m r => Text -> m (Maybe Org.Organization)
findOrgById oId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Org.Organization {..} =
      id ==. B.val_ (Id oId)

findOrgByShortId :: DBFlow m r => ShortId Org.Organization -> m (Maybe Org.Organization)
findOrgByShortId shortOrgId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Org.Organization {..} =
      shortId ==. B.val_ shortOrgId

findByBapUrl :: DBFlow m r => BaseUrl -> m (Maybe Org.Organization)
findByBapUrl bapUrl = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Org.Organization {..} =
      callbackUrl ==. B.val_ (Just bapUrl)
        &&. verified ==. B.val_ True
        &&. enabled ==. B.val_ True

listOrganizations ::
  DBFlow m r =>
  Maybe Int ->
  Maybe Int ->
  [Org.OrganizationType] ->
  [Org.OrganizationDomain] ->
  m [Org.Organization]
listOrganizations mlimit moffset oType oDomain = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) predicate
  where
    limit = toInteger $ fromMaybe 100 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    orderByDesc Org.Organization {..} = B.desc_ createdAt
    predicate Org.Organization {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ enabled ==. B.val_ True,
          verified ==. B.val_ True,
          _type `B.in_` (B.val_ <$> oType),
          domain `B.in_` (B.val_ . Just <$> oDomain)
        ]
