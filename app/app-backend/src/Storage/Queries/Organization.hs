module Storage.Queries.Organization where

import Beckn.Storage.DB.Config
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import Beckn.Types.Schema
import Database.Beam ((==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Organization as Storage

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.OrganizationT))
getDbTable =
  DB.organization . DB.appDb <$> getSchemaName

findOrgByShortId :: DBFlow m r => ShortId Storage.Organization -> m (Maybe Storage.Organization)
findOrgByShortId shortId_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Organization {..} = shortId ==. B.val_ shortId_
