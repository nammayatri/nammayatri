{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Storage.Queries.OrgLocation where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import Database.Beam ((==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id, state)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.OrgLocation as Storage
import qualified Types.Storage.Organization as Org

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.OrgLocationT))
getDbTable =
  DB.orgLocation . DB.transporterDb <$> getSchemaName

createFlow :: DBFlow m r => Storage.OrgLocation -> m ()
createFlow =
  DB.runSqlDB . create

create :: Storage.OrgLocation -> DB.SqlDB ()
create location = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertExpression location)

findById ::
  DBFlow m r =>
  Id Org.Organization ->
  m (Maybe Storage.OrgLocation)
findById orgLocId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.OrgLocation {..} = orgId ==. B.val_ orgLocId