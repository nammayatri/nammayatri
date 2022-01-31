module Storage.Queries.Organization where

import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import Database.Beam ((&&.), (<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Organization as Storage
import Utils.Common

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.OrganizationT))
getDbTable =
  DB.organization . DB.transporterDb <$> getSchemaName

findOrganizationById :: DBFlow m r => Id Storage.Organization -> m (Maybe Storage.Organization)
findOrganizationById orgId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Organization {..} = id ==. B.val_ orgId

loadAllProviders :: DBFlow m r => m [Storage.Organization]
loadAllProviders = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Organization {..} =
      status ==. B.val_ Storage.APPROVED
        &&. domain ==. B.val_ (Just Storage.MOBILITY)
        &&. _type ==. B.val_ Storage.PROVIDER
        &&. enabled ==. B.val_ True

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

updateOrganizationRec :: DBFlow m r => Storage.Organization -> m ()
updateOrganizationRec org = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update dbTable (setClause org now) (predicate $ org.id)
  where
    setClause sOrg now Storage.Organization {..} =
      mconcat
        [ name <-. B.val_ sOrg.name,
          description <-. B.val_ sOrg.description,
          headCount <-. B.val_ sOrg.headCount,
          enabled <-. B.val_ sOrg.enabled,
          updatedAt <-. B.val_ now,
          fromTime <-. B.val_ sOrg.fromTime
        ]
    predicate orgId Storage.Organization {..} = id ==. B.val_ orgId
