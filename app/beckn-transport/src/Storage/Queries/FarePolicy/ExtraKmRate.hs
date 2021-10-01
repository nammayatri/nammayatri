module Storage.Queries.FarePolicy.ExtraKmRate where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id (Id)
import Beckn.Types.Schema
import Database.Beam
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.FarePolicy.ExtraKmRate as Storage
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Vehicle as Vehicle

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.ExtraKmRateT))
getDbTable =
  DB.farePolicyExtraKmRate . DB.transporterDb <$> getSchemaName

create :: Storage.ExtraKmRate -> DB.SqlDB ()
create Storage.ExtraKmRate {..} = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue Storage.ExtraKmRate {..})

findAll ::
  Id Organization.Organization ->
  Vehicle.Variant ->
  DB.SqlDB [Storage.ExtraKmRate]
findAll orgId vehicleVariant_ = do
  dbTable <- getDbTable
  DB.findAll' dbTable (B.orderBy_ orderBy) predicate
  where
    orderBy Storage.ExtraKmRate {..} = B.asc_ fromExtraDistance
    predicate Storage.ExtraKmRate {..} =
      organizationId ==. B.val_ orgId
        &&. vehicleVariant ==. B.val_ vehicleVariant_

deleteAll :: Id Organization.Organization -> Vehicle.Variant -> DB.SqlDB ()
deleteAll orgId var = do
  dbTable <- getDbTable
  DB.delete' dbTable $ predicate orgId var
  where
    predicate orgId_ var_ Storage.ExtraKmRate {..} =
      organizationId ==. B.val_ orgId_
        &&. vehicleVariant ==. B.val_ var_
