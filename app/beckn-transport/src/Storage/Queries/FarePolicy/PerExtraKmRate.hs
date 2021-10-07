module Storage.Queries.FarePolicy.PerExtraKmRate where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id (Id)
import Beckn.Types.Schema
import Data.List.NonEmpty (fromList)
import Database.Beam
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.FarePolicy.PerExtraKmRate as Storage
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Vehicle as Vehicle

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.FarePolicyPerExtraKmRateT))
getDbTable =
  DB.farePolicyExtraKmRate . DB.transporterDb <$> getSchemaName

create :: Storage.FarePolicyPerExtraKmRate -> DB.SqlDB ()
create Storage.FarePolicyPerExtraKmRate {..} = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue Storage.FarePolicyPerExtraKmRate {..})

findAll ::
  Id Organization.Organization ->
  Vehicle.Variant ->
  DB.SqlDB (NonEmpty Storage.FarePolicyPerExtraKmRate)
findAll orgId vehicleVariant_ = do
  dbTable <- getDbTable
  fromList <$> DB.findAll' dbTable (B.orderBy_ orderBy) predicate
  where
    orderBy Storage.FarePolicyPerExtraKmRate {..} = B.asc_ distanceRangeStart
    predicate Storage.FarePolicyPerExtraKmRate {..} =
      organizationId ==. B.val_ orgId
        &&. vehicleVariant ==. B.val_ vehicleVariant_

deleteAll :: Id Organization.Organization -> Vehicle.Variant -> DB.SqlDB ()
deleteAll orgId var = do
  dbTable <- getDbTable
  DB.delete' dbTable $ predicate orgId var
  where
    predicate orgId_ var_ Storage.FarePolicyPerExtraKmRate {..} =
      organizationId ==. B.val_ orgId_
        &&. vehicleVariant ==. B.val_ var_
