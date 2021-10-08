module Storage.Queries.FarePolicy.Discount where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id (Id)
import Beckn.Types.Schema
import Beckn.Utils.Common
import Database.Beam
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.FarePolicy.Discount as Storage
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Vehicle as Vehicle

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.FarePolicyDiscountT))
getDbTable =
  DB.farePolicyDiscount . DB.transporterDb <$> getSchemaName

create :: Storage.FarePolicyDiscount -> DB.SqlDB ()
create Storage.FarePolicyDiscount {..} = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue Storage.FarePolicyDiscount {..})

findAll ::
  Id Organization.Organization ->
  Vehicle.Variant ->
  DB.SqlDB [Storage.FarePolicyDiscount]
findAll orgId vehicleVariant_ = do
  dbTable <- getDbTable
  DB.findAll' dbTable identity predicate
  where
    predicate Storage.FarePolicyDiscount {..} =
      organizationId ==. B.val_ orgId
        &&. vehicleVariant ==. B.val_ vehicleVariant_

deleteAll :: Id Organization.Organization -> Vehicle.Variant -> DB.SqlDB ()
deleteAll orgId var = do
  dbTable <- getDbTable
  DB.delete' dbTable $ predicate orgId var
  where
    predicate orgId_ var_ Storage.FarePolicyDiscount {..} =
      organizationId ==. B.val_ orgId_
        &&. vehicleVariant ==. B.val_ var_
