module Storage.Queries.FarePolicy where

import App.Types (AppEnv (dbCfg), Flow)
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.ID (ID)
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import Beckn.Utils.Common (getSchemaName)
import Database.Beam (SqlEq ((==.)), (&&.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.FarePolicy as Storage

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.FarePolicyT))
getDbTable =
  DB._farePolicy . DB.transporterDb <$> getSchemaName

findFarePolicyByOrgAndVehicleVariant ::
  ID Organization.Organization -> Vehicle.Variant -> Flow (Maybe Storage.FarePolicy)
findFarePolicyByOrgAndVehicleVariant orgId vehicleVariant = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.FarePolicy {..} =
      _organizationId ==. B.val_ orgId
        &&. _vehicleVariant ==. B.val_ vehicleVariant
