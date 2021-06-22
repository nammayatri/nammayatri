module Storage.Queries.TransporterConfig where

import Beckn.Storage.DB.Config
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Types.Storage.Organization (Organization)
import Database.Beam ((&&.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.TransporterConfig as TransporterConfig

getDbTable :: HasFlowDBEnv m r => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity TransporterConfig.TransporterConfigT))
getDbTable =
  DB.transporterConfig . DB.transporterDb <$> getSchemaName

findValueByOrgIdAndKey :: HasFlowDBEnv m r => Id Organization -> TransporterConfig.ConfigKey -> m (Maybe TransporterConfig.TransporterConfig)
findValueByOrgIdAndKey orgId key_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate TransporterConfig.TransporterConfig {..} =
      transporterId ==. B.val_ orgId
        &&. key ==. B.val_ key_
