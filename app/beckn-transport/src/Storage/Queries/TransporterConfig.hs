module Storage.Queries.TransporterConfig where

import App.Types (AppEnv (dbCfg), Flow)
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Database.Beam ((&&.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude
import Types.App (ConfigKey)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.TransporterConfig as TransporterConfig

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity TransporterConfig.TransporterConfigT))
getDbTable =
  DB._transporterConfig . DB.transporterDb <$> getSchemaName

findValueByOrgIdAndKey :: Id Organization -> ConfigKey -> Flow (Maybe TransporterConfig.TransporterConfig)
findValueByOrgIdAndKey orgId key = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= checkDBError
  where
    predicate TransporterConfig.TransporterConfig {..} =
      _transporterId ==. B.val_ orgId
        &&. _key ==. B.val_ key
