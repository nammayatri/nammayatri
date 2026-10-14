-- | Writes of 'person.driverTag', kept out of Storage.Queries.PersonExtra so that low-level
-- modules (SharedLogic.FleetTags, called from the fleet-driver association queries) can
-- write tags without importing Storage.Queries.Person, whose module graph reaches those
-- association queries. Re-exported from Storage.Queries.PersonExtra.
module Storage.Queries.Person.DriverTag (updateDriverTag) where

import Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import qualified Sequelize as Se
import qualified SharedLogic.DriverPool.LTSDataSync as LTSSync
import qualified Storage.Beam.Person as BeamP
import qualified Storage.CachedQueries.CoinsConfig as CQCoinsConfig
import Storage.Queries.OrphanInstances.Person ()

updateDriverTag :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) => Maybe [LYT.TagNameValueExpiry] -> Id Person -> m ()
updateDriverTag driverTag personId = do
  now <- getCurrentTime
  updateOneWithKV [Se.Set BeamP.driverTag (Yudhishthira.tagsNameValueExpiryToTType driverTag), Se.Set BeamP.updatedAt now] [Se.Is BeamP.id $ Se.Eq (getId personId)]
  LTSSync.syncDriverPoolDataToLTS (cast personId) $
    LTSSync.emptyUpdate {LTSSync.driverTag = LTSSync.Set driverTag}
  CQCoinsConfig.clearDriverIncentiveConfigHashForDriver personId
