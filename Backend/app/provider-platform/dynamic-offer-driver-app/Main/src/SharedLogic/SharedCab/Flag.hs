-- | The one exported writer of driver_information.shared_cab_session_active. The flag is fail-closed (True excludes the driver
-- from the plain-taxi pool), so every write goes through QDriverInformationExtra.updateSharedCabSessionActive (DB + LTS shadow
-- sync) inside the cross-app master Redis cell: the writer can be any driver-app service, each with its own key prefix.
-- Nothing else may call updateSharedCabSessionActive.
module SharedLogic.SharedCab.Flag
  ( setSharedCabSessionActive,
    clearSharedCabSessionActive,
  )
where

import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.DriverInformationExtra as QDriverInformationExtra

type FlagFlow m r = (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r)

setSharedCabSessionActive ::
  FlagFlow m r =>
  Id DP.Person ->
  m ()
setSharedCabSessionActive = writeSharedCabSessionActive True

clearSharedCabSessionActive ::
  FlagFlow m r =>
  Id DP.Person ->
  m ()
clearSharedCabSessionActive = writeSharedCabSessionActive False

writeSharedCabSessionActive ::
  FlagFlow m r =>
  Bool ->
  Id DP.Person ->
  m ()
writeSharedCabSessionActive active driverId =
  Redis.runInMasterCloudRedisCellWithCrossAppRedis . Redis.withMasterRedis $
    QDriverInformationExtra.updateSharedCabSessionActive active driverId
