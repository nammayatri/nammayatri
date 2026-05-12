module Storage.Queries.DriverBankAccountExtra where

import Data.List (nub)
import Domain.Types.DriverBankAccount
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import Domain.Types.Person
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Sequelize as Se
import qualified SharedLogic.DriverPool.LTSDataSync as LTSSync
import qualified Storage.Beam.DriverBankAccount as Beam
import qualified Storage.Queries.FleetDriverAssociation as QFOA
import Storage.Queries.OrphanInstances.DriverBankAccount ()

-- Extra code goes here --
getDriverBankAccounts :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Kernel.Types.Id.Id Domain.Types.Person.Person] -> m [Domain.Types.DriverBankAccount.DriverBankAccount])
getDriverBankAccounts driverIds = do findAllWithKV [Se.And [Se.Is Beam.driverId $ Se.In (Kernel.Types.Id.getId <$> driverIds)]]

-- Wrapper for src-read-only function with LTS sync

updateAccountStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) => Bool -> Bool -> Bool -> Id DP.Person -> m ()
updateAccountStatus chargesEnabled payoutsEnabled detailsSubmitted driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.chargesEnabled chargesEnabled, Se.Set Beam.payoutsEnabled (Just payoutsEnabled), Se.Set Beam.detailsSubmitted detailsSubmitted, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (getId driverId)]
  fanoutBankAccountSync driverId $
    LTSSync.emptyUpdate {LTSSync.chargesEnabled = LTSSync.Set chargesEnabled}

-- | Sync a BA-derived LTS update to the BA owner's own pool data and to every
-- active fleet-member driver under them. paymentMode is only set at BA creation
-- (and on fleet membership change), so subsequent chargesEnabled-only updates
-- leave it as Unchanged.
syncBankAccountToPool ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id DP.Person ->
  Bool ->
  Maybe DMPM.PaymentMode ->
  m ()
syncBankAccountToPool ownerId chargesEnabled mbPaymentMode =
  fanoutBankAccountSync ownerId $
    LTSSync.emptyUpdate
      { LTSSync.chargesEnabled = LTSSync.Set chargesEnabled,
        LTSSync.bankAccountPaymentMode = LTSSync.Set mbPaymentMode
      }

fanoutBankAccountSync ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id DP.Person ->
  LTSSync.DriverPoolDataUpdate ->
  m ()
fanoutBankAccountSync ownerId update = do
  fleetMemberIds <- QFOA.getActiveDriverIdsByFleetOwnerId ownerId.getId
  let targetDriverIds = nub (cast ownerId : map cast fleetMemberIds)
  mapM_ (`LTSSync.syncDriverPoolDataToLTS` update) targetDriverIds
