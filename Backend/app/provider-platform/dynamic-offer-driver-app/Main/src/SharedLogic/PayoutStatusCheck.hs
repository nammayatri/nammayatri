module SharedLogic.PayoutStatusCheck
  ( PayoutStatusCheckCreator,
    getPayoutStatusCheckConfig,
    getPayoutStatusCheckConfigForPerson,
    scheduleNextPayoutStatusCheck,
    afterPayoutOrderCreated,
  )
where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.VehicleCategory as DVC
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.AccountConfig as FinanceConfig
import qualified Lib.Finance.Domain.Types.Account as FAccount
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import qualified Lib.Payment.Payout.StatusCheck as PSC
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator (AllocatorJobType (..))
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.PayoutConfig (PayoutConfigDimensions (..))
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Vehicle as QV
import Tools.Error

type PayoutStatusCheckCreator m r =
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text]
  )

getPayoutStatusCheckConfig :: (CacheFlow m r, EsqDBFlow m r) => DPayoutOrder.PayoutOrder -> m PSC.PayoutStatusCheckConfig
getPayoutStatusCheckConfig order = do
  merchantOperatingCityId <- case order.merchantOperatingCityId of
    Just mocId -> pure (Id mocId)
    Nothing -> do
      person <- QP.findById (Id order.customerId) >>= fromMaybeM (PersonNotFound order.customerId)
      pure person.merchantOperatingCityId
  -- Kernel-managed wallet transfers carry their own poll pacing on the class config
  -- (both fields must be set); PayoutConfig stays the fallback and the source for
  -- non-wallet payout orders. Pacing reads the DRIVER class even for fleet owners —
  -- poll cadence is per rail behaviour, not per role.
  mbKernelPacing <-
    if order.entityName == Just DPayment.DRIVER_WALLET_TRANSACTION
      then do
        cfg <-
          FinanceConfig.getAccountConfig
            FinanceConfig.AccountClass
              { merchantId = order.merchantId,
                merchantOperatingCityId = merchantOperatingCityId.getId,
                accountType = FAccount.Liability,
                counterpartyType = Just FAccount.DRIVER,
                subLedger = Nothing
              }
        pure $
          PSC.PayoutStatusCheckConfig
            <$> (secondsToNominalDiffTime . fromIntegral <$> cfg.statusCheckIntervalSec)
            <*> cfg.statusCheckMaxAttempts
      else pure Nothing
  maybe (getPayoutStatusCheckConfigForPerson (Id order.customerId) merchantOperatingCityId) pure mbKernelPacing

getPayoutStatusCheckConfigForPerson :: (CacheFlow m r, EsqDBFlow m r) => Id DP.Person -> Id DMOC.MerchantOperatingCity -> m PSC.PayoutStatusCheckConfig
getPayoutStatusCheckConfigForPerson personId merchantOperatingCityId = do
  mbVehicle <- QV.findById personId
  let vehicleCategory = fromMaybe DVC.AUTO_CATEGORY ((.category) =<< mbVehicle)
  payoutConfig <-
    getOneConfig (PayoutConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, vehicleCategory = Just vehicleCategory, isPayoutEnabled = Nothing}) Nothing
      >>= fromMaybeM (PayoutConfigNotFound (show vehicleCategory) merchantOperatingCityId.getId)
  pure
    PSC.PayoutStatusCheckConfig
      { checkInterval = secondsToNominalDiffTime (fromIntegral payoutConfig.payoutStatusCheckInterval),
        maxAttempts = payoutConfig.payoutStatusCheckMaxAttempts
      }

scheduleNextPayoutStatusCheck :: (PayoutStatusCheckCreator m r) => DPayoutOrder.PayoutOrder -> PSC.PayoutStatusCheckJobData -> NominalDiffTime -> m ()
scheduleNextPayoutStatusCheck order jobData delay =
  createJobIn @_ @'PayoutStatusCheck (Just $ Id order.merchantId) (Id <$> order.merchantOperatingCityId) delay jobData

afterPayoutOrderCreated :: (PayoutStatusCheckCreator m r) => DPayoutOrder.PayoutOrder -> m ()
afterPayoutOrderCreated order = do
  config <- getPayoutStatusCheckConfig order
  PSC.schedulePayoutStatusCheck scheduleNextPayoutStatusCheck config order
