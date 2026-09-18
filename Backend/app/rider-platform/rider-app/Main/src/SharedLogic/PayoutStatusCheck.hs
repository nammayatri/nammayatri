module SharedLogic.PayoutStatusCheck
  ( PayoutStatusCheckCreator,
    getPayoutStatusCheckConfig,
    scheduleNextPayoutStatusCheck,
    afterPayoutOrderCreated,
  )
where

import qualified Domain.Types.VehicleCategory as DV
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import qualified Lib.Payment.Payout.StatusCheck as PSC
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.JobScheduler (RiderJobType (..))
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.PayoutConfig (PayoutConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson
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
    Just mocId -> pure mocId
    Nothing -> do
      person <- QPerson.findById (Id order.customerId) >>= fromMaybeM (PersonNotFound order.customerId)
      pure person.merchantOperatingCityId.getId
  payoutConfig <-
    getOneConfig (PayoutConfigDimensions {merchantOperatingCityId, vehicleCategory = Just DV.AUTO_CATEGORY, isPayoutEnabled = Nothing, payoutEntity = Nothing}) Nothing
      >>= fromMaybeM (PayoutConfigNotFound "AUTO_CATEGORY" merchantOperatingCityId)
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
