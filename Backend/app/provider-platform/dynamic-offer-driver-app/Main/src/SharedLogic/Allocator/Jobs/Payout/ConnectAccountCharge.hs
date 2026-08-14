{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.Payout.ConnectAccountCharge (sendConnectAccountCharge) where

import qualified Data.Time as Time
import Domain.Action.UI.DriverWallet (counterpartyFromRole)
import qualified Domain.Types.TransporterConfig as DTConf
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Scheduler
import SharedLogic.Allocator
import SharedLogic.Finance.Wallet
  ( buildDriverChargeCtx,
    connectBearerToFunder,
    recordStripeChargeLedger,
    walletReferenceConnectAccountCharges,
  )
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverBankAccountExtra as QDBA
import qualified Storage.Queries.Person as QPerson

-- | Scheduled job that deducts the Stripe connect-account maintenance charge from
--   every active connected account in the operating city, per the configured bearer.
--   Opt-in: only runs when connectAccountCharge / bearer / frequency are all set.
sendConnectAccountCharge ::
  ( EncFlow m r,
    CacheFlow m r,
    Finance.HasActorInfo m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    SchedulerFlow r,
    BeamFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasKafkaProducer r,
    HasField "blackListedJobs" r [Text],
    Redis.HedisLTSFlowEnv r
  ) =>
  Job 'ConnectAccountChargeDeduction ->
  m ExecutionResult
sendConnectAccountCharge Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
      merchantOpCityId = jobData.merchantOperatingCityId
  mbTConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing
  case mbTConfig of
    Nothing -> do
      logWarning $ "No TransporterConfig for connect-account-charge job in city " <> merchantOpCityId.getId
      pure Complete
    Just tConfig -> do
      let dwc = tConfig.driverWalletConfig
      case (dwc.connectAccountCharge, dwc.connectAccountChargeBearer, dwc.connectAccountChargeFrequency) of
        (Just charge, Just bearer, Just freq) | charge > 0 -> do
          accounts <- QDBA.findActiveConnectAccountsByCity merchantOpCityId
          logInfo $ "Posting connect-account charge for " <> show (length accounts) <> " active accounts in city " <> merchantOpCityId.getId
          forM_ accounts $ \acc -> do
            mbPerson <- QPerson.findById acc.driverId
            whenJust mbPerson $ \person -> do
              let counterparty = counterpartyFromRole person.role
                  chargeCtx = buildDriverChargeCtx counterparty acc.driverId.getId jobData.merchantId.getId merchantOpCityId.getId tConfig.currency ("ConnectAccountCharge-" <> acc.driverId.getId)
              recordStripeChargeLedger chargeCtx (connectBearerToFunder bearer) charge walletReferenceConnectAccountCharges
                >>= fromEitherM (\e -> InternalError ("Failed to post connect-account charge: " <> show e))
          nextTime <- nextConnectChargeRun freq
          pure $ ReSchedule nextTime
        _ -> do
          logInfo "Connect-account charge not fully configured (charge/bearer/frequency); stopping job."
          pure Complete

-- | Next run time based on the configured frequency (interval from now, in UTC).
nextConnectChargeRun :: (MonadFlow m) => DTConf.ChargeFrequency -> m UTCTime
nextConnectChargeRun freq = do
  now <- getCurrentTime
  pure $ case freq of
    DTConf.CHARGE_DAILY -> Time.addUTCTime Time.nominalDay now
    DTConf.CHARGE_WEEKLY -> Time.addUTCTime (7 * Time.nominalDay) now
    DTConf.CHARGE_MONTHLY -> Time.UTCTime (Time.addGregorianMonthsClip 1 (Time.utctDay now)) (Time.utctDayTime now)
