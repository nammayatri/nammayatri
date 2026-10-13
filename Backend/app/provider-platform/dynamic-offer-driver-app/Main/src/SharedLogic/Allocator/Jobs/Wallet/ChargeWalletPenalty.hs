{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | A penalty the behaviour engine decided (CHARGE_FEE) in a wallet-subscription city: debited from the
--   driver's own wallet right away instead of becoming a fee that waits for a daily collection. The
--   debit is posted at most once per charge (reference-checked under the wallet lock), so the job is
--   retried freely until it goes through, then the wallet floor is applied.
module SharedLogic.Allocator.Jobs.Wallet.ChargeWalletPenalty
  ( chargeWalletPenaltyJob,
  )
where

import Kernel.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Scheduler
import SharedLogic.Allocator
import qualified SharedLogic.DriverCancellationPenalty as DCP
import SharedLogic.Finance.WalletAccount (isWalletSubscriptionCity)
import qualified SharedLogic.Finance.WalletFloor as WalletFloor
import qualified SharedLogic.Finance.WalletSubscription as WalletSubscription
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson

chargeWalletPenaltyJob ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    BeamFlow m r,
    Finance.HasActorInfo m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Job 'ChargeWalletPenalty ->
  m ExecutionResult
chargeWalletPenaltyJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
      cityId = jobData.merchantOperatingCityId
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = cityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound cityId.getId)
  if isWalletSubscriptionCity transporterConfig.driverWalletConfig
    then do
      driver <- QPerson.findById jobData.driverId >>= fromMaybeM (PersonNotFound jobData.driverId.getId)
      WalletSubscription.debitBehaviourPenaltyFromWallet transporterConfig jobData.merchantId.getId cityId.getId jobData.driverId jobData.walletPenaltyAmount jobData.walletPenaltyChargeId
      WalletFloor.afterWalletDebit transporterConfig driver
      pure Complete
    else do
      -- The city left wallet subscription after the penalty was queued: it goes on the fee rail, as before.
      void $ DCP.chargeDriverPenaltyFee jobData.merchantId cityId (cast jobData.driverId) jobData.walletPenaltyAmount transporterConfig.currency transporterConfig
      pure Complete
