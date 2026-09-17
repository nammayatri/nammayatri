-- | The effect constraints an onboarding flag recompute needs. Kept dependency-free so the
--   document-fetch layer can use it without an import cycle.
module SharedLogic.DriverOnboarding.OnboardingFlags.Types
  ( OnboardingFlow,
    FlagSnapshot (..),
    EntityFlagChange (..),
    FlagTransition (..),
    flagTransitions,
  )
where

import qualified AWS.S3 as S3
import qualified Domain.Types.Alert.AlertEntityType as DAlertEntity
import qualified Domain.Types.Alert.OnboardingAlertAction as DOnboardingAlertAction
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import qualified Kernel.Storage.Clickhouse.Config as CH
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Commons (KafkaTopic)
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions)
import Kernel.Utils.Common

type OnboardingFlow m r =
  ( MonadFlow m,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    ServiceFlow m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig, "maxNotificationShards" ::: Int],
    HasField "broadcastMessageTopic" r KafkaTopic,
    HasField "authTokenCacheExpiry" r Seconds,
    HasField "s3Env" r (S3.S3Env m),
    HasField "externalServiceRateLimitOptions" r APIRateLimitOptions,
    HasField "imageExtractionTimeoutSec" r Seconds,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text],
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  )

data FlagSnapshot = FlagSnapshot
  { fsVerified :: Bool,
    fsApproved :: Maybe Bool,
    fsEnabled :: Maybe Bool,
    fsBlocked :: Maybe Bool,
    fsDisabledReasonFlag :: Maybe DI.DisabledReasonFlag
  }
  deriving (Show, Eq)

data EntityFlagChange = EntityFlagChange
  { efcEntityType :: DAlertEntity.AlertEntityType,
    efcEntityId :: Text,
    efcSubject :: Text,
    efcFleetOwnerId :: Maybe (Id DP.Person),
    efcMerchantId :: Id DM.Merchant,
    efcMerchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    efcOld :: FlagSnapshot,
    efcNew :: FlagSnapshot
  }
  deriving (Show)

data FlagTransition = FlagTransition
  { ftAction :: DOnboardingAlertAction.OnboardingAlertAction,
    ftParams :: [(Text, Text)]
  }
  deriving (Show, Eq)

flagTransitions :: EntityFlagChange -> [FlagTransition]
flagTransitions change =
  catMaybes
    [ transition (not old.fsVerified && new.fsVerified && isNothing new.fsApproved) DOnboardingAlertAction.DocumentApprovalPendingAction subjectParams,
      transition (isNothing old.fsApproved && new.fsApproved == Just False) DOnboardingAlertAction.RejectAction subjectParams,
      transition (old.fsApproved /= Just True && new.fsApproved == Just True) DOnboardingAlertAction.ApproveAction subjectParams,
      transition (rose fsEnabled) DOnboardingAlertAction.EnableAction subjectParams,
      transition (fell fsEnabled) DOnboardingAlertAction.DisableAction disabledParams,
      transition (rose fsBlocked) DOnboardingAlertAction.BlockAction subjectParams,
      transition (fell fsBlocked) DOnboardingAlertAction.UnblockAction subjectParams,
      transition (isNothing old.fsDisabledReasonFlag && isJust new.fsDisabledReasonFlag) DOnboardingAlertAction.DisableAction disabledParams
    ]
  where
    old = change.efcOld
    new = change.efcNew
    subject = change.efcSubject

    subjectParams = [("subject", subject)]
    disabledParams = subjectParams <> [("disabledReason", maybe "" show new.fsDisabledReasonFlag)]

    rose field = not (fromMaybe False (field old)) && fromMaybe False (field new)
    fell field = fromMaybe False (field old) && not (fromMaybe False (field new))

    transition cond action params =
      if cond then Just (FlagTransition action params) else Nothing
