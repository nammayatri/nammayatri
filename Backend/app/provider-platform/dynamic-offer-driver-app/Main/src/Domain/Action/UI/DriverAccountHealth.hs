module Domain.Action.UI.DriverAccountHealth (getDriverAccountHealth, accountHealthStatus) where

import API.Types.UI.DriverAccountHealth
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Domain.Types.DriverInformation (DriverInformation)
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Domain.Types.TransporterConfig (TransporterConfig)
import qualified Environment
import Kernel.Prelude
import Kernel.Types.Id (Id, cast)
import Kernel.Utils.Common hiding (WARNING)
import qualified Lib.BehaviorTracker.BlockTracker as BT
import qualified Lib.BehaviorTracker.Types as BTT
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicElement as DALE
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicRollout as DALR
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.BehaviourManagement.ConsequenceDispatcher as BehaviorDispatch
import qualified SharedLogic.BehaviourManagement.RulebookSpec as RulebookSpec
import qualified SharedLogic.BehaviourManagement.Visibility as Visibility
import qualified SharedLogic.DriverPool as DP
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformation as QDriverInformation
import Tools.DynamicLogic (filterActiveRollouts, selectAppDynamicLogicVersion)
import Tools.Error

-- | Reads only what the behaviour framework already holds for the driver: each shown behaviour's
-- counters and the rules of its live rulebook, the warnings the engine pinned and the account block.
getDriverAccountHealth ::
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow AccountHealthRes
  )
getDriverAccountHealth (mbPersonId, _merchantId, merchantOpCityId) = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Driver not authenticated")
  driverInfo <- QDriverInformation.findById (cast personId) >>= fromMaybeM DriverInfoNotFound
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let shown = [(behaviour, behaviour.counterConfig transporterConfig) | behaviour <- shownBehaviours]
  visibility <-
    Visibility.getEntityBehaviorVisibility BTT.DRIVER personId.getId . Just $
      BTT.BehaviorDomainConfig {actionTypes = [(behaviour.actionType, counterConfig) | (behaviour, counterConfig) <- shown], blockReasonTags = [], blockTypes = []}
  behaviours <- forM shown $ \(behaviour, counterConfig) -> do
    rules <- liveRules merchantOpCityId localTime behaviour.domain
    let counters = fromMaybe Map.empty $ Map.lookup behaviour.actionType visibility.counters
        toPeriod periodConfig =
          let counterValues = Map.findWithDefault BTT.defaultCounterValues periodConfig.periodName counters
           in BehaviourPeriod
                { period = periodConfig.periodName,
                  actionCount = fromIntegral counterValues.actionCount,
                  eligibleCount = fromIntegral counterValues.eligibleCount,
                  rate = fromIntegral counterValues.rate
                }
    pure
      BehaviourHealth
        { behaviour = show behaviour.domain,
          actionType = behaviour.actionType,
          periods = map toPeriod counterConfig.periods,
          rules
        }
  warnings <- BT.readProfileWarnings BTT.DRIVER personId.getId
  now <- getCurrentTime
  pure
    AccountHealthRes
      { status = accountHealthStatus now driverInfo warnings,
        behaviours,
        warnings,
        block = if isBlocked now driverInfo then Just AccountBlock {blockedUntil = driverInfo.blockExpiryTime, reasonFlag = driverInfo.blockReasonFlag} else Nothing
      }

-- | A behaviour the account health screen shows: its rulebook domain, the counter its rules
-- read, and that counter's config.
data ShownBehaviour = ShownBehaviour
  { domain :: LYT.LogicDomain,
    actionType :: Text,
    counterConfig :: TransporterConfig -> BTT.CounterConfig
  }

-- | The behaviours shown, in screen order. Another behaviour is one more entry here.
shownBehaviours :: [ShownBehaviour]
shownBehaviours =
  [ ShownBehaviour
      { domain = LYT.CANCELLATION_RATE_BEHAVIOR,
        actionType = DP.rideCancellationActionType,
        counterConfig = rideCancellationCounterConfig
      }
  ]

-- | The window the engine counts cancellations with (Lib.DriverScore): the counters share one
-- cache, which a read with a shorter window would trim.
rideCancellationCounterConfig :: TransporterConfig -> BTT.CounterConfig
rideCancellationCounterConfig transporterConfig =
  BTT.CounterConfig
    { windowSizeDays = toInteger $ fromMaybe 7 transporterConfig.cancellationRateWindow,
      counters = [BTT.ACTION_COUNT, BTT.ELIGIBLE_COUNT],
      periods = DP.rideCancellationCounterConfig.periods,
      hashTagEntityId = DP.rideCancellationCounterConfig.hashTagEntityId
    }

-- | The rules of the domain's live rulebook version, picked like the engine does, so with one live
-- version they are the ones enforced; during a split rollout each pick is random, so the screen
-- may show another version's rules. Empty when the city has no live version or its version has no
-- rulebook-builder table.
liveRules :: Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> UTCTime -> LYT.LogicDomain -> Environment.Flow [BehaviourRule]
liveRules merchantOpCityId localTime domain = do
  mbVersion <- selectAppDynamicLogicVersion (cast merchantOpCityId) domain localTime Nothing
  liveVersions <- nub . map (.version) . filter (\rollout -> rollout.percentageRollout > 0) . filterActiveRollouts <$> DALR.findByMerchantOpCityAndDomainWithFallback (cast merchantOpCityId) domain
  when (length liveVersions > 1) $ logWarning $ "Account health: split " <> show domain <> " rollout, shown rules may differ from the enforced ones; live versions " <> show liveVersions
  case mbVersion of
    Nothing -> pure []
    Just version -> do
      elements <- DALE.findByDomainAndVersion domain version
      let mbRules = listToMaybe $ mapMaybe (RulebookSpec.decodeRules BehaviorDispatch.parseBlockReasonFlag <=< (.description)) elements
      when (isNothing mbRules) $ logDebug $ "Account health: no rulebook-builder table in " <> show domain <> " version " <> show version
      pure $ fromMaybe [] mbRules

-- | The driver's overall standing from what the engine already set: an unexpired block, else a pinned warning.
accountHealthStatus :: UTCTime -> DriverInformation -> [BTT.ProfileWarningInfo] -> AccountHealthStatus
accountHealthStatus now driverInfo warnings
  | isBlocked now driverInfo = BLOCKED
  | not (null warnings) = WARNING
  | otherwise = GOOD

-- | The same check as going online: an expired block stays set until then, but no longer counts.
isBlocked :: UTCTime -> DriverInformation -> Bool
isBlocked now driverInfo = driverInfo.blocked && maybe True (> now) driverInfo.blockExpiryTime
