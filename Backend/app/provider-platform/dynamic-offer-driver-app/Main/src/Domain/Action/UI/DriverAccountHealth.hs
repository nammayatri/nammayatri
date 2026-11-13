module Domain.Action.UI.DriverAccountHealth (getDriverAccountHealth) where

import API.Types.UI.DriverAccountHealth
import qualified Data.Map.Strict as Map
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import Kernel.Prelude
import Kernel.Types.Id (Id, cast)
import Kernel.Utils.Common
import qualified Lib.BehaviorTracker.BlockTracker as BT
import qualified Lib.BehaviorTracker.Types as BTT
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicElement as DALE
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.BehaviourManagement.RulebookSpec as RulebookSpec
import qualified SharedLogic.BehaviourManagement.Visibility as Visibility
import qualified SharedLogic.DriverPool as DP
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformation as QDriverInformation
import Tools.DynamicLogic (selectAppDynamicLogicVersion, tossForEntity)
import Tools.Error

-- | Reads only what the behaviour framework already holds: the city's live cancellation
-- rules, the driver's counters, the warnings the engine pinned and the account block.
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
  -- the version the engine judges this driver by (Lib.DriverScore): same per-driver toss, so the
  -- limits shown are the ones enforced, also during a weighted rollout
  rulesVersion <- selectAppDynamicLogicVersion (cast merchantOpCityId) LYT.CANCELLATION_RATE_BEHAVIOR localTime (Just (tossForEntity personId.getId))
  ladder <- case rulesVersion of
    Nothing -> pure []
    Just version -> do
      elements <- DALE.findByDomainAndVersion LYT.CANCELLATION_RATE_BEHAVIOR version
      let mbLadder = listToMaybe $ mapMaybe (RulebookSpec.decodeLadder <=< (.description)) elements
      when (isNothing mbLadder) $ logDebug $ "Account health: no rulebook-builder table in CANCELLATION_RATE_BEHAVIOR version " <> show version
      pure $ fromMaybe [] mbLadder
  -- the window the engine counts with (Lib.DriverScore): the counters share one cache, which a
  -- read with a shorter window would trim
  let counterConfig =
        BTT.CounterConfig
          { windowSizeDays = toInteger $ fromMaybe 7 transporterConfig.cancellationRateWindow,
            counters = [BTT.ACTION_COUNT, BTT.ELIGIBLE_COUNT],
            periods = DP.rideCancellationCounterConfig.periods,
            hashTagEntityId = DP.rideCancellationCounterConfig.hashTagEntityId
          }
  visibility <-
    Visibility.getEntityBehaviorVisibility BTT.DRIVER personId.getId . Just $
      BTT.BehaviorDomainConfig {actionTypes = [(DP.rideCancellationActionType, counterConfig)], blockReasonTags = [], blockTypes = []}
  warnings <- BT.readProfileWarnings BTT.DRIVER personId.getId
  now <- getCurrentTime
  let counters = fromMaybe Map.empty $ Map.lookup DP.rideCancellationActionType visibility.counters
      toPeriod periodConfig =
        let period = periodConfig.periodName
            counterValues = Map.findWithDefault BTT.defaultCounterValues period counters
         in CancellationPeriod
              { period,
                rate = fromIntegral counterValues.rate,
                eligibleCount = fromIntegral counterValues.eligibleCount,
                actionCount = fromIntegral counterValues.actionCount,
                warnAt = RulebookSpec.warnLimit period ladder,
                blockAt = RulebookSpec.blockLimit period ladder
              }
      -- the same check as going online: an expired block stays set until then, but no longer counts
      isBlocked = driverInfo.blocked && maybe True (> now) driverInfo.blockExpiryTime
  pure
    AccountHealthRes
      { rulesVersion,
        cancellation = map toPeriod counterConfig.periods,
        warnings,
        block = if isBlocked then Just AccountBlock {blockedUntil = driverInfo.blockExpiryTime, reasonFlag = driverInfo.blockReasonFlag} else Nothing
      }
