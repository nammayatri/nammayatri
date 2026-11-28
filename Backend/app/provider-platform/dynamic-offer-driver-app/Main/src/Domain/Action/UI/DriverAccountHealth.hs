module Domain.Action.UI.DriverAccountHealth
  ( getDriverAccountHealth,
    DriverBlock (..),
    driverBlockOf,
    overchargingBlock,
    accountHealthStatus,
    accountBlock,
    CandidateBlock (..),
    candidateBlocks,
    activeBlocks,
    periodsToShow,
    eventsPeriod,
  )
where

import API.Types.UI.DriverAccountHealth
import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import Data.List (nub, sortOn)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Domain.Types.Common as DTCommon
import Domain.Types.DriverInformation (DriverInformation)
import qualified Domain.Types.DriverInformation as DDI
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Domain.Types.TransporterConfig (TransporterConfig)
import qualified Domain.Types.TransporterConfig as DTC
import qualified Environment
import Kernel.Prelude
import Kernel.Types.Id (Id, cast)
import qualified Kernel.Types.Id as KId
import Kernel.Utils.Common hiding (WARNING)
import qualified Lib.BehaviorTracker.Accumulator as BTA
import qualified Lib.BehaviorTracker.BlockTracker as BT
import qualified Lib.BehaviorTracker.EventsWindow as BTEW
import qualified Lib.BehaviorTracker.Types as BTT
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicRollout as DALR
import qualified Lib.Yudhishthira.Tools.TimeBoundRollout as LYTB
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.AppDynamicLogicElement (AppDynamicLogicElement)
import qualified Lib.Yudhishthira.Types.AppDynamicLogicElement as DALET
import qualified Lib.Yudhishthira.Types.AppDynamicLogicRollout as DALRT
import qualified SharedLogic.BehaviourManagement.ConsequenceDispatcher as BehaviorDispatch
import qualified SharedLogic.BehaviourManagement.EventsWindow as EventsWindow
import qualified SharedLogic.BehaviourManagement.Packs as Packs
import qualified SharedLogic.BehaviourManagement.RulebookSpec as RulebookSpec
import qualified SharedLogic.BehaviourManagement.Visibility as Visibility
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformation as QDriverInformation
import qualified Storage.Queries.Person as QPerson
import Tools.DynamicLogic (filterActiveRollouts, getAppDynamicLogicElements)
import Tools.Error

-- | Reads only what the behaviour framework already holds for the driver: each shown behaviour's
-- counters and the rules of its live rulebook, the warnings the engine pinned, the account block,
-- and every framework block still in force. When the live version judges the last X events, the
-- counts of those events come as one more period, "events".
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
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  -- Every behaviour live in the city is shown, in pack order.
  live <- fmap catMaybes . forM Packs.behaviourPacks $ \pack -> do
    (rows, mbVersion) <- liveVersion merchantOpCityId localTime pack.packDomain
    if isNothing mbVersion
      then pure Nothing
      else do
        rules <- liveRules pack.packDomain mbVersion rows
        eventsPeriods <- readEventsPeriod personId pack.packDomain rows
        pure $ Just (pack, rules, eventsPeriods)
  behaviours <- forM live $ \(pack, rules, eventsPeriods) -> do
    periods <- maybe (pure []) (judgedPeriods personId transporterConfig pack.packDomain rules) pack.counting
    pure BehaviourHealth {behaviour = show pack.packDomain, periods = periods <> eventsPeriods, rules}
  warnings <- BT.readProfileWarnings BTT.DRIVER personId.getId
  now <- getCurrentTime
  let driverBlock = driverBlockOf driverInfo
      mbOverchargingTill = fst <$> overchargingBlock transporterConfig localTime person.driverTag
      candidates = candidateBlocks now driverBlock
      -- the framework's records are looked up by tag: the registry's tags, and the ones the live
      -- rules block with (a city may name its own)
      ruleBlockTags = [tag | (_, rules, _) <- live, rule <- rules, rule.kind `elem` blockRuleKinds, Just tag <- [rule.tag]]
  records <- readBlockRecords personId (nub $ Visibility.defaultDriverDomainConfig.blockReasonTags <> ruleBlockTags) candidates
  pure
    AccountHealthRes
      { status = accountHealthStatus now driverBlock (isJust mbOverchargingTill) warnings,
        behaviours,
        warnings,
        block = accountBlock now driverBlock mbOverchargingTill,
        blocks = activeBlocks driverBlock candidates records
      }

-- | Rule kinds that put a block on the driver.
blockRuleKinds :: [Text]
blockRuleKinds = ["HARD_BLOCK", "PERMANENT_BLOCK", "SOFT_BLOCK", "FEATURE_BLOCK"]

-- | The counts the rules judge, per period, as the engine counts them: the offence counter's
-- actions out of the eligible counter's events (Lib.BehaviorTracker's
-- buildCounterValuesWithEligible, which the engine's snapshot uses), with the counter's settings
-- from the framework's registry as the engine counts with them in this city.
judgedPeriods :: Id Domain.Types.Person.Person -> TransporterConfig -> LYT.LogicDomain -> [BehaviourRule] -> Packs.PackCounting -> Environment.Flow [BehaviourPeriod]
judgedPeriods personId transporterConfig domain rules counting =
  case lookup counting.offenceCounter Visibility.defaultDriverDomainConfig.actionTypes of
    Nothing -> do
      logError $ "Account health: " <> show domain <> " counts in " <> counting.offenceCounter <> ", which the behaviour framework's counter registry doesn't list, so its counts are left out"
      pure []
    Just registryConfig -> do
      let config = counting.cityCounterConfig transporterConfig registryConfig
      forM (periodsToShow rules config.periods) $ \periodConfig -> do
        counterValues <-
          BTA.buildCounterValuesWithEligible config.hashTagEntityId BTT.DRIVER counting.offenceCounter counting.eligibleCounter personId.getId periodConfig.periodDays config.windowSizeDays
        pure
          BehaviourPeriod
            { period = periodConfig.periodName,
              actionCount = fromIntegral counterValues.actionCount,
              eligibleCount = fromIntegral counterValues.eligibleCount,
              rate = fromIntegral counterValues.rate,
              windowSize = Nothing
            }

-- | The counter periods the rules read (counters.<period>.*; the "events" period is read apart).
-- A version without readable rules shows all of the counter's periods.
periodsToShow :: [BehaviourRule] -> [BTT.PeriodConfig] -> [BTT.PeriodConfig]
periodsToShow [] periods = periods
periodsToShow rules periods = filter ((`elem` readPeriods) . (.periodName)) periods
  where
    readPeriods =
      [ periodName
        | rule <- rules,
          condition <- rule.conditions,
          Just fact <- [T.stripPrefix "counters." condition.fact],
          let periodName = T.takeWhile (/= '.') fact,
          periodName /= BTEW.eventsCounterName
      ]

-- | The domain's live rulebook version and its rows, picked the way the engine's rule runner
-- picks them, so with one live version they are the ones enforced; during a split rollout each
-- pick is random, so the screen may show another version than the one that fired.
liveVersion :: Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> UTCTime -> LYT.LogicDomain -> Environment.Flow ([AppDynamicLogicElement], Maybe Int)
liveVersion merchantOpCityId localTime domain = do
  rollouts <- filterActiveRollouts <$> DALR.findByMerchantOpCityAndDomainWithFallback (cast merchantOpCityId) domain
  -- the same time-bound filter as the engine's pick: versions live at other hours aren't a split
  mbWindowName <- LYTB.activeTimeBoundName (cast merchantOpCityId) domain localTime
  let liveVersions = nub . map (.version) . filter (\rollout -> rollout.percentageRollout > 0) $ LYTB.applicableForTimeBound mbWindowName rollouts
  when (length liveVersions > 1) $ logWarning $ "Account health: split " <> show domain <> " rollout, shown rules may differ from the enforced ones; live versions " <> show liveVersions
  -- not live in the city: nothing more to read
  if null liveVersions
    then pure ([], Nothing)
    else getAppDynamicLogicElements (cast merchantOpCityId) domain localTime Nothing Nothing

-- | The rules of the version's rows. Empty when no version is live or the version has no
-- rulebook-builder table. A table that is there but can't be read also gives none, and is logged
-- as a warning: that version is still enforced, but the screen shows no rules for it.
liveRules :: LYT.LogicDomain -> Maybe Int -> [AppDynamicLogicElement] -> Environment.Flow [BehaviourRule]
liveRules domain mbVersion rows = do
  let descriptions = mapMaybe (.description) rows
      mbRules = listToMaybe $ mapMaybe (RulebookSpec.decodeRules BehaviorDispatch.parseBlockReasonFlag) descriptions
  when (isJust mbVersion && isNothing mbRules) $
    if any RulebookSpec.hasRulebookTable descriptions
      then logWarning $ "Account health: the rulebook-builder table of " <> show domain <> " version " <> show mbVersion <> " couldn't be read, so no rules are shown for it"
      else logDebug $ "Account health: no rulebook-builder table in " <> show domain <> " version " <> show mbVersion
  pure $ fromMaybe [] mbRules

-- | The "events" period when the version judges the last X events (its settings) and the behaviour
-- keeps an events window (its pack); none for a daily/weekly version. A window that can't be read
-- leaves the period out rather than failing the screen.
readEventsPeriod :: Id Domain.Types.Person.Person -> LYT.LogicDomain -> [AppDynamicLogicElement] -> Environment.Flow [BehaviourPeriod]
readEventsPeriod personId domain rows =
  case (EventsWindow.eventsWindowSizeOf (EventsWindow.rowsSettings rows), Packs.findEventsSupport domain) of
    (Just lastX, Just support) -> do
      result <- withTryCatch "accountHealth:eventsWindowRead" $ BTEW.getWindow support.windowEntity support.windowKeyName personId.getId
      case result of
        Right window -> pure [eventsPeriod lastX window]
        Left err -> do
          logError $ "Account health: the " <> show domain <> " events window of driver " <> personId.getId <> " couldn't be read, so its events period is left out: " <> show err
          pure []
    _ -> pure []

-- | The newest lastX events of a window, counted as the engine counts them for counters.events.
eventsPeriod :: Int -> Text -> BehaviourPeriod
eventsPeriod lastX window =
  let (counterValues, _) = BTEW.countLast lastX window
   in BehaviourPeriod
        { period = BTEW.eventsCounterName,
          actionCount = fromIntegral counterValues.actionCount,
          eligibleCount = fromIntegral counterValues.eligibleCount,
          rate = fromIntegral counterValues.rate,
          windowSize = Just lastX
        }

-- | The block and restriction fields of DriverInformation: what is in force right now.
data DriverBlock = DriverBlock
  { blocked :: Bool,
    blockExpiryTime :: Maybe UTCTime,
    blockReasonFlag :: Maybe BlockReasonFlag,
    -- | who set the account block: the framework, or a dashboard user
    blockStateModifier :: Maybe Text,
    softBlockExpiryTime :: Maybe UTCTime,
    softBlockStiers :: Maybe [DTCommon.ServiceTierType],
    tollRouteBlockedTill :: Maybe UTCTime,
    acRestricted :: Bool
  }

driverBlockOf :: DriverInformation -> DriverBlock
driverBlockOf driverInfo =
  DriverBlock
    { blocked = driverInfo.blocked,
      blockExpiryTime = driverInfo.blockExpiryTime,
      blockReasonFlag = driverInfo.blockReasonFlag,
      blockStateModifier = driverInfo.blockStateModifier,
      softBlockExpiryTime = driverInfo.softBlockExpiryTime,
      softBlockStiers = driverInfo.softBlockStiers,
      tollRouteBlockedTill = driverInfo.tollRouteBlockedTill,
      acRestricted = driverInfo.acUsageRestrictionType == DDI.ToggleNotAllowed
    }

-- | The driver's overall standing from what is in force, most severe first: BLOCKED when the
-- driver can't go online (their account block, or the overcharging block the profile also
-- reports); RESTRICTED when they can but are losing service tiers (soft block) or a feature (toll
-- routes, AC); WARNING when the engine pinned a warning; else GOOD. Read from DriverInformation
-- alone, so the profile computes the same status without the framework's block records.
accountHealthStatus :: UTCTime -> DriverBlock -> Bool -> [BTT.ProfileWarningInfo] -> AccountHealthStatus
accountHealthStatus now driverBlock overchargingBlocked warnings
  | isBlocked now driverBlock || overchargingBlocked = BLOCKED
  | isRestricted now driverBlock = RESTRICTED
  | not (null warnings) = WARNING
  | otherwise = GOOD

-- | A soft block or a feature block in force.
isRestricted :: UTCTime -> DriverBlock -> Bool
isRestricted now driverBlock =
  maybe False (> now) driverBlock.softBlockExpiryTime
    || maybe False (> now) driverBlock.tollRouteBlockedTill
    || driverBlock.acRestricted

-- | A block driver_information shows in force, before the framework's record of it is matched.
data CandidateBlock = CandidateBlock
  { candidateType :: ActiveBlockType,
    -- | when it ends, as driver_information holds it; Nothing for a permanent block or the AC
    -- restriction (which keeps no end there)
    candidateEnd :: Maybe UTCTime,
    candidateFeature :: Maybe Text,
    candidateTiers :: Maybe [DTCommon.ServiceTierType]
  }

-- | Every block in force, from driver_information alone (the source of truth: a dashboard unblock
-- clears it, but never the framework's records). Blocks that stop the driver going online first.
candidateBlocks :: UTCTime -> DriverBlock -> [CandidateBlock]
candidateBlocks now driverBlock =
  [ CandidateBlock {candidateType = if isJust driverBlock.blockExpiryTime then HARD else PERMANENT, candidateEnd = driverBlock.blockExpiryTime, candidateFeature = Nothing, candidateTiers = Nothing}
    | isBlocked now driverBlock
  ]
    <> [ CandidateBlock {candidateType = SOFT, candidateEnd = driverBlock.softBlockExpiryTime, candidateFeature = Nothing, candidateTiers = driverBlock.softBlockStiers}
         | maybe False (> now) driverBlock.softBlockExpiryTime
       ]
    <> [ CandidateBlock {candidateType = FEATURE, candidateEnd = driverBlock.tollRouteBlockedTill, candidateFeature = Just "TOLL_ROUTES", candidateTiers = Nothing}
         | maybe False (> now) driverBlock.tollRouteBlockedTill
       ]
    <> [CandidateBlock {candidateType = FEATURE, candidateEnd = Nothing, candidateFeature = Just "AC_USAGE", candidateTiers = Nothing} | driverBlock.acRestricted]

-- | The framework's block records of the types the candidates have, under these tags. No
-- candidate, no read: most drivers cost no lookup.
readBlockRecords :: Id Domain.Types.Person.Person -> [Text] -> [CandidateBlock] -> Environment.Flow [BTT.BlockInfo]
readBlockRecords personId tags candidates =
  fmap (map fst . catMaybes) . forM [(blockType, tag) | blockType <- nub (map (recordTypeOf . (.candidateType)) candidates), tag <- tags] $
    \(blockType, tag) -> BT.readBlockKey BTT.DRIVER personId.getId blockType tag

recordTypeOf :: ActiveBlockType -> BTT.BlockType
recordTypeOf = \case
  HARD -> BTT.HARD_BLOCK
  PERMANENT -> BTT.PERMANENT_BLOCK
  SOFT -> BTT.SOFT_BLOCK
  FEATURE -> BTT.FEATURE_BLOCK

-- | Each candidate as the app shows it, with the tag and reason of the framework record that put
-- it there, when one matches: same type and, for a timed block, the same end (the column and the
-- record are written a moment apart); an account block only when the framework set it. A record
-- that matches nothing (lifted, or replaced by a newer block) is ignored; a candidate that
-- matches no record (a dashboard block, a tag the lookup didn't know) comes without a tag.
activeBlocks :: DriverBlock -> [CandidateBlock] -> [BTT.BlockInfo] -> [ActiveBlock]
activeBlocks driverBlock candidates records = map toActiveBlock candidates
  where
    toActiveBlock candidate =
      let mbRecord = listToMaybe . sortOn (endDistance candidate) $ filter (matches candidate) records
       in ActiveBlock
            { blockType = candidate.candidateType,
              tag = (.reasonTag) <$> mbRecord,
              reason = (.blockReason) <$> mbRecord,
              -- the AC restriction keeps no end in driver_information: its record's end, if any
              blockedUntil = candidate.candidateEnd <|> (if candidate.candidateFeature == Just "AC_USAGE" then (.expiresAt) =<< mbRecord else Nothing),
              feature = candidate.candidateFeature,
              serviceTiers = candidate.candidateTiers
            }
    matches candidate record =
      record.blockType == recordTypeOf candidate.candidateType && case candidate.candidateType of
        HARD -> byFramework && sameEnd candidate.candidateEnd record.expiresAt
        PERMANENT -> byFramework
        SOFT -> sameEnd candidate.candidateEnd record.expiresAt
        FEATURE ->
          (A.String <$> candidate.candidateFeature) == Just record.extraParams
            && (candidate.candidateFeature == Just "AC_USAGE" || sameEnd candidate.candidateEnd record.expiresAt)
    byFramework = driverBlock.blockStateModifier == Just frameworkBlockModifier
    sameEnd (Just columnEnd) (Just recordEnd) = abs (diffUTCTime columnEnd recordEnd) <= 600
    sameEnd _ _ = False
    endDistance candidate record = case (candidate.candidateEnd, record.expiresAt) of
      (Just columnEnd, Just recordEnd) -> abs (diffUTCTime columnEnd recordEnd)
      _ -> 0

-- | The blockStateModifier the framework writes with its account blocks (ConsequenceDispatcher).
frameworkBlockModifier :: Text
frameworkBlockModifier = "BehaviorManagementFramework"

-- | The block to show: the driver's own block, else the overcharging block (given by when it ends).
-- Its reasonFlag is reported only with an expiry: only a timed block sets the flag, and a later
-- untimed block (permanent, or from the dashboard) leaves the old one in place.
accountBlock :: UTCTime -> DriverBlock -> Maybe UTCTime -> Maybe AccountBlock
accountBlock now driverBlock mbOverchargingTill
  | isBlocked now driverBlock =
    Just AccountBlock {blockedUntil = driverBlock.blockExpiryTime, reasonFlag = driverBlock.blockReasonFlag <* driverBlock.blockExpiryTime}
  | otherwise = (\till -> AccountBlock {blockedUntil = Just till, reasonFlag = Nothing}) <$> mbOverchargingTill

-- | The same check as going online: an expired block stays set until then, but no longer counts.
isBlocked :: UTCTime -> DriverBlock -> Bool
isBlocked now driverBlock = driverBlock.blocked && maybe True (> now) driverBlock.blockExpiryTime

-- | The overcharging block the profile reports, as (blocked until, charging-behaviour tag) while it
-- is on: a high overcharging tag with an unexpired OverchargingBlockedTill, in a city with the
-- blocker enabled.
overchargingBlock :: TransporterConfig -> UTCTime -> Maybe [LYT.TagNameValueExpiry] -> Maybe (UTCTime, A.Value)
overchargingBlock transporterConfig currentLocalTime personTags = do
  guard transporterConfig.enableOverchargingBlocker
  let driverTags = Yudhishthira.convertTags $ fromMaybe [] personTags
  overchargingTag <- Yudhishthira.accessTagKey (LYT.TagName "DriverChargingBehaviour") driverTags
  blockedTillValue <- Yudhishthira.accessTagKey (LYT.TagName "OverchargingBlockedTill") driverTags
  blockedTillEpoch :: Integer <- case A.fromJSON blockedTillValue of
    A.Success epoch -> Just epoch
    A.Error _ -> Nothing
  let blockedTill = posixSecondsToUTCTime (fromInteger (blockedTillEpoch - fromIntegral transporterConfig.timeDiffFromUtc))
  guard (overchargingTag `elem` ["SuperOverCharging", "HighOverCharging", "MediumOverCharginge"] && currentLocalTime < blockedTill)
  pure (blockedTill, overchargingTag)
