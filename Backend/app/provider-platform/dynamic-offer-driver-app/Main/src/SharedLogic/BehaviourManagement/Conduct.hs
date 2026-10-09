{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | What a driver is shown about the consequences currently applied to them: which
-- one wins (priority), how registry entries are checked against enforcement state
-- (driver_information), and how its message is looked up and filled in.
-- Everything here is pure; the IO lives in the dispatcher and Domain.Action.UI.DriverConduct.
module SharedLogic.BehaviourManagement.Conduct
  ( programmeTag,
    activeFromAction,
    parseBlockReasonFlag,
    reconcileWithDriverInfo,
    consequencePriority,
    selectCurrent,
    messageKeyCandidates,
    renderPlaceholders,
    placeholderValues,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.List as List
import Data.Ord (Down (..))
import Data.Scientific (floatingOrInteger)
import qualified Data.Text as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Domain.Types.DriverInformation as DI
import Kernel.Prelude
import Kernel.Utils.Common
import Lib.BehaviorTracker.ActiveConsequences (ActiveConsequence (..))
import qualified Lib.ConsequenceEngine.Types as CET
import qualified Lib.Yudhishthira.Types as LYT
import Tools.Error (BlockReasonFlag (..))

-- | WARN and CHARGE_FEE have no duration of their own; they stay visible this long.
displayHoursForNotices :: Int
displayHoursForNotices = 24

-- | Stable programme key, e.g. RATING_BEHAVIOR (LogicDomain's Show is kebab-case).
programmeTag :: LYT.LogicDomain -> Text
programmeTag = T.replace "-" "_" . show

-- | Registry entry for a consequence that was just applied, or Nothing for
-- consequences the driver isn't shown (nudges are already overlay pushes; counters,
-- tags and auto-assign opt-outs are internal).
activeFromAction :: UTCTime -> Maybe LYT.LogicDomain -> CET.ConsequenceAction -> Maybe ActiveConsequence
activeFromAction now mbDomain = \case
  CET.HardBlock p ->
    Just $
      entry "HARD_BLOCK" (Just $ fromMaybe "HARD_BLOCK" p.blockReasonTag) (hoursFromNow p.blockDurationHours) $
        A.object ["hours" A..= p.blockDurationHours, "reason" A..= p.blockReason]
  CET.PermanentBlock p ->
    Just $
      entry "PERMANENT_BLOCK" (Just $ fromMaybe "PERMANENT_BLOCK" p.blockReasonTag) Nothing $
        A.object ["reason" A..= p.blockReason]
  CET.SoftBlock p ->
    Just $
      entry "SOFT_BLOCK" (Just $ fromMaybe "SOFT_BLOCK" p.blockReasonTag) (hoursFromNow p.blockDurationHours) $
        A.object ["hours" A..= p.blockDurationHours, "reason" A..= p.blockReason, "serviceTiers" A..= fromMaybe [] p.blockedServiceTiers]
  CET.FeatureBlock p ->
    Just $
      entry "FEATURE_BLOCK" (Just $ fromMaybe p.featureName p.blockReasonTag) (hoursFromNow p.blockDurationHours) $
        A.object ["hours" A..= p.blockDurationHours, "reason" A..= p.blockReason, "feature" A..= p.featureName]
  CET.Warn p ->
    Just $ entry "WARN" (Just p.warnKey) (hoursFromNow displayHoursForNotices) $ A.object []
  CET.ChargeFee p
    | p.penaltyAmount > 0 ->
      Just $
        entry "CHARGE_FEE" p.feeCooldownTag (hoursFromNow displayHoursForNotices) $
          A.object ["amount" A..= p.penaltyAmount, "currency" A..= p.currency, "reason" A..= p.chargeReason]
  _ -> Nothing
  where
    entry cType tag till extra =
      ActiveConsequence {consequenceType = cType, programme = programmeTag <$> mbDomain, reasonTag = tag, appliedAt = now, validTill = till, params = extra}
    -- 0 hours means "no end" for blocks (the dispatcher schedules no unblock job then).
    hoursFromNow h = if h > 0 then Just (addUTCTime (fromIntegral h * 3600) now) else Nothing

-- | Map blockReasonTag text to BlockReasonFlag enum
parseBlockReasonFlag :: Maybe Text -> BlockReasonFlag
parseBlockReasonFlag = \case
  Just "CancellationRateDaily" -> CancellationRateDaily
  Just "CancellationRateWeekly" -> CancellationRateWeekly
  Just "CancellationRate" -> CancellationRate
  Just "ExtraFareDaily" -> ExtraFareDaily
  Just "ExtraFareWeekly" -> ExtraFareWeekly
  Just "DrunkAndDriveViolation" -> DrunkAndDriveViolation
  Just "DocumentExpiry" -> DocumentExpiry
  Just "PickupStall" -> PickupStall
  Just "LOW_RATING_BLOCK" -> LowRating
  Just "ByDashboard" -> ByDashboard
  Just other -> fromMaybe ByDashboard (readMaybe $ toString other)
  Nothing -> ByDashboard

paramText :: Text -> A.Value -> Maybe Text
paramText key (A.Object o) = case AKM.lookup (AK.fromText key) o of
  Just (A.String t) -> Just t
  _ -> Nothing
paramText _ _ = Nothing

-- | Is this engine block the one driver_information currently holds? Timed blocks are
-- matched on their reason flag; a permanent block on "blocked with no expiry".
isHeldBlock :: DI.DriverInformation -> ActiveConsequence -> Bool
isHeldBlock di c = case c.consequenceType of
  "HARD_BLOCK" -> Just (parseBlockReasonFlag c.reasonTag) == di.blockReasonFlag
  "PERMANENT_BLOCK" -> isNothing di.blockExpiryTime
  _ -> False

-- | Keep only registry entries still enforced, with end times taken from
-- driver_information (the source of truth), and add entries for restrictions that
-- were applied outside the behaviour engine (dashboard blocks, legacy cancellation
-- blocks, issue-breach soft blocks) so they are shown too.
reconcileWithDriverInfo :: UTCTime -> DI.DriverInformation -> [ActiveConsequence] -> [ActiveConsequence]
reconcileWithDriverInfo now di entries = engineBlocks <> diBlock <> softBlocks <> diSoftBlock <> others
  where
    future = maybe False (> now)
    hardBlockActive = di.blocked && maybe True (> now) di.blockExpiryTime
    softBlockActive = future di.softBlockExpiryTime

    -- An engine block is shown only if it is the block driver_information holds.
    engineBlocks =
      [ c {validTill = di.blockExpiryTime}
        | hardBlockActive,
          c <- entries,
          isHeldBlock di c
      ]
    diBlock =
      [ ActiveConsequence
          { consequenceType = "HARD_BLOCK",
            programme = Nothing,
            reasonTag = show <$> di.blockReasonFlag,
            appliedAt = now,
            validTill = di.blockExpiryTime,
            params = A.object ["reason" A..= di.blockedReason]
          }
        | hardBlockActive,
          null engineBlocks
      ]
    softBlocks = [c {validTill = di.softBlockExpiryTime} | softBlockActive, c <- entries, c.consequenceType == "SOFT_BLOCK"]
    diSoftBlock =
      [ ActiveConsequence
          { consequenceType = "SOFT_BLOCK",
            programme = Nothing,
            reasonTag = di.softBlockReasonFlag,
            appliedAt = now,
            validTill = di.softBlockExpiryTime,
            params = A.object ["serviceTiers" A..= (map show (fromMaybe [] di.softBlockStiers) :: [Text]), "reason" A..= di.softBlockReasonFlag]
          }
        | softBlockActive,
          null softBlocks
      ]
    others = filter keepOther entries
    keepOther c = case c.consequenceType of
      "FEATURE_BLOCK"
        | paramText "feature" c.params == Just "TOLL_ROUTES" -> future di.tollRouteBlockedTill
      t -> t `notElem` ["HARD_BLOCK", "PERMANENT_BLOCK", "SOFT_BLOCK"]

-- | Higher shows first.
consequencePriority :: Text -> Int
consequencePriority = \case
  "PERMANENT_BLOCK" -> 100
  "HARD_BLOCK" -> 90
  "SOFT_BLOCK" -> 70
  "FEATURE_BLOCK" -> 60
  "CHARGE_FEE" -> 40
  "WARN" -> 30
  _ -> 0

-- | The one consequence to show now: highest priority; among equals the one ending
-- soonest (no end sorts last), then the most recently applied. When it ends, the
-- next call naturally returns the next one.
selectCurrent :: UTCTime -> [ActiveConsequence] -> Maybe ActiveConsequence
selectCurrent now =
  listToMaybe
    . List.sortOn (\c -> (Down (consequencePriority c.consequenceType), endKey c.validTill, Down c.appliedAt))
    . filter (\c -> maybe True (> now) c.validTill)
  where
    endKey = maybe (1 :: Int, Nothing) (\t -> (0, Just t))

-- | Overlay keys tried in order for the message: most specific first.
-- Upper-cased, so a flag like ByDashboard becomes CONDUCT_HARD_BLOCK_BYDASHBOARD.
messageKeyCandidates :: ActiveConsequence -> [Text]
messageKeyCandidates c =
  List.nub . map T.toUpper $
    [base <> "_" <> tag | Just tag <- [c.reasonTag]]
      <> [base <> "_" <> prog | Just prog <- [c.programme]]
      <> [base]
  where
    base = "CONDUCT_" <> c.consequenceType

-- | Values for {placeholders} in the overlay title/description. {until} is in the
-- city's local time; absent values are simply not substituted.
placeholderValues :: Seconds -> ActiveConsequence -> [(Text, Text)]
placeholderValues (Seconds tzOffset) c =
  catMaybes
    [ ("until",) . fmtLocal <$> c.validTill,
      ("hours",) <$> paramShown "hours",
      ("serviceTiers",) <$> tiers,
      ("feature",) <$> paramText "feature" c.params,
      ("amount",) <$> paramShown "amount",
      ("currency",) <$> paramText "currency" c.params,
      ("reason",) <$> paramText "reason" c.params
    ]
  where
    fmtLocal t = T.pack $ formatTime defaultTimeLocale "%d %b, %I:%M %p" (addUTCTime (fromIntegral tzOffset) t)
    paramShown key = case c.params of
      A.Object o -> case AKM.lookup (AK.fromText key) o of
        Just (A.Number n) -> Just $ either showDouble showInteger (floatingOrInteger n)
        Just (A.String t) -> Just t
        _ -> Nothing
      _ -> Nothing
    showDouble :: Double -> Text
    showDouble = show
    showInteger :: Integer -> Text
    showInteger = show
    tiers = case c.params of
      A.Object o -> case AKM.lookup "serviceTiers" o of
        Just (A.Array xs) -> Just . T.intercalate ", " $ [t | A.String t <- toList xs]
        _ -> Nothing
      _ -> Nothing

-- | Replace each {name} with its value.
renderPlaceholders :: [(Text, Text)] -> Text -> Text
renderPlaceholders values txt = foldl' (\acc (k, v) -> T.replace ("{" <> k <> "}") v acc) txt values
