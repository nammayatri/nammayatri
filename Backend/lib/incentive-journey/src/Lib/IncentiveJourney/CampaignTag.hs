{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Driver-level IJ campaign membership gate as a Namma tag (not DI).
-- Wire (per-CJM): IJCampaign:<cjmId>#Assigned;daily|tb[#expiredAt]
-- Windows are NOT stored in the tag — real TimeBound lives on milestone / CJM cache.
-- Tag tbKeys are only Redis suffixes: "daily" (unbounded) or "tb" (any bounded windows).
-- Legacy value still accepted: IJCampaign:<cjmId>#<cjmId>;Assigned;… and long tb:BoundedPeaks… keys.
-- Hot path reads this from person.driverTag / LTS; no assignment DB hit.
module Lib.IncentiveJourney.CampaignTag
  ( ijCampaignTagName,
    ijCampaignTagNameFor,
    isIjCampaignTagName,
    cjmIdFromTagName,
    CampaignMembership (..),
    timeBoundKeyDaily,
    timeBoundKeyBounded,
    mkTimeBoundKeysFromMilestones,
    encodeMembershipValue,
    encodeLegacyMembershipValue,
    decodeMembershipsFromTag,
    decodeMembershipsFromTagValue,
    parseMembershipsFromTagText,
    membershipNeedsMetricTracking,
    isBehaviorRelatedMilestone,
  )
where

import Data.List (nub)
import qualified Data.Text as T
import Kernel.Prelude
import qualified Kernel.Types.TimeBound as TB
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM

-- | Legacy single-tag name (still accepted when reading).
ijCampaignTagName :: Text
ijCampaignTagName = "IJCampaign"

-- | One tag per CJM so pooling can expire each campaign independently.
ijCampaignTagNameFor :: Text -> Text
ijCampaignTagNameFor cjmId = ijCampaignTagName <> ":" <> cjmId

isIjCampaignTagName :: Text -> Bool
isIjCampaignTagName name =
  name == ijCampaignTagName || (ijCampaignTagName <> ":") `T.isPrefixOf` name

cjmIdFromTagName :: Text -> Maybe Text
cjmIdFromTagName name =
  case T.stripPrefix (ijCampaignTagName <> ":") name of
    Just cjmId | not (T.null cjmId) -> Just cjmId
    _ -> Nothing

timeBoundKeyDaily :: Text
timeBoundKeyDaily = "daily"

-- | Single Redis suffix for any BoundedByWeekday / BoundedByDay schedule.
-- In-window checks use CJM/milestone TimeBound from cache — not this string.
timeBoundKeyBounded :: Text
timeBoundKeyBounded = "tb"

data CampaignMembership = CampaignMembership
  { cjmId :: Text,
    linkType :: DCJM.CampaignUserLinkType,
    timeBoundKeys :: [Text]
  }
  deriving (Eq, Show, Generic)

-- | Redis timeBound key list from journey milestones.
-- Unbounded / missing → "daily"; any bounded schedule → "tb" (short; not the window JSON).
mkTimeBoundKeysFromMilestones :: [DIJM.IncentiveJourneyMilestone] -> [Text]
mkTimeBoundKeysFromMilestones milestones =
  let keys = nub $ concatMap milestoneTimeBoundKeys milestones
   in if null keys then [timeBoundKeyDaily] else keys

milestoneTimeBoundKeys :: DIJM.IncentiveJourneyMilestone -> [Text]
milestoneTimeBoundKeys m = case m.timeBounds of
  Nothing -> [timeBoundKeyDaily]
  Just TB.Unbounded -> [timeBoundKeyDaily]
  Just _ -> [timeBoundKeyBounded]

-- | Per-CJM tag value: linkType;tbKeys (cjmId lives in the tag name).
encodeMembershipValue :: CampaignMembership -> Text
encodeMembershipValue m =
  linkTypeToText m.linkType
    <> ";"
    <> T.intercalate "," m.timeBoundKeys

-- | Legacy blob segment: cjmId;linkType;tbKeys
encodeLegacyMembershipValue :: CampaignMembership -> Text
encodeLegacyMembershipValue m =
  m.cjmId <> ";" <> encodeMembershipValue m

decodeMembershipsFromTag :: Text -> Text -> [CampaignMembership]
decodeMembershipsFromTag name raw =
  let fragments = filter (not . T.null) $ T.splitOn "&" raw
      mbNameCjmId = cjmIdFromTagName name
   in mapMaybe (parseMembershipFragment mbNameCjmId) fragments

-- | Legacy / value-only decode (cjmId must be inside each fragment).
decodeMembershipsFromTagValue :: Text -> [CampaignMembership]
decodeMembershipsFromTagValue raw =
  mapMaybe (parseMembershipFragment Nothing) $ filter (not . T.null) $ T.splitOn "&" raw

parseMembershipsFromTagText :: Text -> [CampaignMembership]
parseMembershipsFromTagText = decodeMembershipsFromTagValue

parseMembershipFragment :: Maybe Text -> Text -> Maybe CampaignMembership
parseMembershipFragment mbNameCjmId part = case T.splitOn ";" part of
  -- New: Assigned;daily  (cjmId from tag name)
  (linkTxt : tbPart : _)
    | Just linkType <- textToLinkType linkTxt,
      Just cjmId <- mbNameCjmId ->
      Just
        CampaignMembership
          { cjmId = cjmId,
            linkType = linkType,
            timeBoundKeys = parseTimeBoundKeys tbPart
          }
  -- Legacy: cjmId;Assigned;daily
  (cjmId : linkTxt : tbPart : _)
    | not (T.null cjmId),
      Just linkType <- textToLinkType linkTxt ->
      Just
        CampaignMembership
          { cjmId = cjmId,
            linkType = linkType,
            timeBoundKeys = parseTimeBoundKeys tbPart
          }
  (linkTxt : _)
    | Just linkType <- textToLinkType linkTxt,
      Just cjmId <- mbNameCjmId ->
      Just CampaignMembership {cjmId = cjmId, linkType = linkType, timeBoundKeys = [timeBoundKeyDaily]}
  (cjmId : linkTxt : _)
    | not (T.null cjmId),
      Just linkType <- textToLinkType linkTxt ->
      Just CampaignMembership {cjmId = cjmId, linkType = linkType, timeBoundKeys = [timeBoundKeyDaily]}
  _ -> Nothing

parseTimeBoundKeys :: Text -> [Text]
parseTimeBoundKeys tbPart =
  let ks = filter (not . T.null) $ T.splitOn "," tbPart
   in if null ks then [timeBoundKeyDaily] else ks

linkTypeToText :: DCJM.CampaignUserLinkType -> Text
linkTypeToText = \case
  DCJM.Assigned -> "Assigned"
  DCJM.OptIn -> "OptIn"

textToLinkType :: Text -> Maybe DCJM.CampaignUserLinkType
textToLinkType = \case
  "Assigned" -> Just DCJM.Assigned
  "OptIn" -> Just DCJM.OptIn
  _ -> Nothing

-- | Whether to write an IJCampaign person tag (hot-path membership), not whether to bump ij:bm.
-- Assigned: tag if controlFeatures (AA/pooling/cancel) and/or behaviour milestones.
-- OptIn: tag after opt-in. ij:bm bumps are gated separately via CampaignControls.trackBehaviorMetrics.
membershipNeedsMetricTracking ::
  DCJM.CampaignUserLinkType ->
  Bool -> -- cjmHasControlFeatures
  Bool -> -- hasBehaviorMilestone
  Bool
membershipNeedsMetricTracking linkType hasControls hasBehavior =
  case linkType of
    DCJM.OptIn -> True -- tag set only after opt-in
    DCJM.Assigned -> hasControls || hasBehavior

-- | Behaviour milestones that need campaign Redis / deferred settle tracking.
isBehaviorRelatedMilestone :: DIJM.IncentiveJourneyMilestone -> Bool
isBehaviorRelatedMilestone m =
  isJust m.evaluation
    || case m.conditionType of
      DIJM.AcceptanceRate -> True
      DIJM.CancellationRate -> True
      DIJM.OnlineMinutes -> True
      DIJM.JSONLogic -> True -- evaluation.logic required; treat as behaviour-capable
      _ -> False
