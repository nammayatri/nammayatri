{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Pure display / overlay-copy helpers shared by driver and rider apps.
module Lib.IncentiveJourney.Display
  ( -- * Journey list ordering
    orderJourneyAssignmentPairsForDisplay,
    orderJourneysForDisplay,
    selectPreferredJourney,

    -- * Overlay template keys
    milestoneCompletedOverlayKey,
    milestoneWaivedOverlayKey,

    -- * Milestone target / template text
    aggregatedDisplayConditionValue,
    buildMilestoneTargetDescription,
    resolveOverlayMilestoneDescription,
    applyMilestoneOverlayTemplates,
    displayMilestoneRewardAmount,
    formatRidesCompleted,
    formatEarningsCompleted,
    formatDistanceCompleted,
    formatDurationCompleted,
    formatTicketsBooked,
  )
where

import Data.List (partition)
import qualified Data.Text as T
import Kernel.Prelude
import qualified Lib.IncentiveJourney.Domain.Action.Evaluate as Eval
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM

-- | Active windows first, then inactive (same order used by UI list + preferred pick).
orderJourneyAssignmentPairsForDisplay ::
  UTCTime ->
  [(DIJ.IncentiveJourney, DCJM.CohortJourneyMapping)] ->
  [(DIJ.IncentiveJourney, DCJM.CohortJourneyMapping)]
orderJourneyAssignmentPairsForDisplay localTime journeysWithMapping =
  let (active, inactive) = partition (\(j, m) -> Eval.isJourneyWindowActiveFor localTime j m) journeysWithMapping
   in active <> inactive

orderJourneysForDisplay ::
  UTCTime ->
  [(DIJ.IncentiveJourney, DCJM.CohortJourneyMapping)] ->
  [DIJ.IncentiveJourney]
orderJourneysForDisplay localTime = map fst . orderJourneyAssignmentPairsForDisplay localTime

selectPreferredJourney ::
  UTCTime ->
  [(DIJ.IncentiveJourney, DCJM.CohortJourneyMapping)] ->
  Maybe DIJ.IncentiveJourney
selectPreferredJourney localTime = listToMaybe . orderJourneysForDisplay localTime

milestoneCompletedOverlayKey :: Text
milestoneCompletedOverlayKey = "INCENTIVE_JOURNEY_MILESTONE_COMPLETED"

milestoneWaivedOverlayKey :: Text
milestoneWaivedOverlayKey = "INCENTIVE_JOURNEY_MILESTONE_WAIVED_OFF"

overlayTemplateText :: Text -> Text
overlayTemplateText txt = "{#" <> txt <> "#}"

formatRidesCompleted :: Int -> Maybe Text -> Text
formatRidesCompleted n mbQualifier =
  let rideWord = if n == 1 then "ride" else "rides"
   in case mbQualifier of
        Nothing -> show n <> " " <> rideWord <> " completed"
        Just qualifier -> show n <> " " <> qualifier <> " " <> rideWord <> " completed"

formatEarningsCompleted :: Int -> Text
formatEarningsCompleted n = "Rs " <> show n <> " earned"

formatDistanceCompleted :: Int -> Text
formatDistanceCompleted meters
  | meters >= 1000 && meters `mod` 1000 == 0 =
    show (meters `div` 1000) <> " km covered"
  | otherwise =
    show meters <> " m covered"

formatDurationCompleted :: Int -> Text
formatDurationCompleted seconds
  | seconds >= 3600 && seconds `mod` 3600 == 0 =
    show (seconds `div` 3600) <> " hr completed"
  | seconds >= 60 && seconds `mod` 60 == 0 =
    show (seconds `div` 60) <> " min completed"
  | otherwise =
    show seconds <> " sec completed"

formatTicketsBooked :: Int -> Text
formatTicketsBooked n =
  let ticketWord = if n == 1 then "ticket" else "tickets"
   in show n <> " " <> ticketWord <> " booked"

aggregatedDisplayConditionValue :: DIJM.IncentiveJourneyMilestone -> [DIJM.IncentiveJourneyMilestone] -> Int
aggregatedDisplayConditionValue milestone allMilestones =
  sum
    [ m.conditionValue
      | m <- allMilestones,
        m.conditionType == milestone.conditionType,
        m.order <= milestone.order
    ]

buildMilestoneTargetDescription :: DIJM.IncentiveJourneyMilestone -> Int -> Text
buildMilestoneTargetDescription milestone displayConditionValue =
  case milestone.conditionType of
    DIJM.RideCompleted -> formatRidesCompleted displayConditionValue Nothing
    DIJM.Earnings -> formatEarningsCompleted displayConditionValue
    DIJM.Distance -> formatDistanceCompleted displayConditionValue
    DIJM.RideDuration -> formatDurationCompleted displayConditionValue
    DIJM.BookingTicket -> formatTicketsBooked displayConditionValue

resolveOverlayMilestoneDescription ::
  DIJM.IncentiveJourneyMilestone ->
  [DIJM.IncentiveJourneyMilestone] ->
  Text
resolveOverlayMilestoneDescription milestone journeyMilestones =
  case milestone.name of
    Just n | not (T.null (T.strip n)) -> n
    _ ->
      case milestone.description of
        Just desc | not (T.null (T.strip desc)) -> desc
        _ ->
          let displayValue = aggregatedDisplayConditionValue milestone journeyMilestones
           in buildMilestoneTargetDescription milestone displayValue

applyMilestoneOverlayTemplates :: DIJ.IncentiveJourney -> Text -> DIJM.IncentiveJourneyMilestone -> Int -> Text -> Text
applyMilestoneOverlayTemplates journey milestoneTarget milestone displayReward =
  T.replace (overlayTemplateText "journeyName") journey.name
    . T.replace (overlayTemplateText "milestoneName") (fromMaybe milestoneTarget milestone.name)
    . T.replace (overlayTemplateText "milestoneDescription") milestoneTarget
    . T.replace (overlayTemplateText "milestoneOrder") (show milestone.order)
    . T.replace (overlayTemplateText "rewardAmount") (show displayReward)
    . T.replace (overlayTemplateText "rewardType") (show milestone.rewardType)

displayMilestoneRewardAmount :: Int -> DIJM.IncentiveJourneyMilestone -> Int
displayMilestoneRewardAmount awarded milestone =
  if awarded > 0 then awarded else fromMaybe 0 milestone.rewardValue
