{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Extra.TimeBound
  ( BoundedPeaks (..),
    TimeBound (..),
    findBoundedDomain,
  )
where

import Control.Applicative ((<|>))
import Data.Time
import Data.Time.Calendar.WeekDate
import Kernel.Prelude
import Kernel.Utils.Common
import Kernel.Utils.GenericPretty
import qualified Text.Show
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data BoundedPeaks = BoundedPeaks
  { monday :: [(TimeOfDay, TimeOfDay)],
    tuesday :: [(TimeOfDay, TimeOfDay)],
    wednesday :: [(TimeOfDay, TimeOfDay)],
    thursday :: [(TimeOfDay, TimeOfDay)],
    friday :: [(TimeOfDay, TimeOfDay)],
    saturday :: [(TimeOfDay, TimeOfDay)],
    sunday :: [(TimeOfDay, TimeOfDay)]
  }
  deriving (Eq, Ord, Generic, Show, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving (PrettyShow) via Showable BoundedPeaks

data TimeBound
  = BoundedByWeekday BoundedPeaks
  | BoundedByDay [(Day, [(TimeOfDay, TimeOfDay)])]
  | Unbounded
  deriving (Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving (PrettyShow) via Showable TimeBound

instance Show TimeBound where
  show Unbounded = "Unbounded"
  show (BoundedByWeekday peaks) = show peaks
  show (BoundedByDay days) = show days

instance Read TimeBound where
  readsPrec _ str
    | str == "Unbounded" = [(Unbounded, "")]
    | otherwise =
      case (readMaybe str :: Maybe BoundedPeaks) of
        Just bound -> [(BoundedByWeekday bound, "")]
        Nothing ->
          case (readMaybe str :: Maybe [(Day, [(TimeOfDay, TimeOfDay)])]) of
            Just bound -> [(BoundedByDay bound, "")]
            Nothing -> [(Unbounded, "")]

$(mkBeamInstancesForEnum ''TimeBound)

findBoundedDomain :: (HasField "timeBounds" domain TimeBound) => [domain] -> UTCTime -> Maybe domain
findBoundedDomain domains localTime = do
  let currTimeOfDay = utcTimeToDiffTime localTime
      currentDay = utctDay localTime
      (_, _, currentDayOfWeek) = toWeekDate currentDay
  let (domainBoundedByWeekday, domainBoundedByDay) =
        foldl
          ( \acc@(domainBoundedByWeekday_, domainBoundedByDay_) domain ->
              case domain.timeBounds of
                BoundedByWeekday timeBounds ->
                  if isWithin currTimeOfDay (getPeaksForCurrentDay currentDayOfWeek timeBounds)
                    then (Just domain, domainBoundedByDay_)
                    else acc
                BoundedByDay days ->
                  if maybe False (isWithin currTimeOfDay) (snd <$> find (\(day, _) -> day == currentDay) days)
                    then (domainBoundedByWeekday_, Just domain)
                    else acc
                Unbounded -> acc
          )
          (Nothing, Nothing)
          domains
  domainBoundedByWeekday <|> domainBoundedByDay
  where
    isWithin _ [] = False
    isWithin currTime [(startTime, endTime)] = currTime > timeOfDayToTime startTime && currTime < timeOfDayToTime endTime
    isWithin currTime ((startTime, endTime) : xs) = (currTime > timeOfDayToTime startTime && currTime < timeOfDayToTime endTime) || isWithin currTime xs

    getPeaksForCurrentDay currentDayOfWeek peaks =
      case currentDayOfWeek of
        1 -> peaks.monday
        2 -> peaks.tuesday
        3 -> peaks.wednesday
        4 -> peaks.thursday
        5 -> peaks.friday
        6 -> peaks.saturday
        7 -> peaks.sunday
        _ -> peaks.monday -- This case should never come.
