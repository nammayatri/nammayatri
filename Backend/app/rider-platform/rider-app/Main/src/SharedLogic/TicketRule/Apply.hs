{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module SharedLogic.TicketRule.Apply where

import qualified API.Types.Dashboard.AppManagement.TicketDashboard as TD
import Data.Time
  ( Day,
    DayOfWeek (..),
    dayOfWeek,
    diffDays,
    localDay,
    localTimeOfDay,
    minutesToTimeZone,
    utcToLocalTime,
  )
import qualified Data.Time as Time
import Domain.Types.ServiceCategory (ServiceCategory (..))
import Domain.Types.ServicePeopleCategory (ServicePeopleCategory (..))
import Domain.Types.TicketPlace (PlaceStatus (..), TicketPlace (..))
import Domain.Types.TicketService (TicketService (..))
import qualified Environment
import GHC.Generics
import Kernel.Prelude
import Kernel.Types.Id
import qualified Kernel.Types.Price as Price
import Kernel.Utils.Time
import SharedLogic.TicketRule.Core
import qualified SharedLogic.TicketRule.Core as Core
import SharedLogic.TicketUtils as TU

data CurrentContext = CurrentContext
  { currentTime :: UTCTime,
    timeOfDay :: TimeOfDay,
    currentDate :: Day,
    daysBeforeBooking :: Maybe Int,
    maxCapacity :: Maybe Int,
    bookedCount :: Maybe Int,
    remainingCapacity :: Maybe Int,
    isWeekday :: Bool
  }
  deriving (Show, Eq, Generic)

createCurrentContext :: UTCTime -> Int -> Maybe Day -> (Maybe Int, Maybe Int) -> CurrentContext
createCurrentContext utcTime tzMinutes mbVisitDate (mbMaxCapacity, mbBookedCount) =
  let tz = minutesToTimeZone tzMinutes
      localTime = utcToLocalTime tz utcTime
      today = localDay localTime
      currentTimeOfDay = localTimeOfDay localTime
      todayWeekday = dayOfWeek today
      daysBeforeBooking = case mbVisitDate of
        Just visitDate -> Just $ fromIntegral $ diffDays visitDate today
        Nothing -> Nothing
      isWeekdayVal = todayWeekday `elem` [Time.Monday, Time.Tuesday, Time.Wednesday, Time.Thursday, Time.Friday]
   in CurrentContext
        { currentTime = utcTime,
          timeOfDay = currentTimeOfDay,
          currentDate = today,
          daysBeforeBooking = daysBeforeBooking,
          isWeekday = isWeekdayVal,
          maxCapacity = mbMaxCapacity,
          bookedCount = mbBookedCount,
          remainingCapacity = (-) <$> mbMaxCapacity <*> mbBookedCount
        }

getCurrentContext :: Int -> Maybe Day -> Maybe (Id ServiceCategory) -> Environment.Flow CurrentContext
getCurrentContext tzMinutes mbVisitDate mbCategoryId = do
  now <- getCurrentTime
  mbCurrentSeatStatus <- case (mbVisitDate, mbCategoryId) of
    (Just visitDate, Just categoryId) -> do
      res <- TU.seatStatus (TD.CurrentSeatStatusReq {date = visitDate, serviceCategory = categoryId.getId})
      pure $ (res.maxCapacity, Just res.bookedCount)
    _ -> pure (Nothing, Nothing)
  pure $ createCurrentContext now tzMinutes mbVisitDate mbCurrentSeatStatus

class HasRules a where
  getRules :: a -> [Rule]

class HasRules a => Applicable a where
  applyRule :: CurrentContext -> Rule -> a -> a

  applyRules :: CurrentContext -> [Rule] -> a -> a
  applyRules ctx rules entity = foldl' (flip (applyRule ctx)) entity rules

  applyAction :: ActionType -> a -> a

checkCondition :: CurrentContext -> Condition -> Bool
checkCondition ctx condition =
  case condition of
    And conditions -> all (checkCondition ctx) conditions
    Or conditions -> any (checkCondition ctx) conditions
    Not c -> not $ checkCondition ctx c
    TimeOfDayRange ranges -> any (inTimeRange (timeOfDay ctx)) ranges
    Weekday weekdays ->
      let todayWeekday = dayOfWeek ctx.currentDate
          weekdaySet = toEnum' <$> weekdays
       in todayWeekday `elem` weekdaySet
    DateRange ranges -> any (inDateRange (currentDate ctx)) ranges
    Dates dates -> currentDate ctx `elem` dates
    DaysBeforeBooking comp count -> case ctx.daysBeforeBooking of
      Just days -> compareValues comp days count
      Nothing -> False
    RemainingCapacity comp capacity -> case ctx.remainingCapacity of
      Just remaining -> compareValues comp remaining capacity
      Nothing -> False
    BookedCount comp count -> case ctx.bookedCount of
      Just booked -> compareValues comp booked count
      Nothing -> False
  where
    inTimeRange :: TimeOfDay -> (TimeOfDay, TimeOfDay) -> Bool
    inTimeRange t (start, end) = t >= start && t <= end

    inDateRange :: Day -> (Day, Day) -> Bool
    inDateRange d (start, end) = d >= start && d <= end

    compareValues :: Comparator -> Int -> Int -> Bool
    compareValues comp val1 val2 = case comp of
      LessThan -> val1 < val2
      LessThanOrEqual -> val1 <= val2
      Equal -> val1 == val2
      GreaterThanOrEqual -> val1 >= val2
      GreaterThan -> val1 > val2

applyRuleGeneric :: Applicable a => CurrentContext -> Rule -> a -> a
applyRuleGeneric ctx rule entity =
  if checkCondition ctx rule.condition
    then applyAction rule.action.actionType entity
    else entity

processEntity :: Applicable a => CurrentContext -> a -> a
processEntity ctx entity = applyRules ctx (getRules entity) entity

instance HasRules TicketPlace where
  getRules place = fromMaybe [] $ place.rules

instance Applicable TicketPlace where
  applyRule = applyRuleGeneric
  applyAction actionType place = case actionType of
    Open -> place {status = Domain.Types.TicketPlace.Active}
    Closed -> place {status = Domain.Types.TicketPlace.Inactive}
    PricingIncreaseBy _ -> place
    PriceDecreaseBy _ -> place
    PriceSet _ -> place
    ChangeDescription description -> place {description = Just description}
    _ -> place

instance HasRules TicketService where
  getRules service = fromMaybe [] $ service.rules

instance Applicable TicketService where
  applyRule = applyRuleGeneric
  applyAction actionType service = case actionType of
    Open -> service {isClosed = False}
    Closed -> service {isClosed = True}
    _ -> service

instance HasRules ServiceCategory where
  getRules category = fromMaybe [] $ category.rules

instance Applicable ServiceCategory where
  applyRule = applyRuleGeneric
  applyAction = applyActionToCategory

instance HasRules ServicePeopleCategory where
  getRules category = fromMaybe [] $ category.rules

instance Applicable ServicePeopleCategory where
  applyRule = applyRuleGeneric
  applyAction = applyActionToPeopleCategory

applyActionToCategory :: ActionType -> ServiceCategory -> ServiceCategory
applyActionToCategory actionType category = case actionType of
  Open -> category {isClosed = False}
  Closed -> category {isClosed = True}
  OverrideBusinessHours _ -> category {remainingActions = pure $ (fromMaybe [] category.remainingActions) ++ [actionType]}
  _ -> category

applyActionToPeopleCategory :: ActionType -> ServicePeopleCategory -> ServicePeopleCategory
applyActionToPeopleCategory actionType category = case actionType of
  Open -> category {isClosed = False}
  Closed -> category {isClosed = True}
  PricingIncreaseBy factor ->
    category {pricePerUnit = category.pricePerUnit {Price.amount = category.pricePerUnit.amount + factor}}
  PriceDecreaseBy factor ->
    category {pricePerUnit = category.pricePerUnit {Price.amount = category.pricePerUnit.amount - factor}}
  PriceSet setAmount ->
    category {pricePerUnit = category.pricePerUnit {Price.amount = setAmount}}
  _ -> category

toEnum' :: Weekday -> DayOfWeek
toEnum' = \case
  Core.Monday -> Time.Monday
  Core.Tuesday -> Time.Tuesday
  Core.Wednesday -> Time.Wednesday
  Core.Thursday -> Time.Thursday
  Core.Friday -> Time.Friday
  Core.Saturday -> Time.Saturday
  Core.Sunday -> Time.Sunday
