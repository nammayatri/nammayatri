{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Engineering.Helpers.Utils where

import Prelude
import Common.Types.App (CalendarModalDateObject, CalendarModalWeekObject, GlobalPayload(..), MobileNumberValidatorResp(..), ModifiedCalendarObject, Payload(..))
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (lift)
import Data.Either (Either(..), hush)
import Data.Function.Uncurried (Fn2, runFn2, Fn3, Fn1)
import Data.Maybe (Maybe(..), maybe)
import Data.String (length, trim, toLower)
import Data.Maybe (Maybe(..))
import Effect.Uncurried (EffectFn2(..), runEffectFn2, EffectFn1(..), runEffectFn1)
import Data.String (length, trim, Pattern(..), split, toUpper, toLower, joinWith, take, drop)
import Data.String.CodeUnits (charAt)
import Data.Foldable (foldl)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.Commons (flowRunner, liftFlow)
import Foreign (unsafeToForeign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (Foreign, decode, decodeJSON, encodeJSON)
import Halogen.VDom.DOM.Prop (PropValue)
import LoaderOverlay.Handler as UI
import Log (printLog)
import MerchantConfig.DefaultConfig as DefaultConfig
import MerchantConfig.Types (AppConfig)
import Presto.Core.Types.Language.Flow (Flow, doAff, getState, modifyState, delay)
import PrestoDOM.Core (terminateUI)
import Types.App (FlowBT, GlobalState(..))
import Unsafe.Coerce (unsafeCoerce)
import Data.Array (find)
import Data.Tuple (Tuple(..), fst, snd)

-- Common Utils
foreign import reboot :: Effect Unit

foreign import showSplash :: Effect Unit

ifelse :: forall a. Boolean -> a -> a -> a
ifelse p a b = if p then a else b

infixl 1 ifelse as ?

fromProp :: PropValue -> String
fromProp = unsafeCoerce

-- Loader Utils
toggleLoader :: Boolean -> Flow GlobalState Unit
toggleLoader =
  if _ then do
    state <- getState
    _ <- liftFlow $ launchAff $ flowRunner state UI.loaderScreen
    pure unit
  else do
    doAff $ liftEffect $ terminateUI $ Just "LoaderOverlay"

loaderText :: String -> String -> Flow GlobalState Unit
loaderText mainTxt subTxt = void $ modifyState (\(GlobalState state) -> GlobalState state { loaderOverlay { data { title = mainTxt, subTitle = subTxt } } })

showAndHideLoader :: Boolean -> String -> String -> GlobalState -> Effect Unit
showAndHideLoader showLoader title description state = do
  void $
    launchAff $ flowRunner state
      $ do
          void $ loaderText title description
          void $ toggleLoader showLoader
          pure unit
  pure unit

-- Mobile Number Validator Utils
mobileNumberValidator :: String -> String -> String -> MobileNumberValidatorResp
mobileNumberValidator _ countryShortCode mobileNumber =
  let
    len = length mobileNumber

    maxLen = mobileNumberMaxLength countryShortCode
  in
    if len <= maxLen then case countryShortCode of
      "IN" -> case (charAt 0 mobileNumber) of
        Just a ->
          if a == '0' || a == '1' || a == '2' || a == '3' || a == '4' then
            Invalid
          else if a == '5' then
            if mobileNumber == "5000500050" then Valid else Invalid
          else if len == maxLen then Valid else ValidPrefix
        Nothing -> ValidPrefix
      "FR" -> case (charAt 0 mobileNumber) of
        Just a ->
          if a == '6' || a == '7' then
            if len == maxLen then Valid else ValidPrefix
          else
            Invalid
        Nothing -> ValidPrefix
      "BD" -> case (charAt 0 mobileNumber) of
        Just a ->
          if a == '1' then
            if len == maxLen then Valid else ValidPrefix
          else
            Invalid
        Nothing -> ValidPrefix
      _ -> Invalid
    else
      MaxLengthExceeded

mobileNumberMaxLength :: String -> Int
mobileNumberMaxLength countryShortCode = case countryShortCode of
  "IN" -> 10
  "FR" -> 9
  "BD" -> 10
  _ -> 0

-- Local Storage Utils
foreign import saveToLocalStoreImpl :: String -> String -> EffectFnAff Unit

foreign import fetchFromLocalStoreImpl :: String -> (String -> Maybe String) -> Maybe String -> Effect (Maybe String)

saveToLocalStore' :: String -> String -> EffectFnAff Unit
saveToLocalStore' = saveToLocalStoreImpl

class Serializable a where
  serialize :: a -> String
  deserialize :: String -> Maybe a

instance genericSerializable :: (Encode a, Decode a) => Serializable a where
  serialize = encodeJSON
  deserialize = decodeJSON >>> runExcept >>> hush

saveObject :: forall s. Serializable s => String -> s -> Flow GlobalState Unit
saveObject objName obj =
  doAff do
    (fromEffectFnAff <<< saveToLocalStore' objName $ (serialize obj))

-- Calendar Utils
foreign import getWeeksInMonth :: Int -> Int -> Array CalendarModalWeekObject

initializeCalendar :: Boolean -> ModifiedCalendarObject
initializeCalendar selectTodaysDate =
  let currentDay = getCurrentDay true
      weeks = getWeeksInMonth currentDay.year currentDay.intMonth
  in if selectTodaysDate 
       then selectSingleCalendarDate currentDay Nothing Nothing weeks
       else 
         { selectedTimeSpan : currentDay, 
           weeks : weeks, 
           startDate : Nothing,
           endDate : Nothing 
         }

foreign import getCurrentDay :: Boolean -> CalendarModalDateObject

foreign import decrementMonth :: Int -> Int -> CalendarModalDateObject

foreign import incrementMonth :: Int -> Int -> CalendarModalDateObject

decrementCalendarMonth :: CalendarModalDateObject -> Maybe CalendarModalDateObject -> Maybe CalendarModalDateObject -> ModifiedCalendarObject
decrementCalendarMonth selectedTimeSpan startDate endDate = do
  let
    decrementedMonthDate = decrementMonth selectedTimeSpan.intMonth selectedTimeSpan.year

    weeks = getWeeksInMonth decrementedMonthDate.year decrementedMonthDate.intMonth

    modifiedWeeks = getPreviousState startDate endDate weeks
  { selectedTimeSpan: decrementedMonthDate
  , weeks: modifiedWeeks
  , startDate: startDate
  , endDate: endDate
  }

incrementCalendarMonth :: CalendarModalDateObject -> Maybe CalendarModalDateObject -> Maybe CalendarModalDateObject -> ModifiedCalendarObject
incrementCalendarMonth selectedTimeSpan startDate endDate = do
  let
    incrementedMonthDate = incrementMonth selectedTimeSpan.intMonth selectedTimeSpan.year

    weeks = getWeeksInMonth incrementedMonthDate.year incrementedMonthDate.intMonth

    modifiedWeeks = getPreviousState startDate endDate weeks
  { selectedTimeSpan: incrementedMonthDate
  , weeks: modifiedWeeks
  , startDate: startDate
  , endDate: endDate
  }

updateWeeks :: CalendarModalDateObject -> Boolean -> CalendarModalDateObject -> CalendarModalWeekObject -> CalendarModalWeekObject
updateWeeks selDate isStart startDate week =
  if selDate == startDate then
    week
  else if isStart then
    let
      modifiedData = map (\date -> if date.date == selDate.date && date.intMonth == selDate.intMonth then date { isStart = true } else date) week.week
    in
      { week: modifiedData }
  else
    let
      modifiedData =
        map
          ( \day ->
              if day.utcDate == "" then
                day
              else if day.utcDate == startDate.utcDate then
                day { isStart = true, isInRange = false }
              else if day.utcDate == selDate.utcDate then
                day { isEnd = true, isInRange = false }
              else if day.utcDate > startDate.utcDate && day.utcDate < selDate.utcDate then
                day { isInRange = true, isStart = false, isEnd = false }
              else
                day
          )
          week.week
    in
      { week: modifiedData }

revertWeeks :: CalendarModalWeekObject -> CalendarModalWeekObject
revertWeeks week =
  let
    modifiedData = map (\date -> date { isStart = false, isEnd = false, isInRange = false }) week.week
  in
    { week: modifiedData }

getPreviousState :: Maybe CalendarModalDateObject -> Maybe CalendarModalDateObject -> Array CalendarModalWeekObject -> Array CalendarModalWeekObject
getPreviousState mbStartDate mbEndDate weeks = do
  case mbStartDate of
    Nothing -> weeks
    Just startDate -> do
      case mbEndDate of
        Nothing -> map (updateWeeks startDate true dummyDateItem) weeks
        Just endDate -> do
          map (updateWeeks endDate false startDate) weeks

dummyDateItem ∷ CalendarModalDateObject
dummyDateItem = { date: 0, isInRange: false, isStart: false, isEnd: false, utcDate: "", shortMonth: "", year: 0, intMonth: 0 }

selectRangeCalendarDate :: CalendarModalDateObject -> Maybe CalendarModalDateObject -> Maybe CalendarModalDateObject -> Array CalendarModalWeekObject -> ModifiedCalendarObject
selectRangeCalendarDate date mbStartDate mbEndDate weeks = do
  if date.date == 0 then
    { startDate: mbStartDate, endDate: mbEndDate, weeks: weeks, selectedTimeSpan: date }
  else do
    case mbStartDate of
      Nothing -> do
        let
          modifiedDates = map (updateWeeks date true dummyDateItem) weeks
        { startDate: Just date, endDate: Nothing, weeks: modifiedDates, selectedTimeSpan: date }
      Just startDate -> do
        case mbEndDate of
          Nothing -> do
            if date.date == startDate.date then
              { startDate: mbStartDate, endDate: mbEndDate, weeks: weeks, selectedTimeSpan: date }
            else do
              let
                modStartDate = if ((startDate.utcDate > date.utcDate && startDate.intMonth == date.intMonth && startDate.year == date.year) || (startDate.intMonth > date.intMonth && startDate.year == date.year) || startDate.year > date.year) then date else startDate

                modEndDate = if ((startDate.utcDate > date.utcDate && startDate.intMonth == date.intMonth && startDate.year == date.year) || (startDate.intMonth > date.intMonth && startDate.year == date.year) || startDate.year > date.year) then startDate else date

                modifiedDates = map (updateWeeks modEndDate false modStartDate) weeks
              { startDate: Just modStartDate, endDate: Just modEndDate, weeks: modifiedDates, selectedTimeSpan: modStartDate }
          Just _ -> do
            let
              revertDates = map (revertWeeks) weeks

              modifiedDates = map (updateWeeks date true dummyDateItem) revertDates
            { startDate: Just date, endDate: Nothing, weeks: modifiedDates, selectedTimeSpan: date }

selectSingleCalendarDate :: CalendarModalDateObject -> Maybe CalendarModalDateObject -> Maybe CalendarModalDateObject -> Array CalendarModalWeekObject -> ModifiedCalendarObject
selectSingleCalendarDate date mbStartDate mbEndDate weeks = do
  if date.date == 0 then
    { startDate: mbStartDate, endDate: mbEndDate, weeks: weeks, selectedTimeSpan: date }
  else do
    case mbStartDate of
      _ -> do
        let modifiedDates = map (updateWeeks date true dummyDateItem) weeks
        { startDate : Just date, endDate : Nothing, weeks : modifiedDates, selectedTimeSpan : date }

cityCodeMap :: Array (Tuple String String)
cityCodeMap = 
  [ Tuple "std:080" "bangalore"
  , Tuple "std:033" "kolkata"
  , Tuple "std:001" "paris"
  , Tuple "std:484" "kochi"
  , Tuple "std:011" "delhi"
  , Tuple "std:040" "hyderabad"
  , Tuple "std:022" "mumbai"
  , Tuple "std:044" "chennai"
  , Tuple "std:0422" "coimbatore"
  , Tuple "std:0413" "pondicherry"
  , Tuple "std:08342" "goa"
  , Tuple "std:020" "pune"
  , Tuple "std:0821" "mysore"
  , Tuple "std:0816" "tumakuru"
  , Tuple "std:01189" "noida"
  , Tuple "std:0124" "gurugram"
  ]

getCityFromCode :: String -> String
getCityFromCode code = 
  let 
    cityCodeTuple = find (\ tuple -> (fst tuple) == code) cityCodeMap
  in maybe "" (\tuple -> snd tuple) cityCodeTuple

getCodeFromCity :: String -> String
getCodeFromCity city = 
  let 
    cityCodeTuple = find (\tuple -> (snd tuple) == (toLower city)) cityCodeMap
  in maybe "" (\tuple -> fst tuple) cityCodeTuple
  
capitalizeFirstChar :: String -> String
capitalizeFirstChar inputStr =
  let splitedArray = split (Pattern " ") (inputStr)
      output = map (\item -> (toUpper (take 1 item)) <> (toLower (drop 1 item))) splitedArray
    in joinWith " " output

fetchLanguage :: String -> String
fetchLanguage currLang = case currLang of
                  "HI_IN" -> "hi"
                  "KN_IN" -> "kn"
                  "TA_IN" -> "ta"
                  _       -> "en"
                  
