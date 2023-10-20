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

import Common.Types.App (MobileNumberValidatorResp(..), CalendarModalWeekObject(..),CalendarModalDateObject(..), ModifiedCalendarObject(..))
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Engineering.Helpers.Commons (flowRunner, liftFlow, os)
import Effect (Effect (..))
import Foreign.Class (class Decode, class Encode, encode)
import Foreign.Generic (decode, encode, Foreign, decodeJSON, encodeJSON)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.String.CodeUnits (charAt)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, runEffectFn1, runEffectFn2)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Foreign (Foreign, unsafeToForeign)
import Helpers.FileProvider.Utils (loadInWindow, mergeObjects)
import LoaderOverlay.Handler as UI
import Log (printLog)
import MerchantConfig.DefaultConfig as DefaultConfig
import MerchantConfig.Types (AppConfig)
import Presto.Core.Types.Language.Flow (Flow, doAff, getState, modifyState, delay)
import PrestoDOM.Core (terminateUI)
import Types.App (FlowBT, GlobalState(..))


foreign import toggleLoaderIOS :: EffectFn1 Boolean Unit

foreign import loaderTextIOS :: EffectFn2 String String Unit

foreign import getFromWindow :: EffectFn1 String Foreign

foreign import saveToLocalStoreImpl :: String -> String -> EffectFnAff Unit

foreign import fetchFromLocalStoreImpl :: String -> (String -> Maybe String) -> Maybe String -> Effect (Maybe String)

foreign import getWeeksInMonth :: Int -> Int -> Array CalendarModalWeekObject

foreign import getCurrentDay :: String -> CalendarModalDateObject

foreign import decrementMonth :: Int -> Int -> CalendarModalDateObject

foreign import incrementMonth :: Int -> Int -> CalendarModalDateObject
foreign import getDayBeforeOrAfter :: String -> Int -> Boolean -> CalendarModalDateObject

saveToLocalStore' :: String -> String -> EffectFnAff Unit
saveToLocalStore' = saveToLocalStoreImpl
foreign import reboot :: Effect Unit
foreign import showSplash :: Effect Unit

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

getSeparatorFactor :: Int
getSeparatorFactor = 8

defaultSeparatorCount :: Int
defaultSeparatorCount = 4

showAndHideLoader :: Number -> String -> String -> GlobalState -> Effect Unit
showAndHideLoader delayInMs title description state = do
  _ <-
    launchAff $ flowRunner state
      $ do
          _ <- loaderText title description
          _ <- toggleLoader true
          _ <- delay $ Milliseconds delayInMs
          _ <- toggleLoader false
          pure unit
  pure unit

mobileNumberValidator :: String -> String -> String -> MobileNumberValidatorResp 
mobileNumberValidator country countryShortCode mobileNumber = 
  let len = length mobileNumber
      maxLen = mobileNumberMaxLength countryShortCode
  in if len <=  maxLen then 
      case countryShortCode of 
        "IN" -> case (charAt 0 mobileNumber) of
                  Just a -> if a=='0' || a=='1' || a=='2' || a=='3' || a=='4' then Invalid
                            else if a=='5' then if mobileNumber=="5000500050" then Valid else Invalid
                                 else if len == maxLen then Valid else ValidPrefix 
                  Nothing -> ValidPrefix 
        "FR" -> case (charAt 0 mobileNumber) of 
                  Just a -> if a == '6' || a == '7' then if len == maxLen then Valid else ValidPrefix
                            else Invalid 
                  Nothing -> ValidPrefix
        "BD" -> case (charAt 0 mobileNumber) of 
                  Just a -> if a == '1' then if len == maxLen then Valid else ValidPrefix 
                            else Invalid
                  Nothing -> ValidPrefix
        _ -> Invalid
      else MaxLengthExceeded

mobileNumberMaxLength :: String -> Int
mobileNumberMaxLength countryShortCode = case countryShortCode of
  "IN" -> 10
  "FR" -> 9
  "BD" -> 10
  _ -> 0

getAppConfig :: String -> FlowBT String AppConfig
getAppConfig = liftFlowBT <<< runEffectFn1 getAppConfigImpl

getAppConfigImpl :: EffectFn1 String AppConfig
getAppConfigImpl =
  mkEffectFn1 \key -> do
    config <- runEffectFn1 getFromWindow key
    case runExcept (decode config) of
      Right obj -> pure obj
      Left err1 -> do
        _ <- pure $ printLog "Not able to decode config" $ "Fallbacks to default config for missing Keys" <> (show err1)
        mergedObjects <- runEffectFn1 mergeObjects [(unsafeToForeign DefaultConfig.config),config]
        case runExcept (decode mergedObjects) of
          Right obj -> do
            runEffectFn2 loadInWindow key mergedObjects
            pure obj
          Left err -> do
            _ <- pure $ printLog "Not able to decode config not able to  find in default config" (show err)
            pure $ DefaultConfig.config

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


decrementCalendarMonth :: CalendarModalDateObject ->  Maybe CalendarModalDateObject ->  Maybe CalendarModalDateObject -> ModifiedCalendarObject
decrementCalendarMonth selectedTimeSpan startDate endDate  = do
  let decrementedMonthDate = decrementMonth selectedTimeSpan.intMonth selectedTimeSpan.year
      weeks = getWeeksInMonth decrementedMonthDate.year decrementedMonthDate.intMonth
      modifiedWeeks = getPreviousState startDate endDate weeks
  { selectedTimeSpan : decrementedMonthDate,
    weeks : modifiedWeeks,
    startDate : startDate,
    endDate : endDate
  }

incrementCalendarMonth :: CalendarModalDateObject ->  Maybe CalendarModalDateObject ->  Maybe CalendarModalDateObject -> ModifiedCalendarObject
incrementCalendarMonth selectedTimeSpan startDate endDate  = do
  let incrementedMonthDate = incrementMonth selectedTimeSpan.intMonth selectedTimeSpan.year
      weeks = getWeeksInMonth incrementedMonthDate.year incrementedMonthDate.intMonth
      modifiedWeeks = getPreviousState startDate endDate weeks
  { selectedTimeSpan : incrementedMonthDate,
    weeks : modifiedWeeks,
    startDate : startDate,
    endDate : endDate
  }


updateWeeks :: CalendarModalDateObject -> Boolean -> CalendarModalDateObject -> CalendarModalWeekObject ->  CalendarModalWeekObject
updateWeeks selDate isStart startDate week =
  if selDate == startDate then week
  else if isStart then 
    let modifiedData = map (\date -> if date.date == selDate.date && date.intMonth == selDate.intMonth then date {isStart = true} else date) week.week
    in {week : modifiedData}
  else
    let modifiedData = map (\day -> if day.utcDate == "" then day
                                    else if day.utcDate == startDate.utcDate then day {isStart = true, isInRange = false}
                                    else if day.utcDate == selDate.utcDate then day {isEnd = true, isInRange = false}
                                    else if day.utcDate > startDate.utcDate && day.utcDate < selDate.utcDate then day {isInRange = true, isStart = false, isEnd = false}
                                    else day) week.week
    in {week : modifiedData}

revertWeeks :: CalendarModalWeekObject ->  CalendarModalWeekObject
revertWeeks week =
    let modifiedData = map (\date -> date {isStart = false, isEnd = false, isInRange = false}) week.week
    in {week : modifiedData}

getPreviousState :: Maybe CalendarModalDateObject -> Maybe CalendarModalDateObject -> Array CalendarModalWeekObject -> Array CalendarModalWeekObject
getPreviousState mbStartDate mbEndDate weeks = do
  case mbStartDate of
    Nothing -> weeks
    Just startDate -> do
      case mbEndDate of 
        Nothing -> map (updateWeeks startDate true dummyDateItem) weeks
        Just endDate -> do
          map (updateWeeks endDate false startDate) weeks

dummyDateItem = {date : 0, isInRange : false, isStart : false, isEnd : false, utcDate : "", shortMonth : "", year : 0, intMonth : 0}


selectRangeCalendarDate :: CalendarModalDateObject -> Maybe CalendarModalDateObject -> Maybe CalendarModalDateObject -> Array CalendarModalWeekObject -> ModifiedCalendarObject
selectRangeCalendarDate date mbStartDate mbEndDate weeks = do
  if date.date == 0 then { startDate : mbStartDate, endDate : mbEndDate, weeks : weeks, selectedTimeSpan : date }
  else do
    case mbStartDate of
      Nothing -> do
        let modifiedDates = map (updateWeeks date true dummyDateItem) weeks
        { startDate : Just date, endDate : Nothing, weeks : modifiedDates, selectedTimeSpan : date }
      Just startDate -> do
          case mbEndDate of
              Nothing -> do
                  if date.date == startDate.date then { startDate : mbStartDate, endDate : mbEndDate, weeks : weeks, selectedTimeSpan : date }
                  else do
                    let modStartDate = if ((startDate.utcDate > date.utcDate && startDate.intMonth == date.intMonth && startDate.year == date.year) || (startDate.intMonth  > date.intMonth && startDate.year == date.year) || startDate.year > date.year) then date else startDate
                        modEndDate = if ((startDate.utcDate > date.utcDate && startDate.intMonth == date.intMonth && startDate.year == date.year) || (startDate.intMonth  > date.intMonth && startDate.year == date.year) || startDate.year > date.year) then startDate else date
                        modifiedDates = map (updateWeeks modEndDate false modStartDate) weeks
                    { startDate : Just modStartDate, endDate : Just modEndDate, weeks : modifiedDates, selectedTimeSpan : modStartDate}
              Just _ -> do
                let revertDates = map (revertWeeks) weeks
                    modifiedDates = map (updateWeeks date true dummyDateItem) revertDates
                { startDate : Just date, endDate : Nothing, weeks : modifiedDates, selectedTimeSpan : date }

selectSingleCalendarDate :: CalendarModalDateObject -> Maybe CalendarModalDateObject -> Maybe CalendarModalDateObject -> Array CalendarModalWeekObject -> ModifiedCalendarObject
selectSingleCalendarDate date mbStartDate mbEndDate weeks = do
  if date.date == 0 then { startDate : mbStartDate, endDate : mbEndDate, weeks : weeks, selectedTimeSpan : date }
  else do
    case mbStartDate of 
      _ -> do
        let modifiedDates = map (updateWeeks date true dummyDateItem) weeks
        { startDate : Just date, endDate : Nothing, weeks : modifiedDates, selectedTimeSpan : date }

ifelse :: forall a. Boolean -> a -> a -> a
ifelse p a b = if p then a else b

infixl 1 ifelse as ?