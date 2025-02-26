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
import Common.Types.App (CalendarModalDateObject, CalendarModalWeekObject, GlobalPayload(..), MobileNumberValidatorResp(..), ModifiedCalendarObject, Payload(..), LazyCheck(..), City(..))
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (lift)
import Data.Either (Either(..), hush)
import Data.Function.Uncurried (Fn2, runFn2, Fn3, Fn1)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Maybe (Maybe(..))
import Effect.Uncurried (EffectFn2(..), runEffectFn2, EffectFn1(..), runEffectFn1)
import Data.String (length, trim, Pattern(..), split, toUpper, toLower, joinWith, take, drop, replaceAll, Replacement(..), replace)
import Data.String.CodeUnits (charAt)
import Data.Foldable (foldl)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.Commons (flowRunner, liftFlow, os)
import Foreign (unsafeToForeign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (Foreign, decode, decodeJSON, encodeJSON)
import Halogen.VDom.DOM.Prop (PropValue)
import LoaderOverlay.Handler as UI
import Toast.Handler as UI
import Log (printLog)
import MerchantConfig.DefaultConfig as DefaultConfig
import MerchantConfig.Types (AppConfig)
import Presto.Core.Types.Language.Flow (Flow, doAff, getState, modifyState, delay)
import PrestoDOM.Core (terminateUI)
import Types.App (FlowBT, GlobalState(..))
import Unsafe.Coerce (unsafeCoerce)
import Data.Array (elem, slice, cons, uncons)
import Data.Array as DA
import Data.Tuple (Tuple(..), fst, snd)
import ConfigProvider
import Storage (getValueToLocalStore, setValueToLocalStore, KeyStore(..))
import Data.Number (fromString)
import JBridge (toast, setKeyInSharedPref, getResourceIdentifier)
import Language.Strings (getString)
import Language.Types
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Data.Int as DI
import Data.Number.Format (fixed, toStringWith)
import DecodeUtil

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
    doAff $ liftEffect $ terminateLoader ""
showToast :: String -> Flow GlobalState Unit
showToast message = do
  void $ modifyState (\(GlobalState state) -> GlobalState state { toast { data { title = "", message = message } } })
  doAff $ liftEffect $ terminateToast ""
  state <- getState
  _ <- liftFlow $ launchAff $ flowRunner state UI.toastScreen
  pure unit

terminateToast :: String -> Effect Unit
terminateToast _ = terminateUI $ Just "Toast"

terminateLoader :: String -> Effect Unit
terminateLoader _ = terminateUI $ Just "LoaderOverlay"

loaderText :: String -> String -> Flow GlobalState Unit
loaderText mainTxt subTxt = void $ modifyState (\(GlobalState state) -> GlobalState state { loaderOverlay { data { title = mainTxt, subTitle = subTxt } } })

toastText :: String -> String -> Flow GlobalState Unit
toastText title message = void $ modifyState (\(GlobalState state) -> GlobalState state { toast { data { title = title, message = message } } })

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
      "US" -> if len == maxLen then Valid else ValidPrefix
      _ -> if len == maxLen then Valid else ValidPrefix
    else
      MaxLengthExceeded

mobileNumberMaxLength :: String -> Int
mobileNumberMaxLength countryShortCode = case countryShortCode of
  "IN" -> 10
  "FR" -> 9
  "BD" -> 10
  "US" -> 10
  _ -> 10

-- Local Storage Utils
foreign import saveToLocalStoreImpl :: String -> String -> EffectFnAff Unit

foreign import compareDate :: EffectFn2 String String Boolean

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
  , Tuple "std:0484" "kochi"
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
  , Tuple "std:0141" "jaipur"
  , Tuple "std:0172" "chandigarh"
  , Tuple "std:0487" "thrissur"
  , Tuple "std:0471" "trivandrum"
  , Tuple "std:0495" "kozhikode"
  , Tuple "std:0431" "trichy"
  , Tuple "std:04362" "thanjavur"
  , Tuple "std:0427" "salem"
  , Tuple "std:0462" "tirunelveli"
  , Tuple "std:04344" "hosur"
  , Tuple "std:0452" "madurai"
  , Tuple "std:0416" "vellore"
  , Tuple "std:0353" "siliguri"
  , Tuple "std:08192" "davanagere"
  , Tuple "std:08182" "shivamogga"
  , Tuple "std:0836" "hubli"
  , Tuple "std:0824" "mangalore"
  , Tuple "std:08472" "gulbarga"
  , Tuple "std:08200" "udupi"
  ]

getCityFromCode :: String -> String
getCityFromCode code = 
  let 
    cityCodeTuple = DA.find (\ tuple -> (fst tuple) == code) cityCodeMap
  in maybe "" (\tuple -> snd tuple) cityCodeTuple

getCodeFromCity :: String -> String
getCodeFromCity city = 
  let 
    cityCodeTuple = DA.find (\tuple -> (snd tuple) == (toLower city)) cityCodeMap
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
                  "BN_IN" -> "bn"
                  "TE_IN" -> "te"
                  "ML_IN" -> "ml"
                  _       -> "en"

handleUpdatedTerms :: String -> Effect Unit
handleUpdatedTerms message = do
  appConfig <- runEffectFn1 getAppConfigEff appConfig
  let termsVersion = getValueToLocalStore T_AND_C_VERSION
  if (termsVersion `elem` ["__failed", "(null)"]) then void $ pure $ runFn2 setKeyInSharedPref "T_AND_C_VERSION" "1.0" else pure unit
  let isTermsUpdated = (fromMaybe 0.0 (fromString $ getValueToLocalStore T_AND_C_VERSION)) < appConfig.termsVersion
  if isTermsUpdated then do
      void $ pure $ runFn2 setKeyInSharedPref "T_AND_C_VERSION" (show appConfig.termsVersion)
      void $ pure $ toast $ message
  else pure unit        
        
                  
getReferralCode :: String -> Maybe String
getReferralCode referralData =
  let tuples = parseKeyValues referralData "&" "="
  in findValueFromTuples tuples "utm_campaign"

parseKeyValues :: String -> String -> String -> Array (Tuple String String)
parseKeyValues string queryPattern keyValuePattern = DA.catMaybes $ map (\query -> parseKeyValueWithPattern query keyValuePattern) (split (Pattern queryPattern) string)

parseKeyValueWithPattern :: String -> String -> Maybe (Tuple String String)
parseKeyValueWithPattern string pattern = 
  let afterSplit = split (Pattern pattern) string 
  in  if DA.length afterSplit == 2 then
        Just (Tuple (fromMaybe "" (afterSplit DA.!! 0)) (fromMaybe "" (afterSplit DA.!! 1)))
      else Nothing
      
findValueFromTuples :: Array (Tuple String String) -> String -> Maybe String
findValueFromTuples tuples key = 
  let tuple = DA.find (\(Tuple a _) -> a == key) tuples
  in case tuple of
        Just (Tuple _ b) -> Just b
        Nothing          -> Nothing

splitIntoEqualParts :: forall a. Int -> Array a -> Array (Array a)
splitIntoEqualParts _ [] = []
splitIntoEqualParts n arr =
  let 
    part = slice 0 n arr
    rest = slice n (DA.length arr) arr
  in 
    cons part (splitIntoEqualParts n rest)

splitArrayByLengths :: forall a. Array a -> Array Int -> Maybe (Array (Array a))
splitArrayByLengths array lengths = split array lengths []
  where
    split :: Array a -> Array Int -> Array (Array a) -> Maybe (Array (Array a))
    split remaining [] result = Just result
    split remaining lenArray result =
      case uncons lenArray of 
         Just {head: len, tail: restLenArray} -> if len <= DA.length remaining then
                                                  let part = DA.take len remaining
                                                      rest = DA.drop len remaining
                                                  in split rest restLenArray (result <> [part])
                                                 else
                                                  Just $ result <> [remaining]
         Nothing -> Just $ result <> [remaining]

getFlexBoxCompatibleVersion :: String -> String  
getFlexBoxCompatibleVersion _ = 
  if os == "IOS" then 
    case getMerchant FunctionCall of 
      NAMMAYATRI -> "1.3.6"
      YATRISATHI -> "1.0.5"
      YATRI -> "2.1.0"
      _ -> "0.0.0"
    else do 
      case getMerchant FunctionCall of 
        NAMMAYATRI -> "1.3.10"
        YATRISATHI -> "0.1.7"
        YATRI -> "2.2.2"
        _ -> "0.0.0"

getFixedTwoDecimals :: Number -> String
getFixedTwoDecimals amount = case (DI.fromNumber amount) of
                                Just value -> show value
                                Nothing ->  toStringWith (fixed 2) amount

formatNumber :: Number -> Maybe Int -> String
formatNumber amount decimalPlaces = case (DI.fromNumber amount),decimalPlaces  of
                                      (Just value), _ -> show value
                                      Nothing, Just decimalPlace -> toStringWith (fixed decimalPlace) amount
                                      _,_ -> show amount
                                      
                                
        
formatMinIntoHoursMins :: Int -> String
formatMinIntoHoursMins mins = 
  let 
    hours = mins / 60
    minutes = mins `mod` 60
  in (if hours < 10 then "0" else "") <> show hours <> " : " <> (if minutes < 10 then "0" else "") <> show minutes <> " hr"

getColorWithOpacity :: Int -> String -> String 
getColorWithOpacity opacity color =
  let percentToHex = (if opacity `mod` 2 == 1 then DI.ceil else DI.floor) $  (255.0 * (DI.toNumber opacity)) /100.0
      hexString = getHexFromInt percentToHex
      prefixForColor = "#" <> if length hexString < 2 then "0" <> hexString else hexString
  in prefixForColor <> (replace (Pattern "#") (Replacement "") color)

getHexFromInt :: Int -> String
getHexFromInt number = if number == 0 then "0" else toUpper (convertToHex number)

convertToHex :: Int -> String
convertToHex number = do
  if number == 0 
    then ""
    else do
      let quotient = number/16
          remainder = number `mod` 16
          hexDigit = fromMaybe "" (intToHexChar remainder)
      append (convertToHex quotient) hexDigit

intToHexChar :: Int -> Maybe String
intToHexChar n =
  hexChars DA.!! n
  where
    hexChars = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f"]

checkConditionToShowInternetScreen :: forall a. a -> Boolean
checkConditionToShowInternetScreen lazy = 
  let 
    count = (getKeyWithDefaultFromWindow "noInternetCount" 0) + 1
    forced = getKeyWithDefaultFromWindow "forceAppToNoInternetScreen" false
  in 
    if count == 3 || forced then
      let _ = setKeyInWindow "noInternetCount" 0
      in true
    else 
      let _ = setKeyInWindow "noInternetCount" count
      in false

foreign import getAndRemoveLatestNotificationType :: Unit -> String

getCitySpecificMarker :: City -> String -> Maybe String -> String
getCitySpecificMarker city variant currentStage =
    -- For compatibility with older native apk versions
    let isDeliveryImagePresent = (getResourceIdentifier "ny_ic_bike_delivery_nav_on_map" "drawable") /= 0
        isHeritageCabImagePresent = (getResourceIdentifier "ny_ic_heritage_cab_nav_on_map" "drawable") /= 0 
        variantImage = case variant of
            _ | DA.elem variant ["AUTO_RICKSHAW", "EV_AUTO_RICKSHAW"] -> getAutoImage city
            "SEDAN"         -> "ny_ic_vehicle_nav_on_map"
            "SUV"           -> "ny_ic_suv_nav_on_map"
            "HATCHBACK"     -> "ny_ic_hatchback_nav_on_map"
            "BIKE"          -> if currentStage == Just "RideStarted" then "ny_ic_bike_pickup_nav_on_map" else "ny_ic_bike_nav_on_map"
            "DELIVERY_BIKE" -> if isDeliveryImagePresent then "ny_ic_bike_delivery_nav_on_map" else "ny_ic_bike_nav_on_map"
            "SUV_PLUS"      -> "ny_ic_suv_plus_nav_on_map"
            _ | isAmbulance variant -> "ny_ic_ambulance_nav_on_map"
            "HERITAGE_CAB"  -> if isHeritageCabImagePresent then "ny_ic_heritage_cab_nav_on_map" else "ny_ic_vehicle_nav_on_map"
            _               -> "ny_ic_vehicle_nav_on_map"
    in variantImage


isAmbulance :: String -> Boolean
isAmbulance vehicleVariant = DA.any (_ == vehicleVariant) ["AMBULANCE_TAXI", "AMBULANCE_TAXI_OXY", "AMBULANCE_AC", "AMBULANCE_AC_OXY", "AMBULANCE_VENTILATOR"]

getAutoImage :: City -> String
getAutoImage city = case city of
    Hyderabad -> "ny_ic_black_yellow_auto"
    _ | elem city [Kochi, Kozhikode, Thrissur, Trivandrum] -> "ny_ic_koc_auto_on_map"
    _ | elem city [Chennai, Vellore, Hosur, Madurai, Thanjavur, Tirunelveli, Salem, Trichy] -> "ny_ic_black_yellow_auto"
    _         -> "ic_auto_nav_on_map"

isTamilNaduCity :: City -> Boolean 
isTamilNaduCity city = elem city [Chennai, Vellore, Hosur, Madurai, Thanjavur, Tirunelveli, Salem, Trichy, Pudukkottai]

isKeralaCity :: City -> Boolean 
isKeralaCity city = elem city [Kochi, Kozhikode, Thrissur, Trivandrum]

getCityFromString :: String -> City
getCityFromString cityString =
  case cityString of
    "Bangalore" -> Bangalore
    "Kolkata" -> Kolkata
    "Paris" -> Paris
    "Kochi" -> Kochi
    "Delhi" -> Delhi
    "Hyderabad" -> Hyderabad
    "Mumbai" -> Mumbai
    "Chennai" -> Chennai
    "Coimbatore" -> Coimbatore
    "Pondicherry" -> Pondicherry
    "Goa" -> Goa
    "Pune" -> Pune
    "Mysore" -> Mysore
    "Tumakuru" -> Tumakuru
    "Noida" -> Noida
    "Gurugram" -> Gurugram
    "Siliguri" -> Siliguri
    "Trivandrum" -> Trivandrum
    "Thrissur" -> Thrissur
    "Kozhikode" -> Kozhikode
    "Vellore" -> Vellore
    "Hosur" -> Hosur
    "Madurai" -> Madurai
    "Thanjavur" -> Thanjavur
    "Tirunelveli" -> Tirunelveli
    "Salem" -> Salem
    "Trichy" -> Trichy
    "Bhubaneswar" -> Bhubaneswar
    "Cuttack" -> Cuttack
    "Nalgonda" -> Nalgonda
    "Puri" -> Puri
    "Pudukkottai" -> Pudukkottai
    "Bidar" -> Bidar
    _ -> AnyCity
