{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Resource.Constants where

import Common.Types.App as Common
import Data.Array as DA
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Array (length, filter, reverse, (!!), null)
import Data.String (Pattern(..), split, toLower, trim, contains, joinWith, replaceAll, Replacement(..))
import Language.Strings (getString)
-- import Helpers.Utils (parseFloat, toStringJSON, extractKeyByRegex, formatFareType)
import Language.Types (STR(..))
import Prelude ((==), (&&), (<>), ($), (/=), (>), map, (-))
import Screens.Types as ST
import Services.API (LocationInfo(..), StopLocation(..), StopLocationAddress(..), TripCategory(..), AddressComponents(..))
import JBridge as JB
import Data.String as DS
import Data.Function.Uncurried (runFn2, Fn2)

foreign import extractKeyByRegex :: Fn2 String String String

type Language =
    {
        name :: String,
        value :: String
    }

whiteListedInputString :: Array String 
whiteListedInputString = ["Hospital"]

getLanguages :: Array Language
getLanguages = 
    [
        {name:"English",value:"EN_US"},
        {name:"ಕನ್ನಡ",value:"KN_IN"},
        {name:"हिन्दी",value :"HI_IN"},
        {name:"தமிழ்",value :"TA_IN"},
        {name:"తెలుగు", value : "TE_IN"}
    ]

decodeAddress :: LocationInfo -> Boolean -> String
decodeAddress ( LocationInfo address) fullAddress =
        if fullAddress then 
             if ( trim (fromMaybe "" address.city) == "" && trim (fromMaybe "" address.area) == "" && trim (fromMaybe "" address.street) == "" && trim (fromMaybe "" address.door) == "" && trim (fromMaybe "" address.building) == "" ) then
                    ((fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
            else if ( trim (fromMaybe "" address.area) == "" && trim (fromMaybe "" address.street) == "" && trim (fromMaybe "" address.door) == "" && trim (fromMaybe "" address.building) == "" ) then
                    ((fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
            else if ( trim (fromMaybe "" address.street) == "" && trim (fromMaybe "" address.door) == "" && trim (fromMaybe "" address.building) == "" ) then
                    ((fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
            else if ( trim (fromMaybe "" address.door) == "" && trim (fromMaybe "" address.building) == "") then
                    ((fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
            else if ( trim (fromMaybe "" address.door) == "") then
                    ((fromMaybe "" address.building) <> ", " <> (fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
                    else
                    ((fromMaybe "" address.door) <> ", " <> (fromMaybe "" address.building) <> ", " <> (fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
        else 
            if ( trim (fromMaybe "" address.city) == "" && trim (fromMaybe "" address.area) == "" && trim (fromMaybe "" address.street) == ""  ) then
                    (trim (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
            else if ( trim (fromMaybe "" address.area) == "" && trim (fromMaybe "" address.street) == "" ) then
                    (trim (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
            else if ( trim (fromMaybe "" address.street) == "") then
                    (trim (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
                    else
                    (trim (fromMaybe "" address.street)) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country)

decodeShortAddress :: LocationInfo -> String
decodeShortAddress (LocationInfo loc) =
  fromMaybe "" loc.area <> ", " <>
  fromMaybe "" loc.city <> ", " <>
  fromMaybe "" loc.country <> ", " <>
  fromMaybe "" loc.areaCode


tripDatesCount :: Int
tripDatesCount = 15

getPspIcon :: String -> String 
getPspIcon vpa = do
    let handleName = ((split (Pattern "@") (vpa)) DA.!! 1)
    case handleName of 
        Nothing -> "ny_ic_defaultpg"
        Just handle -> case handle of
            "ybl" -> "ny_ic_phonepe"
            "ibl" -> "ny_ic_phonepe"
            "axl" -> "ny_ic_phonepe"
            "okhdfcbank" -> "ny_ic_gpay"
            "okicici" -> "ny_ic_gpay"
            "oksbi" -> "ny_ic_gpay"
            "okaxis" -> "ny_ic_gpay"
            "paytm" -> "ny_ic_paytm"
            "apl" -> "ny_ic_amazonpay"
            "yapl" -> "ny_ic_amazonpay"
            "indus" -> "ny_ic_induspay"
            "upi" -> "ny_ic_bhim"
            _ -> "ny_ic_defaultpg"

waitTimeConstructor :: String -> ST.TimerStatus
waitTimeConstructor key = case key of
  "NoStatus" -> ST.NoStatus
  "Triggered" -> ST.Triggered
  "PostTriggered" -> ST.PostTriggered
  "Scheduled" -> ST.Scheduled
  "NotTriggered" -> ST.NotTriggered
  _ -> ST.NoStatus

transformDocText :: ST.RegisterationStep -> String
transformDocText stage = 
  case stage of
    ST.DRIVING_LICENSE_OPTION -> (getString DRIVING_LICENSE)
    ST.VEHICLE_DETAILS_OPTION -> getString VEHICLE_REGISTERATON_CERTIFICATE
    ST.GRANT_PERMISSION -> getString GRANT_PERMISSIONS
    ST.SUBSCRIPTION_PLAN -> getString $ SUBSCRIPTION_PLAN_STR "SUBSCRIPTION_PLAN_STR"
    ST.PROFILE_PHOTO -> getString PROFILE_PHOTO_STR
    ST.AADHAAR_CARD -> getString AADHAAR_CARD_STR
    ST.PAN_CARD -> getString PAN_CARD_STR
    ST.VEHICLE_PERMIT -> getString VEHICLE_PERMIT_STR
    ST.FITNESS_CERTIFICATE -> getString FITNESS_CERTIFICATE_STR
    ST.VEHICLE_INSURANCE -> getString VEHICLE_INSURANCE_STR
    ST.VEHICLE_PUC -> getString VEHICLE_PUC_STR
    ST.NO_OPTION -> ""

transformToRegisterationStep :: String -> ST.RegisterationStep
transformToRegisterationStep doctype = 
  case doctype of
        "DriverLicense" -> ST.DRIVING_LICENSE_OPTION
        "VehicleRegistrationCertificate" -> ST.VEHICLE_DETAILS_OPTION
        "Permissions" -> ST.GRANT_PERMISSION
        "SubscriptionPlan" -> ST.SUBSCRIPTION_PLAN
        "ProfilePhoto" -> ST.PROFILE_PHOTO
        "AadhaarCard" -> ST.AADHAAR_CARD
        "PanCard" -> ST.PAN_CARD
        "VehiclePermit" -> ST.VEHICLE_PERMIT
        "VehicleFitnessCertificate" -> ST.FITNESS_CERTIFICATE
        "VehicleInsurance" -> ST.VEHICLE_INSURANCE
        "VehiclePUC" -> ST.VEHICLE_PUC
        _ -> ST.NO_OPTION

transformToDoctype :: ST.RegisterationStep -> String
transformToDoctype step = 
  case step of
    ST.DRIVING_LICENSE_OPTION -> "DriverLicense"
    ST.VEHICLE_DETAILS_OPTION -> "VehicleRegistrationCertificate"
    ST.VEHICLE_PERMIT -> "VehiclePermit"
    ST.FITNESS_CERTIFICATE -> "VehicleFitnessCertificate"
    ST.VEHICLE_INSURANCE -> "VehicleInsurance"
    ST.VEHICLE_PUC -> "VehiclePUC"
    ST.SUBSCRIPTION_PLAN -> "SubscriptionPlan"
    ST.PROFILE_PHOTO -> "ProfilePhoto"
    ST.AADHAAR_CARD -> "AadhaarCard"
    ST.PAN_CARD -> "PanCard"
    ST.GRANT_PERMISSION -> "Permissions"
    ST.NO_OPTION -> ""

transformVehicleType :: Maybe String -> Maybe ST.VehicleCategory
transformVehicleType vehicletype =
  case vehicletype of
     Just "CAR" -> Just ST.CarCategory
     Just "AUTO_CATEGORY" -> Just ST.AutoCategory
     Just "MOTORCYCLE" -> Just ST.BikeCategory
     Just "AMBULANCE" -> Just ST.AmbulanceCategory
     Just "TRUCK" -> Just ST.TruckCategory
     Just "BUS" -> Just ST.BusCategory
     _ -> Nothing

decodeVehicleType :: String -> Maybe ST.VehicleCategory
decodeVehicleType value = case value of
    "AutoCategory" -> Just ST.AutoCategory
    "CarCategory" -> Just ST.CarCategory
    "BikeCategory" -> Just ST.BikeCategory
    "AmbulanceCategory" -> Just ST.AmbulanceCategory
    "TruckCategory" -> Just ST.TruckCategory
    "BusCategory" -> Just ST.BusCategory
    _ -> Nothing
rideTypeConstructor :: Maybe TripCategory -> ST.TripType
rideTypeConstructor ( tripCategory) = 
  case tripCategory of
    Nothing -> ST.OneWay
    Just (TripCategory tripCategory') ->
        case toLower tripCategory'.tag of
                "oneway" -> ST.OneWay 
                "roundtrip" -> ST.RoundTrip
                "rental" -> ST.Rental
                "intercity" -> ST.Intercity
                "rideshare" -> ST.RideShare
                "delivery" -> ST.Delivery
                _ -> ST.OneWay


constructLocationInfo :: Maybe Number -> Maybe Number -> Maybe LocationInfo
constructLocationInfo (latitude) (longitude) = 
    case latitude,longitude of
        Just latitude',Just longitude' -> 
                Just $ LocationInfo {
                        area :  Just "",
                        state :  Just "",
                        country :  Just "",
                        building :  Just "",
                        door : Just "",
                        street :  Just "",
                        lat : latitude',
                        city :  Just "",
                        areaCode :  Just "",
                        lon : longitude',
                        instructions : Nothing,
                        extras : Nothing
                }
        _,_ -> Nothing

getLocationInfoFromStopLocation :: StopLocationAddress -> Number -> Number -> LocationInfo
getLocationInfoFromStopLocation (StopLocationAddress {door, building, street, area, city, state, country, areaCode}) lat lon = 
     LocationInfo 
        {
                area :  area,
                state :  state,
                country :  country,
                building :  building,
                door : door,
                street :  street,
                lat : lat,
                city :  city,
                areaCode :  areaCode,
                lon : lon,
                instructions : Nothing,
                extras : Nothing
        }

getHomeStageFromString :: String -> ST.HomeScreenStage
getHomeStageFromString localStage = 
  case localStage of
        "HomeScreen" -> ST.HomeScreen
        "RideRequested" -> ST.RideRequested
        "RideAccepted" -> ST.RideAccepted
        "RideStarted" -> ST.RideStarted
        "RideCompleted" -> ST.RideCompleted
        "ChatWithCustomer" -> ST.ChatWithCustomer
        _ -> ST.HomeScreen

verifiedVehicleOnly :: Boolean
verifiedVehicleOnly = true

pendingVehicleOnly :: Boolean
pendingVehicleOnly = false

getCategoryFromVariant :: String -> Maybe ST.VehicleCategory -- check here if any new vehicle category is introduced
getCategoryFromVariant variant = case variant of
  _ | DA.elem variant ["AUTO_RICKSHAW", "EV_AUTO_RICKSHAW"] -> Just ST.AutoCategory
  "BIKE" -> Just ST.BikeCategory
  _ | variant `DA.elem` ["AMBULANCE_TAXI", "AMBULANCE_TAXI_OXY", "AMBULANCE_AC", "AMBULANCE_AC_OXY", "AMBULANCE_VENTILATOR"] -> Just ST.AmbulanceCategory
  _ -> Just ST.CarCategory

rideMoreEarnMorePopupStartTime :: String
rideMoreEarnMorePopupStartTime = "07:30:00"

referAndEarnCoinPopupStartTime :: String
referAndEarnCoinPopupStartTime = "07:30:00"

convertCoinToCashPopupStartTime :: String
convertCoinToCashPopupStartTime = "12:30:00"

convertCoinsToCashPopupEndTime :: String
convertCoinsToCashPopupEndTime = "16:30:00"

dayEndTime :: String
dayEndTime = "23:59:59"

serviceTierMapping :: Boolean -> String -> Maybe Boolean -> String
serviceTierMapping hideAcNonAc tierName acRide = 
  case hideAcNonAc, acRide, tierName of
    _ ,Just true, "AC Mini" -> "Mini"
    _ , _ , "DELIVERY_BIKE" -> "Delivery"
    false,Just true,"Ventilator" ->  "Ventilator"
    false,Just false,"Non-AC ∙ O̶₂̶" -> "O̶2̶"
    false,Just false,"Non-AC ∙ O₂" ->  "O₂"
    false,Just true,"AC ∙ O̶₂̶" -> "O̶2̶"
    false,Just true,"AC ∙ O₂" -> "O₂"
    _ ,Just false, "Non-AC Mini" -> "Mini"
    true,Just true,"Ventilator" ->  "Ventilator"
    true,Just false,"Non-AC ∙ O̶₂̶" -> "Non-AC ∙ O̶₂̶"
    true,Just false,"Non-AC ∙ O₂" ->  "Non-AC ∙ O₂"
    true,Just true,"AC ∙ O̶₂̶" -> "AC ∙ O̶₂̶" 
    true,Just true,"AC ∙ O₂" -> "AC ∙ O₂"
    _ , _ , _ -> tierName

defaultSliderDist :: Int
defaultSliderDist = 4

hvSdkTokenExp :: Int
hvSdkTokenExp = 86400

oneDayInMS :: Int
oneDayInMS = 86400000

maxDriverImages :: Int
maxDriverImages = 4

twoHrsInSec :: Int
twoHrsInSec = 7200

fiveMinInSec :: Int
fiveMinInSec = 300

getDelayForAutoComplete :: Int
getDelayForAutoComplete = 800

encodeAddress :: String -> Array AddressComponents -> Maybe String -> Number -> Number -> ST.Address
encodeAddress fullAddress addressComponents placeId lat lon =
  let
    splitedAddress = split (Pattern ", ") fullAddress
    totalAddressComponents = length splitedAddress
    areaCodeFromFullAdd = runFn2 extractKeyByRegex areaCodeRegex fullAddress
    areaCodeFromAddComp = getValueByComponent addressComponents "postal_code"
    areaCodeComp = if (trim areaCodeFromAddComp) /= "" then areaCodeFromAddComp else areaCodeFromFullAdd
    areaCodeVal = Just if (trim areaCodeComp) == "" then (runFn2 extractKeyByRegex areaCodeRegex $ runFn2 JB.getLocationNameV2 lat lon) else areaCodeComp
    gateName = getValueByComponent addressComponents "sublocality"
  in
    { area: if DS.null gateName then splitedAddress !! (totalAddressComponents - 4) else (Just gateName)
    , areaCode: Just if (trim areaCodeFromAddComp) /= "" then areaCodeFromAddComp else areaCodeFromFullAdd
    , building: splitedAddress !! (totalAddressComponents - 6)
    , city: splitedAddress !! (totalAddressComponents - 3)
    , country: splitedAddress !! (totalAddressComponents - 1)
    , state: splitedAddress !! (totalAddressComponents - 2)
    , door:
        if totalAddressComponents > 7 then
          splitedAddress !! 0 <> Just ", " <> splitedAddress !! 1
        else if totalAddressComponents == 7 then
          splitedAddress !! 0
        else
          Nothing
    , street: splitedAddress !! (totalAddressComponents - 5)
    , ward:
        if null addressComponents then
          getWard Nothing (splitedAddress !! (totalAddressComponents - 4)) (splitedAddress !! (totalAddressComponents - 5)) (splitedAddress !! (totalAddressComponents - 6))
        else
          Just $ getValueByComponent addressComponents "sublocality"
    , placeId: placeId
    }

getValueByComponent :: Array AddressComponents -> String -> String
getValueByComponent address componentName = getAddress $ filter (\(AddressComponents item) -> length (getByTag item.types componentName) > 0) address

getByTag :: Array String -> String -> Array String
getByTag tags componentName = (filter (\item -> contains (Pattern componentName) item) tags)

getAddress :: Array AddressComponents -> String
getAddress address = joinWith ", " (reverse (map (\(AddressComponents item) -> item.longName) address))

areaCodeRegex :: String 
areaCodeRegex = "\\b\\d{6}\\b"


getWard :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String
getWard ward area street building =
  let
    actualWard = if (trim (replaceAll (Pattern ",") (Replacement "") (fromMaybe "" ward))) == "" then Nothing else ward

    actualArea = if (trim (fromMaybe "" area)) == "" then Nothing else (area <> Just ", ")

    actualStreet = if (trim (fromMaybe "" street)) == "" then Nothing else (street <> Just ", ")

    actualBuilding = if (trim (fromMaybe "" building)) == "" then Nothing else building
  in
    if isJust actualWard then actualWard else (actualArea <> actualStreet <> actualBuilding)
