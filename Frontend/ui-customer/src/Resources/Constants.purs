{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Resources.Constants where

import Accessor (_description, _amount)
import Common.Types.App (LazyCheck(..))
import Data.Array (filter, length, null, reverse, (!!), head, all, elem, foldl)
import Data.Function.Uncurried (runFn2)
import Data.Int (toNumber)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (Pattern(..), Replacement(..), contains, joinWith, replaceAll, split, trim)
import Helpers.Utils (parseFloat, toStringJSON, extractKeyByRegex)
import Engineering.Helpers.Commons (os)
import Engineering.Helpers.Utils(isEmpty)
import Language.Strings (getString, getEN)
import Language.Types (STR(..))
import MerchantConfig.Utils (getMerchant, Merchant(..))
import MerchantConfig.Utils (getValueFromConfig)
import Prelude (map, show, (&&), (-), (<>), (==), (>), ($), (+), (/=), (<), (/), (*))
import Screens.Types as ST
import Services.API (AddressComponents(..), BookingLocationAPIEntity(..), SavedReqLocationAPIEntity(..), FareBreakupAPIEntity(..))

type Language
  = { name :: String
    , value :: String
    }

getDelayForAutoComplete :: Int
getDelayForAutoComplete = 800

getDelayForLocateOnMap :: Int
getDelayForLocateOnMap = 1000

getSearchRadius :: Int
getSearchRadius = 100000

getLanguages :: Array Language
getLanguages =
  [ { name: "English", value: "EN_US" }
  , { name: "മലയാളം", value: "ML_IN" }
  ]

data DecodeAddress
  = Booking BookingLocationAPIEntity
  | SavedLoc SavedReqLocationAPIEntity

decodeAddress :: DecodeAddress -> String
decodeAddress addressWithCons =
  let
    (BookingLocationAPIEntity address) = case addressWithCons of
      Booking bookingLocation -> bookingLocation
      SavedLoc savedLocation -> getBookingEntity savedLocation
  in
    if ((isEmpty $ trim (fromMaybe "" address.city)) && (isEmpty $ trim (fromMaybe "" address.area)) && (isEmpty $ trim (fromMaybe "" address.street)) && (isEmpty $ trim (fromMaybe "" address.door)) && (isEmpty $ trim (fromMaybe "" address.building))) then
      ((fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else if ((isEmpty $ trim (fromMaybe "" address.area)) && (isEmpty $ trim (fromMaybe "" address.street)) && (isEmpty $ trim (fromMaybe "" address.door)) && (isEmpty $ trim (fromMaybe "" address.building))) then
      ((fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else if ((isEmpty $ trim (fromMaybe "" address.street)) && (isEmpty $ trim (fromMaybe "" address.door)) && (isEmpty $ trim (fromMaybe "" address.building))) then
      ((fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else if ((isEmpty $ trim (fromMaybe "" address.door)) && (isEmpty $ trim (fromMaybe "" address.building))) then
      ((fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else if ((isEmpty $ trim (fromMaybe "" address.door))) then
      ((fromMaybe "" address.building) <> ", " <> (fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else
      ((fromMaybe "" address.door) <> ", " <> (fromMaybe "" address.building) <> ", " <> (fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))

encodeAddress :: String -> Array AddressComponents -> Maybe String -> ST.Address
encodeAddress fullAddress addressComponents placeId =
  let
    totalAddressComponents = length $ split (Pattern ", ") fullAddress
    areaCodeFromFullAdd = runFn2 extractKeyByRegex areaCodeRegex fullAddress
    areaCodeFromAddComp = getValueByComponent addressComponents "postal_code"

    splitedAddress = split (Pattern ", ") fullAddress
  in
    { area: splitedAddress !! (totalAddressComponents - 4)
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

getBookingEntity :: SavedReqLocationAPIEntity -> BookingLocationAPIEntity
getBookingEntity (SavedReqLocationAPIEntity savedLocation) =
  BookingLocationAPIEntity
    { "area": savedLocation.area
    , "state": savedLocation.state
    , "country": savedLocation.country
    , "building": savedLocation.building
    , "door": savedLocation.door
    , "street": savedLocation.street
    , "city": savedLocation.city
    , "areaCode": savedLocation.areaCode
    , "lat": savedLocation.lat
    , "lon": savedLocation.lon
    , "ward": savedLocation.ward
    , "placeId": savedLocation.placeId
    }

getAddressFromSaved :: SavedReqLocationAPIEntity -> ST.Address
getAddressFromSaved (SavedReqLocationAPIEntity savedLocation) =
  { "area": savedLocation.area
  , "state": savedLocation.state
  , "country": savedLocation.country
  , "building": savedLocation.building
  , "door": savedLocation.door
  , "street": savedLocation.street
  , "city": savedLocation.city
  , "areaCode": savedLocation.areaCode
  , "ward": savedLocation.ward -- (getWard savedLocation.ward savedLocation.area savedLocation.street savedLocation.building)
  , "placeId": savedLocation.placeId
  }

getAddressFromBooking :: BookingLocationAPIEntity -> ST.Address
getAddressFromBooking (BookingLocationAPIEntity address) =
  { "area": address.area
  , "state": address.state
  , "country": address.country
  , "building": address.building
  , "door": address.door
  , "street": address.street
  , "city": address.city
  , "areaCode": address.areaCode
  , "ward": getWard address.ward address.area address.street address.building
  , "placeId": address.placeId
  }

getWard :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String
getWard ward area street building =
  let
    actualWard = if (isEmpty $ trim (replaceAll (Pattern ",") (Replacement "") (fromMaybe "" ward))) then Nothing else ward

    actualArea = if (isEmpty $ trim (fromMaybe "" area)) then Nothing else (area <> Just ", ")

    actualStreet = if (isEmpty $ trim (fromMaybe "" street)) then Nothing else (street <> Just ", ")

    actualBuilding = if (isEmpty $ trim (fromMaybe "" building)) then Nothing else building
  in
    if isJust actualWard then actualWard else (actualArea <> actualStreet <> actualBuilding)

getKeyByLanguage :: String -> String
getKeyByLanguage language = case language of 
  "ENGLISH" -> "EN_US"
  "KANNADA" -> "KN_IN" 
  "HINDI"   -> "HI_IN" 
  "MALAYALAM" -> "ML_IN" 
  "TAMIL"   ->"TA_IN"
  "BENGALI" -> "BN_IN"
  _ -> "EN_US" 

getGender :: Maybe ST.Gender -> String -> String 
getGender gender placeHolderText = 
  case gender of 
    Just value -> case value of 
      ST.MALE -> (getString MALE)
      ST.FEMALE -> (getString FEMALE)
      ST.OTHER ->  (getString OTHER)
      _ -> (getString PREFER_NOT_TO_SAY)
    Nothing -> placeHolderText

getFaresList :: Array FareBreakupAPIEntity -> String -> Array ST.FareComponent
getFaresList fares baseDistance =
  map
    ( \(FareBreakupAPIEntity item) ->
          { fareType : item.description
          , price : (getValueFromConfig "currency") <> " " <> 
            (show $ case item.description of 
              "BASE_FARE" -> item.amount + getMerchSpecBaseFare fares
              "SGST" -> (item.amount * 2) + getFareFromArray fares "FIXED_GOVERNMENT_RATE"
              "WAITING_OR_PICKUP_CHARGES" -> item.amount + getFareFromArray fares "PLATFORM_FEE"
              _ -> item.amount)
          , title : case item.description of
                      "BASE_FARE" -> (getEN BASE_FARES) <> if baseDistance == "0 m" then "" else " (" <> baseDistance <> ")"
                      "EXTRA_DISTANCE_FARE" -> getEN NOMINAL_FARE
                      "DRIVER_SELECTED_FARE" -> getEN DRIVER_ADDITIONS
                      "TOTAL_FARE" -> getEN TOTAL_PAID
                      "DEAD_KILOMETER_FARE" -> getEN PICKUP_CHARGE
                      "PICKUP_CHARGES" -> getEN PICKUP_CHARGE
                      "CUSTOMER_SELECTED_FARE" -> getEN CUSTOMER_SELECTED_FARE
                      "WAITING_CHARGES" -> getEN WAITING_CHARGE
                      "EARLY_END_RIDE_PENALTY" -> getEN EARLY_END_RIDE_CHARGES
                      "WAITING_OR_PICKUP_CHARGES" -> getEN MISC_WAITING_CHARGE 
                      "SERVICE_CHARGE" -> getEN SERVICE_CHARGES
                      "FIXED_GOVERNMENT_RATE" -> getEN GOVERNMENT_CHAGRES
                      "PLATFORM_FEE" -> getEN PLATFORM_FEE
                      "SGST" -> getEN PLATFORM_GST
                      _ -> getEN BASE_FARES
          }
    )
    (getFilteredFares fares)

getMerchSpecBaseFare :: Array FareBreakupAPIEntity -> Int
getMerchSpecBaseFare fares =
  case getMerchant FunctionCall of
    YATRISATHI -> getAllFareFromArray fares ["EXTRA_DISTANCE_FARE", "NIGHT_SHIFT_CHARGE"]
    _ -> getAllFareFromArray fares ["EXTRA_DISTANCE_FARE"]


getAllFareFromArray :: Array FareBreakupAPIEntity -> Array String -> Int
getAllFareFromArray fares titles =
  let
    matchingFares = filter (\fare -> (fare^._description) `elem` titles) fares
  in
    foldl (\acc fare -> acc + fare^._amount) 0 matchingFares

getFareFromArray :: Array FareBreakupAPIEntity -> String -> Int
getFareFromArray fareBreakUp fareType = (fromMaybe dummyFareBreakUp (head (filter (\fare -> fare^._description == (fareType)) fareBreakUp)))^._amount

dummyFareBreakUp :: FareBreakupAPIEntity
dummyFareBreakUp = FareBreakupAPIEntity{amount: 0,description: ""}

getMerchantSpecificFilteredFares :: Merchant -> Array String
getMerchantSpecificFilteredFares merchant = 
  case merchant of
    YATRISATHI -> ["EXTRA_DISTANCE_FARE", "TOTAL_FARE", "BASE_DISTANCE_FARE", "NIGHT_SHIFT_CHARGE", "CGST", "PLATFORM_FEE", "FIXED_GOVERNMENT_RATE"]
    _ -> ["EXTRA_DISTANCE_FARE", "TOTAL_FARE", "BASE_DISTANCE_FARE", "CGST", "NIGHT_SHIFT_CHARGE"]

getFilteredFares :: Array FareBreakupAPIEntity -> Array FareBreakupAPIEntity
getFilteredFares = filter (\(FareBreakupAPIEntity item) -> (all (_ /=  item.description) (getMerchantSpecificFilteredFares (getMerchant FunctionCall))))--["EXTRA_DISTANCE_FARE", "TOTAL_FARE", "BASE_DISTANCE_FARE", "CGST", "NIGHT_SHIFT_CHARGE"]) )

getKmMeter :: Int -> String
getKmMeter distance = if (distance < 1000) then toStringJSON distance <> " m" else (parseFloat ((toNumber distance)/ 1000.0)) 2 <> " km"

-- Info ::
-- Vehicle Variants for yatri sathi are SEDAN_TAXI (SEDAN , SUV, HATCHBACK) and NON_AC_TAXI (TAXI)
fetchVehicleVariant :: String -> Maybe ST.VehicleVariant
fetchVehicleVariant variant = case variant of 
                                "SUV" -> Just ST.SUV
                                "SEDAN" -> Just ST.SEDAN
                                "HATCHBACK" -> Just ST.HATCHBACK
                                "AUTO_RICKSHAW" -> Just ST.AUTO_RICKSHAW
                                "TAXI" -> Just ST.TAXI 
                                "TAXI_PLUS" -> Just ST.TAXI_PLUS
                                _ -> Nothing

getVehicleCapacity :: String -> String 
getVehicleCapacity variant = case getMerchant FunctionCall of
  YATRISATHI -> case fetchVehicleVariant variant of
          Just ST.TAXI -> getString ECONOMICAL <> " · " <>  "4 " <> getString PEOPLE
          Just ST.SUV  -> getString SPACIOUS <> " · " <> "6 " <> getString PEOPLE
          _            -> getString COMFY <> " · " <> "4 " <> getString PEOPLE
  YATRI -> case fetchVehicleVariant variant of
          Just ST.SUV -> "6 " <> (getString SEATS)
          Just ST.AUTO_RICKSHAW -> "3 " <> (getString SEATS)
          _ -> "4 " <> (getString SEATS)
  _ ->    ""

getDisabilityType :: String -> Array ST.DisabilityT -> ST.DisabilityT 
getDisabilityType disType disList = (fromMaybe dummyDisabilityList (head (filter(\item -> item.tag == disType) disList)))

dummyDisabilityList :: ST.DisabilityT
dummyDisabilityList ={
  tag : "OTHER",
  id : "8a365d73-b81e-6b21-962b-b1397aa687e0",
  description : "Other"
}

areaCodeRegex :: String 
areaCodeRegex = "\\b\\d{6}\\b"

ticketPlaceId :: String
ticketPlaceId = "1ef78db2-90de-4ed7-a38a-fcbb7ce28135"

ticketEntryId :: String
ticketEntryId = "b73378dc-427f-4efa-9b55-8efe7e3352c2"

ticketAquariumId :: String
ticketAquariumId = "a7eba6ed-99f7-442f-a9d8-00c8b380657b"

ticketCamId :: String
ticketCamId = "d8f47b42-50a5-4a97-8dda-e80a3633d7ab"
