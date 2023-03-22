{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Resources.Constants where

import Data.Array (filter, length, null, reverse, (!!))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (Pattern(..), Replacement(..), contains, joinWith, replaceAll, split, trim)
import Prelude (map, (&&), (-), (<>), (==), (>), ($))
import Screens.Types (Address)
import Services.API (AddressComponents(..), BookingLocationAPIEntity(..), SavedReqLocationAPIEntity(..))

type Language
  = { name :: String
    , value :: String
    }

getDelayForAutoComplete :: Int
getDelayForAutoComplete = 500

getDelayForLocateOnMap :: Int
getDelayForLocateOnMap = 1000

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
    if (trim (fromMaybe "" address.city) == "" && trim (fromMaybe "" address.area) == "" && trim (fromMaybe "" address.street) == "" && trim (fromMaybe "" address.door) == "" && trim (fromMaybe "" address.building) == "") then
      ((fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else if (trim (fromMaybe "" address.area) == "" && trim (fromMaybe "" address.street) == "" && trim (fromMaybe "" address.door) == "" && trim (fromMaybe "" address.building) == "") then
      ((fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else if (trim (fromMaybe "" address.street) == "" && trim (fromMaybe "" address.door) == "" && trim (fromMaybe "" address.building) == "") then
      ((fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else if (trim (fromMaybe "" address.door) == "" && trim (fromMaybe "" address.building) == "") then
      ((fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else if (trim (fromMaybe "" address.door) == "") then
      ((fromMaybe "" address.building) <> ", " <> (fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else
      ((fromMaybe "" address.door) <> ", " <> (fromMaybe "" address.building) <> ", " <> (fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))

encodeAddress :: String -> Array AddressComponents -> Maybe String -> Address
encodeAddress fullAddress addressComponents placeId =
  let
    totalAddressComponents = length $ split (Pattern ", ") fullAddress

    splitedAddress = split (Pattern ", ") fullAddress
  in
    { area: splitedAddress !! (totalAddressComponents - 4)
    , areaCode: Just (getValueByComponent addressComponents "postal_code")
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

getAddressFromSaved :: SavedReqLocationAPIEntity -> Address
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

getAddressFromBooking :: BookingLocationAPIEntity -> Address
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
    actualWard = if (trim (replaceAll (Pattern ",") (Replacement "") (fromMaybe "" ward))) == "" then Nothing else ward

    actualArea = if (trim (fromMaybe "" area)) == "" then Nothing else (area <> Just ", ")

    actualStreet = if (trim (fromMaybe "" street)) == "" then Nothing else (street <> Just ", ")

    actualBuilding = if (trim (fromMaybe "" building)) == "" then Nothing else building
  in
    if isJust actualWard then actualWard else (actualArea <> actualStreet <> actualBuilding)

getKeyByLanguage :: String -> String
getKeyByLanguage language = case language of 
  "ENGLISH" -> "EN_US"
  "KANNADA" -> "KN_IN" 
  "HINDI"   -> "HI_IN" 
  "MALAYALAM" -> "ML_IN" 
  "TAMIL"   ->"TA_IN"
  _ -> "EN_US" 