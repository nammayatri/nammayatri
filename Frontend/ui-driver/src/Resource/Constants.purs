{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Resource.Constants where

import Prelude ((==), (&&), (<>))
import Data.String (trim)
import Services.API (LocationInfo(..))
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Array as DA
import Data.String (Pattern(..), split, toLower)
import Screens.Types as ST
import Engineering.Helpers.MobilityPrelude

type Language =
    {
        name :: String,
        value :: String
    }

getLanguages :: Array Language
getLanguages = 
    [
        {name:"English",value:"EN_US"},
        {name:"ಕನ್ನಡ",value:"KN_IN"},
        {name:"हिन्दी",value :"HI_IN"},
        {name:"தமிழ்",value :"TA_IN"}
    ]

decodeAddress :: LocationInfo -> Boolean -> String
decodeAddress ( LocationInfo address) fullAddress =
        if fullAddress then 
             if ( trim (fromMaybeString address.city) == "" && trim (fromMaybeString address.area) == "" && trim (fromMaybeString address.street) == "" && trim (fromMaybeString address.door) == "" && trim (fromMaybeString address.building) == "" ) then
                    ((fromMaybeString address.state) <> ", " <> (fromMaybeString address.country))
            else if ( trim (fromMaybeString address.area) == "" && trim (fromMaybeString address.street) == "" && trim (fromMaybeString address.door) == "" && trim (fromMaybeString address.building) == "" ) then
                    ((fromMaybeString address.city) <> ", " <> (fromMaybeString address.state) <> ", " <> (fromMaybeString address.country))
            else if ( trim (fromMaybeString address.street) == "" && trim (fromMaybeString address.door) == "" && trim (fromMaybeString address.building) == "" ) then
                    ((fromMaybeString address.area) <> ", " <> (fromMaybeString address.city) <> ", " <> (fromMaybeString address.state) <> ", " <> (fromMaybeString address.country))
            else if ( trim (fromMaybeString address.door) == "" && trim (fromMaybeString address.building) == "") then
                    ((fromMaybeString address.street) <> ", " <> (fromMaybeString address.area) <> ", " <> (fromMaybeString address.city) <> ", " <> (fromMaybeString address.state) <> ", " <> (fromMaybeString address.country))
            else if ( trim (fromMaybeString address.door) == "") then
                    ((fromMaybeString address.building) <> ", " <> (fromMaybeString address.street) <> ", " <> (fromMaybeString address.area) <> ", " <> (fromMaybeString address.city) <> ", " <> (fromMaybeString address.state) <> ", " <> (fromMaybeString address.country))
                    else
                    ((fromMaybeString address.door) <> ", " <> (fromMaybeString address.building) <> ", " <> (fromMaybeString address.street) <> ", " <> (fromMaybeString address.area) <> ", " <> (fromMaybeString address.city) <> ", " <> (fromMaybeString address.state) <> ", " <> (fromMaybeString address.country))
        else 
            if ( trim (fromMaybeString address.city) == "" && trim (fromMaybeString address.area) == "" && trim (fromMaybeString address.street) == ""  ) then
                    (trim (fromMaybeString address.state) <> ", " <> (fromMaybeString address.country))
            else if ( trim (fromMaybeString address.area) == "" && trim (fromMaybeString address.street) == "" ) then
                    (trim (fromMaybeString address.city) <> ", " <> (fromMaybeString address.state) <> ", " <> (fromMaybeString address.country))
            else if ( trim (fromMaybeString address.street) == "") then
                    (trim (fromMaybeString address.area) <> ", " <> (fromMaybeString address.city) <> ", " <> (fromMaybeString address.state) <> ", " <> (fromMaybeString address.country))
                    else
                    (trim (fromMaybeString address.street)) <> ", " <> (fromMaybeString address.area) <> ", " <> (fromMaybeString address.city) <> ", " <> (fromMaybeString address.state) <> ", " <> (fromMaybeString address.country)

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
  _ -> ST.NoStatus