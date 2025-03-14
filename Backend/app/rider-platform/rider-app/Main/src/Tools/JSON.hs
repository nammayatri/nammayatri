{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

module Tools.JSON where

import Data.Aeson
import Data.Char
import qualified Data.Text as T
import Kernel.Prelude

-- FIXME: make generic instances more powerful to capture this case
fareProductOptions :: Options
fareProductOptions =
  defaultOptions
    { sumEncoding = fareProductTaggedObject,
      constructorTagModifier = fareProductConstructorModifier
    }

fareProductTaggedObject :: SumEncoding
fareProductTaggedObject =
  defaultTaggedObject
    { tagFieldName = "fareProductType"
    }

fareProductConstructorModifier :: String -> String
fareProductConstructorModifier = \case
  "OneWayAPIDetails" -> "ONE_WAY"
  "RentalAPIDetails" -> "RENTAL"
  "InterCityAPIDetails" -> "INTER_CITY"
  "DriverOfferAPIDetails" -> "DRIVER_OFFER"
  "AmbulanceAPIDetails" -> "AMBULANCE"
  "DeliveryAPIDetails" -> "DELIVERY" -- for now change accordingly
  "MeterRideAPIDetails" -> "METER_RIDE"
  x -> x

-- Convert camelCase to SCREAMING_SNAKE_CASE
camelCaseToScreamingSnakeCase :: String -> String
camelCaseToScreamingSnakeCase = T.unpack . T.toUpper . T.concatMap addUnderscore . T.pack
  where
    addUnderscore c
      | isUpper c = T.cons '_' (T.singleton c)
      | otherwise = T.singleton c
