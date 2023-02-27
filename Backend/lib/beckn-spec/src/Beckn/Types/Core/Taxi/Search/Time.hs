{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Search.Time where

import Control.Lens ((?~))
import Data.Aeson (Value, object, withObject, (.:), (.=))
import Data.Aeson.Types
  ( Parser,
    explicitParseFieldMaybe,
    withText,
  )
import Data.OpenApi
  ( NamedSchema (..),
    OpenApiType (..),
    Referenced (..),
    ToSchema (..),
    description,
    properties,
    required,
    type_,
  )
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Calendar (DayOfWeek (..))
import EulerHS.Prelude hiding (id, (.=))
import GHC.Exts (fromList)
import Kernel.Utils.Example

data Time = Time
  { timestamp :: UTCTime,
    days :: Maybe (S.Set DayOfWeek)
  }
  deriving (Show)

instance ToJSON Time where
  toJSON v =
    object
      [ ("timestamp" .= v.timestamp),
        ("days" .= fmap renderDays v.days)
      ]

instance FromJSON Time where
  parseJSON = withObject "Time" $ \o ->
    Time
      <$> o .: "timestamp"
      <*> explicitParseFieldMaybe parseDays o "days"

renderDays :: S.Set DayOfWeek -> Text
renderDays days =
  T.intercalate "," $ fmap (T.toUpper . T.pack . show) $ S.toList days

parseDays :: Value -> Parser (S.Set DayOfWeek)
parseDays =
  withText "Days" $ \dayString -> do
    let dayStrings = T.splitOn "," dayString
    S.fromList <$> traverse parseDayOfWeek dayStrings

parseDayOfWeek :: T.Text -> Parser DayOfWeek
parseDayOfWeek t = case T.toLower t of
  "monday" -> return Monday
  "tuesday" -> return Tuesday
  "wednesday" -> return Wednesday
  "thursday" -> return Thursday
  "friday" -> return Friday
  "saturday" -> return Saturday
  "sunday" -> return Sunday
  _ -> fail "Invalid week day"

instance ToSchema Time where
  declareNamedSchema _ = do
    let timestampSchema =
          mempty & type_ ?~ OpenApiString
            & description ?~ "ISO8601 Timestamp"
        daysSchema =
          mempty & type_ ?~ OpenApiString
            & description ?~ "Comma separated values representing days of the week (e.g. monday,tuesday,friday)"
    return $
      NamedSchema (Just "Time") $
        mempty
          & type_ ?~ OpenApiObject
          & properties
            .~ ( fromList
                   [ ("timestamp", Inline timestampSchema),
                     ("days", Inline daysSchema)
                   ]
               )
          & required .~ ["timestamp"]

instance Example Time where
  example =
    Time
      { timestamp = example,
        days = Nothing
      }
