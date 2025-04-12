{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module UrlShortner.Types
  ( UrlCategory (..),
    GenerateShortUrlReq (..),
    GenerateShortUrlRes (..),
    UrlShortnerConfig (..),
  )
where

import qualified Data.Aeson as A
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as T
import Kernel.Prelude hiding (fromString, toString)
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.JSON
import Prelude (show)

{- Author: Jaypal Mudaliyar
  Below categories are coupled with the url-shortner service.
  So whenever adding new category,
  make sure to add corresponding enum to the url-shortner service as well.
-}
metroTicketBooking, mtbEnum, rtkEnum, rideTracking, meterRideReferralLink, mrlEnum :: String
metroTicketBooking = "mtb"
mtbEnum = "METRO_TICKET_BOOKING"
rideTracking = "rtk"
rtkEnum = "RIDE_TRACKING"
meterRideReferralLink = "mrl"
mrlEnum = "METER_RIDE_REFERRAL_LINK"

data UrlCategory = METRO_TICKET_BOOKING | RIDE_TRACKING | METER_RIDE_REFERRAL_LINK
  deriving (Generic)

fromString :: String -> Maybe UrlCategory
fromString str =
  if
      | str == mtbEnum || str == metroTicketBooking -> Just METRO_TICKET_BOOKING
      | str == rtkEnum || str == rideTracking -> Just RIDE_TRACKING
      | str == mrlEnum || str == meterRideReferralLink -> Just METER_RIDE_REFERRAL_LINK
      | otherwise -> Nothing

toString :: UrlCategory -> String
toString METRO_TICKET_BOOKING = metroTicketBooking
toString RIDE_TRACKING = rideTracking
toString METER_RIDE_REFERRAL_LINK = meterRideReferralLink

instance Read UrlCategory where
  readsPrec _ = maybe [] (\x -> [(x, "")]) . fromString

instance Show UrlCategory where
  show = toString

instance FromJSON UrlCategory where
  parseJSON (A.String str) = maybe (fail $ "Invalid UrlCategory:" <> T.unpack str) return (fromString $ T.unpack str)
  parseJSON e = typeMismatch "String" e

instance ToJSON UrlCategory where
  toJSON = A.String . T.pack . toString

data GenerateShortUrlReq = GenerateShortUrlReq
  { baseUrl :: Text,
    customShortCode :: Maybe Text,
    shortCodeLength :: Maybe Int,
    expiryInHours :: Maybe Int,
    urlCategory :: UrlCategory
  }
  deriving (Generic, Read, Show)

instance FromJSON GenerateShortUrlReq where
  parseJSON = genericParseJSON removeNullFields

instance ToJSON GenerateShortUrlReq where
  toJSON = genericToJSON removeNullFields

data GenerateShortUrlRes = GenerateShortUrlRes
  { shortUrl :: Text,
    urlExpiry :: UTCTime
  }
  deriving (Generic, Read, Show)

instance FromJSON GenerateShortUrlRes where
  parseJSON = genericParseJSON removeNullFields

instance ToJSON GenerateShortUrlRes where
  toJSON = genericToJSON removeNullFields

data UrlShortnerConfig = UrlShortnerConfig
  { url :: BaseUrl,
    apiKey :: Text
  }
  deriving (Generic, Show, FromDhall, FromJSON, ToJSON)
