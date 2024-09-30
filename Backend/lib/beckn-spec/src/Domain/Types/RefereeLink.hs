{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.RefereeLink where

import Data.Aeson hiding (Success)
import Data.OpenApi hiding (info, name)
import EulerHS.Prelude hiding (length, map, readMaybe)
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import Kernel.Utils.JSON (removeNullFields)

data ReferrerInfo = ReferrerInfo
  { firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    rating :: Maybe Centesimal,
    registeredAt :: UTCTime,
    totalRides :: Int,
    vehicleNumber :: Text,
    vehicleVariant :: Text,
    applicableServiceTiers :: [Text],
    driverImage :: Maybe Text
  }
  deriving (Generic, FromJSON, ToSchema)

instance ToJSON ReferrerInfo where
  toJSON = genericToJSON removeNullFields

newtype LinkRefereeRes = LinkRefereeRes (Either APISuccess ReferrerInfo)
  deriving (Generic, ToSchema)

instance FromJSON LinkRefereeRes where
  parseJSON v =
    LinkRefereeRes <$> ((Left <$> parseJSON v) <|> (Right <$> parseJSON v))

instance ToJSON LinkRefereeRes where
  toJSON (LinkRefereeRes res) = case res of
    Left success -> toJSON success
    Right info -> toJSON info
