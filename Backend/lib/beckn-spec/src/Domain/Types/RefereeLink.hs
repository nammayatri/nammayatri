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
import qualified Data.Aeson.Text as AT
import Data.OpenApi hiding (info, name, value)
import qualified Data.Text.Lazy as LT
import EulerHS.Prelude hiding (length, map, readMaybe)
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import Kernel.Utils.JSON (removeNullFields, stripPrefixUnderscoreIfAny)
import qualified Kernel.Utils.Schema as Schema
import Kernel.Utils.TH (mkHttpInstancesForEnum)

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

data DriverIdentifierType = REFERRAL_CODE | VEHICLE_NUMBER deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data DriverIdentifier = DriverIdentifier
  { _type :: DriverIdentifierType,
    value :: Text
  }
  deriving (Generic, Show, Read)

instance FromJSON DriverIdentifier where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON DriverIdentifier where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance ToSchema DriverIdentifier where
  declareNamedSchema = genericDeclareNamedSchema Schema.stripPrefixUnderscoreIfAny

mkDriverIdentifier :: Maybe DriverIdentifierType -> Maybe Text -> Maybe DriverIdentifier
mkDriverIdentifier dIType dIValue = do
  dIType' <- dIType
  dIValue' <- dIValue
  pure $
    DriverIdentifier
      { _type = dIType',
        value = dIValue'
      }

-- TODO: Move to shared-kernel
toJsonStr :: ToJSON a => a -> Text
toJsonStr = LT.toStrict . AT.encodeToLazyText

fromJsonStr :: FromJSON a => Text -> Maybe a
fromJsonStr = Data.Aeson.decode . encodeUtf8

$(mkBeamInstancesForEnumAndList ''DriverIdentifierType)

$(mkHttpInstancesForEnum ''DriverIdentifierType)
