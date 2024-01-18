{-# LANGUAGE ApplicativeDo #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module IssueManagement.Common where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Data.Vector as V
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import EulerHS.Prelude hiding (id, state)
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.External.Encryption
import Kernel.External.Types
import Kernel.Prelude
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id (Id, ShortId)
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Servant hiding (Summary)

data Identifier = CUSTOMER | DRIVER
  deriving (Generic, Show, Read, Eq)

data Ride = Ride
  { id :: Id Ride,
    shortId :: ShortId Ride,
    createdAt :: UTCTime
  }

data MerchantOperatingCity = MerchantOperatingCity
  { id :: Id MerchantOperatingCity,
    merchantId :: Id Merchant,
    city :: Context.City
  }

data PersonE e = Person
  { id :: Id Person,
    language :: Maybe Language,
    firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    mobileNumber :: Maybe (EncryptedHashedField e Text),
    merchantOperatingCityId :: Id MerchantOperatingCity
  }
  deriving (Generic)

type Person = PersonE 'AsEncrypted

type DecryptedPerson = PersonE 'AsUnencrypted

instance EncryptedItem Person where
  type Unencrypted Person = (DecryptedPerson, HashSalt)
  encryptItem (Person {..}, salt) = do
    mobileNumber_ <- encryptItem $ (,salt) <$> mobileNumber
    return Person {mobileNumber = mobileNumber_, ..}
  decryptItem Person {..} = do
    mobileNumber_ <- fmap fst <$> decryptItem mobileNumber
    return (Person {mobileNumber = mobileNumber_, ..}, "")

instance EncryptedItem' Person where
  type UnencryptedItem Person = DecryptedPerson
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst

newtype Merchant = Merchant
  { shortId :: ShortId Merchant
  }

data RideT

data Driver

data User

data LocationAPIEntity = LocationAPIEntity
  { lat :: Double,
    lon :: Double,
    street :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text
  }

data BookingStatus = UPCOMING | UPCOMING_6HRS | ONGOING | ONGOING_6HRS | COMPLETED | CANCELLED
  deriving stock (Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

instance FromHttpApiData BookingStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData BookingStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data RideInfoRes = RideInfoRes
  { customerName :: Maybe Text,
    customerPhoneNo :: Text,
    customerPickupLocation :: LocationAPIEntity,
    customerDropLocation :: Maybe LocationAPIEntity,
    driverName :: Text,
    driverPhoneNo :: Maybe Text,
    vehicleNo :: Text,
    actualFare :: Maybe Money,
    bookingStatus :: Maybe BookingStatus
  }

data IssueStatus = OPEN | PENDING_INTERNAL | PENDING_EXTERNAL | RESOLVED | CLOSED | REOPENED | NOT_APPLICABLE
  deriving (Show, Eq, Ord, Read, Generic, ToSchema, FromJSON, ToJSON, ToParamSchema)

$(mkBeamInstancesForEnum ''IssueStatus)

$(mkHttpInstancesForEnum ''IssueStatus)

data ChatType = IssueMessage | IssueOption | MediaFile | IssueDescription
  deriving (Generic, FromJSON, ToSchema, ToJSON, Show, Read, Eq, Ord)

data Sender = USER | BOT
  deriving (Show, Generic, Read, Eq, Ord, ToJSON, FromJSON, ToSchema)

data MessageType = Text | Audio | Image
  deriving (Show, Generic, Read, Eq, Ord, ToJSON, FromJSON, ToSchema)

data Chat = Chat
  { chatType :: ChatType,
    chatId :: Text,
    timestamp :: UTCTime
  }
  deriving (Show, Generic, Read, Eq, Ord, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be Value => HasSqlValueSyntax be Chat where
  sqlValueSyntax = sqlValueSyntax . toJSON

instance FromField Chat where
  fromField = fromFieldEnum

instance FromField [Chat] where
  fromField f mbValue = V.toList <$> fromField f mbValue

instance (HasSqlValueSyntax be (V.Vector Text)) => HasSqlValueSyntax be [Chat] where
  sqlValueSyntax batchList =
    let x = (show <$> batchList :: [Text])
     in sqlValueSyntax (V.fromList x)

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [Chat]

instance FromBackendRow Postgres [Chat]

data ChatDetail = ChatDetail
  { timestamp :: UTCTime,
    content :: Maybe Text,
    id :: Text,
    chatType :: MessageType,
    sender :: Sender,
    label :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MerchantConfig = MerchantConfig
  { mediaFileSizeUpperLimit :: Int,
    mediaFileUrlPattern :: Text,
    kaptureDisposition :: Text
  }
