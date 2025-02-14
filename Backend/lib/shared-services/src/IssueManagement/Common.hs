{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IssueManagement.Common (module IssueManagement.Common, module Domain.Types.VehicleVariant) where

import qualified AWS.S3 as S3
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Data.Vector as V
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField
import Domain.Types.VehicleVariant
import EulerHS.Prelude hiding (any, elem, id, map, state)
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum, mkBeamInstancesForEnumAndList)
import Kernel.External.Encryption
import Kernel.External.Types
import Kernel.Prelude
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id (Id, ShortId)
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Sequelize.SQLObject (SQLObject (..), ToSQLObject (convertToSQLObject))
import Servant hiding (Summary)
import qualified Text.Show

data Identifier = CUSTOMER | DRIVER
  deriving (Generic, Read, Eq)

instance Show Identifier where
  show CUSTOMER = "Customer"
  show DRIVER = "Driver"

data Ride = Ride
  { id :: Id Ride,
    shortId :: ShortId Ride,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    createdAt :: UTCTime,
    counterPartyRideId :: Maybe Text,
    merchantId :: Id Merchant,
    driverId :: Maybe (Id Person)
  }

data Booking = Booking
  { id :: Id Booking,
    bapId :: Maybe Text,
    bapUri :: Maybe BaseUrl,
    bppId :: Maybe Text,
    bppUri :: Maybe BaseUrl,
    quoteId :: Maybe (Id Quote),
    providerId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity
  }

data Quote = Quote

data MerchantOperatingCity = MerchantOperatingCity
  { id :: Id MerchantOperatingCity,
    merchantId :: Id Merchant,
    city :: Context.City
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FRFSTicketBooking = FRFSTicketBooking
  { id :: Id FRFSTicketBooking,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    merchantId :: Id Merchant,
    providerId :: Text,
    providerUrl :: BaseUrl,
    bppItemId :: Text
  }

data PersonE e = Person
  { id :: Id Person,
    language :: Maybe Language,
    firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    mobileNumber :: Maybe (EncryptedHashedField e Text),
    merchantOperatingCityId :: Id MerchantOperatingCity,
    blocked :: Maybe Bool
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

data Merchant = Merchant
  { id :: Id Merchant,
    shortId :: ShortId Merchant,
    subscriberId :: ShortId Subscriber
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data Subscriber = Subscriber {} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

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
  deriving (Generic, ToJSON)

data RideStatus = R_NEW | R_INPROGRESS | R_COMPLETED | R_CANCELLED | R_UPCOMING
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnumAndList ''RideStatus)

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
    vehicleVariant :: Maybe VehicleVariant,
    vehicleServiceTierName :: Maybe Text,
    actualFare :: Maybe Money,
    bookingStatus :: Maybe BookingStatus,
    rideStatus :: RideStatus,
    merchantOperatingCityId :: Maybe Text,
    estimatedDistance :: Maybe HighPrecMeters,
    chargeableDistance :: Maybe HighPrecMeters,
    estimatedFare :: HighPrecMoney,
    computedPrice :: Maybe HighPrecMoney,
    fareBreakup :: [FareBreakup],
    rideCreatedAt :: UTCTime,
    rideStartTime :: Maybe UTCTime,
    mobileCountryCode :: Maybe Text
  }

data IssueStatus = OPEN | PENDING_INTERNAL | PENDING_EXTERNAL | RESOLVED | CLOSED | REOPENED | NOT_APPLICABLE
  deriving (Show, Eq, Ord, Read, Generic, ToSchema, FromJSON, ToJSON, ToParamSchema)

data FareBreakup = FareBreakup
  { amount :: Price,
    description :: Text,
    entityId :: Text,
    entityType :: FareBreakupEntityType
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data FareBreakupEntityType = BOOKING_UPDATE_REQUEST | BOOKING | RIDE | INITIAL_BOOKING
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

$(mkBeamInstancesForEnumAndList ''VehicleVariant)
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

instance {-# OVERLAPPING #-} ToSQLObject Chat where
  convertToSQLObject = SQLObjectValue . show

data ChatDetail = ChatDetail
  { timestamp :: UTCTime,
    content :: Maybe Text,
    title :: Maybe Text,
    actionText :: Maybe Text,
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
    kaptureDisposition :: Text,
    kaptureQueue :: Text,
    counterPartyUrl :: BaseUrl,
    counterPartyApiKey :: Text,
    sensitiveWords :: Maybe [Text],
    sensitiveWordsForExactMatch :: Maybe [Text]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

allLanguages :: [Language]
allLanguages = [minBound .. maxBound]

data IssueReportType = AC_RELATED_ISSUE | DRIVER_TOLL_RELATED_ISSUE | SYNC_BOOKING | EXTRA_FARE_MITIGATION
  deriving stock (Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

data MandatoryUploads = MandatoryUploads
  { fileType :: S3.FileType,
    limit :: Int
  }
  deriving (Show, Generic, Read, Eq, Ord, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be Value => HasSqlValueSyntax be MandatoryUploads where
  sqlValueSyntax = sqlValueSyntax . toJSON

instance FromField MandatoryUploads where
  fromField = fromFieldEnum

instance FromField [MandatoryUploads] where
  fromField f mbValue = V.toList <$> fromField f mbValue

instance (HasSqlValueSyntax be (V.Vector Text)) => HasSqlValueSyntax be [MandatoryUploads] where
  sqlValueSyntax batchList =
    let x = (show <$> batchList :: [Text])
     in sqlValueSyntax (V.fromList x)

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [MandatoryUploads]

instance FromBackendRow Postgres [MandatoryUploads]

instance {-# OVERLAPPING #-} ToSQLObject MandatoryUploads where
  convertToSQLObject = SQLObjectValue . show

data KaptureConfig = KaptureConfig
  { queue :: Text,
    sosQueue :: Maybe Text,
    l0FeedbackQueue :: Maybe Text,
    disposition :: Text,
    deleteAccountCategory :: Maybe Text
  }
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

fromFieldKaptureConfig ::
  Field ->
  Maybe ByteString ->
  Conversion KaptureConfig
fromFieldKaptureConfig f mbValue = do
  value <- fromField f mbValue
  case fromJSON value of
    Success a -> pure a
    _ -> returnError ConversionFailed f "Conversion failed"

instance FromField KaptureConfig where
  fromField = fromFieldKaptureConfig

instance HasSqlValueSyntax be Value => HasSqlValueSyntax be KaptureConfig where
  sqlValueSyntax = sqlValueSyntax . toJSON

instance BeamSqlBackend be => B.HasSqlEqualityCheck be KaptureConfig

instance FromBackendRow Postgres KaptureConfig

data Translation = Translation
  { language :: Language,
    translation :: Text
  }
  deriving stock (Eq, Show, Generic, Read, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CxAgentDetails = CxAgentDetails
  { agentName :: Text,
    agentMobileNumber :: Text
  }
  deriving (Show, Generic, Read, Eq, Ord, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be Value => HasSqlValueSyntax be CxAgentDetails where
  sqlValueSyntax = sqlValueSyntax . toJSON

instance FromField CxAgentDetails where
  fromField = fromFieldEnum

instance FromField [CxAgentDetails] where
  fromField f mbValue = V.toList <$> fromField f mbValue

instance (HasSqlValueSyntax be (V.Vector Text)) => HasSqlValueSyntax be [CxAgentDetails] where
  sqlValueSyntax batchList =
    let x = (show <$> batchList :: [Text])
     in sqlValueSyntax (V.fromList x)

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [CxAgentDetails]

instance FromBackendRow Postgres [CxAgentDetails]

instance {-# OVERLAPPING #-} ToSQLObject CxAgentDetails where
  convertToSQLObject = SQLObjectValue . show

$(mkHttpInstancesForEnum ''IssueReportType)

checkForLOFeedback :: Maybe [Text] -> Maybe [Text] -> Maybe Text -> Bool
checkForLOFeedback mbSensitiveWords mbSensitiveWordsForExactMatch mbFeedbackDetails =
  let sensitiveWords = fromMaybe [] mbSensitiveWords
      sensitiveWordsForExactMatch = fromMaybe [] mbSensitiveWordsForExactMatch
   in maybe False (checkSensitiveWords sensitiveWords sensitiveWordsForExactMatch) mbFeedbackDetails

checkSensitiveWords :: [Text] -> [Text] -> Text -> Bool
checkSensitiveWords sensitiveWords sensitiveWordsForExactMatch feedbackDetails =
  let loweredcaseFeedback = T.toLower feedbackDetails
      splittedFeedback = filter (not . T.null) $ T.words (replacePunctuation loweredcaseFeedback)
   in not (T.null feedbackDetails) && (any (\word -> T.toLower word `T.isInfixOf` loweredcaseFeedback) sensitiveWords || any (\word -> T.toLower word `elem` map T.toLower splittedFeedback) sensitiveWordsForExactMatch)
  where
    replacePunctuation :: T.Text -> T.Text
    replacePunctuation = T.map replaceChar
      where
        replaceChar c
          | c `elem` ['?', '.', ',', '/'] = ' '
          | otherwise = c
