{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.Common
  ( module Dashboard.Common,
    module Domain.Types.VehicleVariant,
    VehicleCategory (CAR, MOTORCYCLE, TRAIN, BUS, FLIGHT, AUTO_CATEGORY, AMBULANCE, TRUCK),
    module Reexport,
  )
where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as Csv
import Data.OpenApi
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Domain.Types.VehicleCategory
import Domain.Types.VehicleVariant
import Kernel.Prelude
import Kernel.ServantMultipart
import Kernel.Types.HideSecrets
import Kernel.Types.HideSecrets as Reexport
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Servant

data Customer

data Driver

data User

data Image

data Ride

data Message

data File

data Receiver

data Booking

data IssueReport

data IssueCategory

data FarePolicy

data DriverHomeLocation

data DriverGoHomeRequest

data Document

data CommonDriverOnboardingDocuments

data TripTransaction

data CoinsConfig

data Person

data IntegratedBPPConfig

data Role = DRIVER | FLEET
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

instance FromHttpApiData Role where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData Role where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data Operator

data VerificationStatus = PENDING | VALID | INVALID | MANUAL_VERIFICATION_REQUIRED | UNAUTHORIZED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverVehicleDetails = DriverVehicleDetails
  { vehicleManufacturer :: Text,
    vehicleModel :: Text,
    vehicleColour :: Text,
    vehicleDoors :: Maybe Int,
    vehicleSeatBelts :: Maybe Int,
    vehicleModelYear :: Maybe Int
  }
  deriving (Generic, ToSchema, Show, ToJSON, FromJSON)

data Summary = Summary
  { totalCount :: Int, --TODO add db indexes
    count :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ListItemResult = SuccessItem | FailItem Text
  deriving stock (Show, Generic)

instance ToJSON ListItemResult where
  toJSON = genericToJSON listItemOptions

instance FromJSON ListItemResult where
  parseJSON = genericParseJSON listItemOptions

instance ToSchema ListItemResult where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions listItemOptions

listItemOptions :: Options
listItemOptions =
  defaultOptions
    { sumEncoding = listItemTaggedObject
    }

listItemTaggedObject :: SumEncoding
listItemTaggedObject =
  TaggedObject
    { tagFieldName = "result",
      contentsFieldName = "errorMessage"
    }

-- is it correct to show every error?
listItemErrHandler :: Monad m => SomeException -> m ListItemResult
listItemErrHandler = pure . FailItem . show @Text @SomeException

addMultipartBoundary :: LBS.ByteString -> ((LBS.ByteString, req) -> res) -> (req -> res)
addMultipartBoundary boundary clientFn reqBody = clientFn (boundary, reqBody)

newtype PersonIdsCsvRow = PersonIdsCsvRow
  { personId :: Text
  }

instance Csv.FromNamedRecord PersonIdsCsvRow where
  parseNamedRecord r = PersonIdsCsvRow <$> r Csv..: "personId"

data DriverTagBulkCSVRow = DriverTagBulkCSVRow
  { driverId :: Text,
    tagName :: Text,
    tagValue :: Text
  }

instance Csv.FromNamedRecord DriverTagBulkCSVRow where
  parseNamedRecord r =
    DriverTagBulkCSVRow
      <$> r Csv..: "driverId"
      <*> r Csv..: "tagName"
      <*> r Csv..: "tagValue"

newtype PersonIdsReq = PersonIdsReq {file :: FilePath}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PersonIdsReq where
  hideSecrets = Kernel.Prelude.identity

newtype UpdateTagBulkReq = UpdateTagBulkReq {file :: FilePath}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateTagBulkReq where
  hideSecrets = Kernel.Prelude.identity

newtype PersonMobileNumberIdsCsvRow = PersonMobileNumberIdsCsvRow
  { mobileNumber :: Maybe Text
  }

instance Csv.FromNamedRecord PersonMobileNumberIdsCsvRow where
  parseNamedRecord r = PersonMobileNumberIdsCsvRow <$> r Csv..: "mobileNumber"

newtype PersonMobileNoReq = PersonMobileNoReq {file :: FilePath}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PersonMobileNoReq where
  hideSecrets = Kernel.Prelude.identity

data PersonRes = PersonRes
  { id :: Text,
    mobileNumber :: Maybe Text,
    alternateMobileNumber :: Maybe Text,
    merchantOperatingCityId :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateTagBulkRes = UpdateTagBulkRes
  { id :: Text,
    isSuccess :: Bool,
    errorReason :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToSchema)

instance ToJSON UpdateTagBulkRes where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

instance FromMultipart Tmp UpdateTagBulkReq where
  fromMultipart form = do
    UpdateTagBulkReq
      <$> fmap fdPayload (lookupFile "file" form)

instance ToMultipart Tmp UpdateTagBulkReq where
  toMultipart form =
    MultipartData [] [FileData "file" (T.pack form.file) "" (form.file)]

instance FromMultipart Tmp PersonIdsReq where
  fromMultipart form = do
    PersonIdsReq
      <$> fmap fdPayload (lookupFile "file" form)

instance ToMultipart Tmp PersonIdsReq where
  toMultipart form =
    MultipartData [] [FileData "file" (T.pack form.file) "" (form.file)]

instance FromMultipart Tmp PersonMobileNoReq where
  fromMultipart form = do
    PersonMobileNoReq
      <$> fmap fdPayload (lookupFile "file" form)

instance ToMultipart Tmp PersonMobileNoReq where
  toMultipart form =
    MultipartData [] [FileData "file" (T.pack form.file) "" (form.file)]

data ServiceNames = YATRI_SUBSCRIPTION | YATRI_RENTAL | DASHCAM_RENTAL_CAUTIO | PREPAID_SUBSCRIPTION
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema, ToParamSchema)

$(mkHttpInstancesForEnum ''ServiceNames)

data WaiveOffMode = WITH_OFFER | WITHOUT_OFFER | NO_WAIVE_OFF
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema, ToParamSchema)

$(mkHttpInstancesForEnum ''WaiveOffMode)

newtype TransactionLogId = TransactionLogId Text
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)
  deriving newtype (FromHttpApiData, ToHttpApiData)

instance Kernel.Types.HideSecrets.HideSecrets TransactionLogId where
  hideSecrets = Kernel.Prelude.identity

data ActionSource = DriverDirect | DriverOnApproval | AutoDetect | Dashboard | ForceDashboard | CronJob
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema, ToParamSchema)

$(mkHttpInstancesForEnum ''ActionSource)

data EarningType = DAILY | WEEKLY | MONTHLY
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema, ToParamSchema)

$(mkHttpInstancesForEnum ''EarningType)
