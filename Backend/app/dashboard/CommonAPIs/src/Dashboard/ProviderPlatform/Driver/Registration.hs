{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Dashboard.ProviderPlatform.Driver.Registration
  ( module Dashboard.ProviderPlatform.Driver.Registration,
    module Reexport,
  )
where

import Dashboard.Common as Reexport
import Kernel.External.Notification.FCM.Types (FCMRecipientToken)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Servant

-- we need to save endpoint transactions only for POST, PUT, DELETE APIs
data DriverRegistrationEndpoint
  = UploadDocumentEndpoint
  | RegisterDLEndpoint
  | RegisterRCEndpoint
  | GenerateAadhaarOtpEndpoint
  | VerifyAadhaarOtpEndpoint
  | AuthEndpoint
  deriving (Show, Read, ToJSON, FromJSON, Generic, Eq, Ord)

derivePersistField "DriverRegistrationEndpoint"

-- driver documents list API --------------
-- ----------------------------------------

type DocumentsListAPI =
  Capture "driverId" (Id Driver)
    :> "documents"
    :> "list"
    :> Get '[JSON] DocumentsListResponse

data DocumentsListResponse = DocumentsListResponse
  { driverLicense :: [Text],
    vehicleRegistrationCertificate :: [Text]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

-- driver documents list API --------------
-- ----------------------------------------

type GetDocumentAPI =
  "getDocument"
    :> Capture "imageId" (Id Image)
    :> Get '[JSON] GetDocumentResponse

newtype GetDocumentResponse = GetDocumentResponse
  { imageBase64 :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

-- driver documents list API --------------
-- ----------------------------------------

type UploadDocumentAPI =
  Capture "driverId" (Id Driver)
    :> "document"
    :> "upload"
    :> ReqBody '[JSON] UploadDocumentReq
    :> Post '[JSON] UploadDocumentResp

data DocumentType
  = DriverLicense
  | VehicleRegistrationCertificate
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data UploadDocumentReq = UploadDocumentReq
  { imageBase64 :: Text,
    imageType :: DocumentType
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype UploadDocumentTReq = UploadDocumentTReq
  { imageType :: DocumentType
  }
  deriving (Generic, ToJSON, FromJSON)

newtype UploadDocumentResp = UploadDocumentResp
  { imageId :: Id Image
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets UploadDocumentReq where
  type ReqWithoutSecrets UploadDocumentReq = UploadDocumentTReq
  hideSecrets UploadDocumentReq {..} = UploadDocumentTReq {..}

instance HideSecrets UploadDocumentResp where
  hideSecrets = identity

-- register DL API ------------------------
-- ----------------------------------------

type RegisterDLAPI =
  Capture "driverId" (Id Driver)
    :> "register"
    :> "dl"
    :> ReqBody '[JSON] RegisterDLReq
    :> Post '[JSON] APISuccess

data RegisterDLReq = RegisterDLReq
  { driverLicenseNumber :: Text,
    operatingCity :: Text,
    driverDateOfBirth :: UTCTime,
    imageId1 :: Id Image,
    imageId2 :: Maybe (Id Image),
    dateOfIssue :: Maybe UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets RegisterDLReq where
  hideSecrets = identity

-- register RC API ------------------------
-- ----------------------------------------

type RegisterRCAPI =
  Capture "driverId" (Id Driver)
    :> "register"
    :> "rc"
    :> ReqBody '[JSON] RegisterRCReq
    :> Post '[JSON] APISuccess

data RegisterRCReq = RegisterRCReq
  { vehicleRegistrationCertNumber :: Text,
    imageId :: Id Image,
    operatingCity :: Text,
    dateOfRegistration :: Maybe UTCTime,
    multipleRC :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets RegisterRCReq where
  hideSecrets = identity

-- generate AadhaarOtp  API ------------------------
-- ----------------------------------------

type GenerateAadhaarOtpAPI =
  Capture "driverId" (Id Driver)
    :> "register"
    :> "generateAadhaarOtp"
    :> ReqBody '[JSON] GenerateAadhaarOtpReq
    :> Post '[JSON] GenerateAadhaarOtpRes

data GenerateAadhaarOtpReq = GenerateAadhaarOtpReq
  { aadhaarNumber :: Text,
    consent :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets GenerateAadhaarOtpReq where
  hideSecrets = identity

data GenerateAadhaarOtpRes = GenerateAadhaarOtpRes
  { message :: Text,
    transactionId :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets GenerateAadhaarOtpRes where
  hideSecrets = identity

-- verify AadhaarOtp  API ------------------------
-- ----------------------------------------

type VerifyAadhaarOtpAPI =
  Capture "driverId" (Id Driver)
    :> "register"
    :> "verifyAadhaarOtp"
    :> ReqBody '[JSON] VerifyAadhaarOtpReq
    :> Post '[JSON] VerifyAadhaarOtpRes

data VerifyAadhaarOtpReq = VerifyAadhaarOtpReq
  { otp :: Int,
    shareCode :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets VerifyAadhaarOtpReq where
  hideSecrets = identity

data VerifyAadhaarOtpRes = VerifyAadhaarOtpRes
  { message :: Text,
    code :: Text,
    name :: Text,
    gender :: Text,
    date_of_birth :: Text,
    share_code :: Text,
    image :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets VerifyAadhaarOtpRes where
  hideSecrets = identity

-- auth  API ------------------------
-- ----------------------------------------

type AuthAPI =
  "auth"
    :> ReqBody '[JSON] AuthReq
    :> Post '[JSON] AuthRes

data AuthReq = AuthReq
  { mobileNumber :: Text,
    mobileCountryCode :: Text
  }
  deriving (Generic, FromJSON, ToSchema, ToJSON)

data AuthRes = AuthRes
  { authId :: Text,
    attempts :: Int
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON)

-- verify  API ------------------------
-- ----------------------------------------

type VerifyAPI =
  Capture "authId" Text
    :> "verify"
    :> ReqBody '[JSON] AuthVerifyReq
    :> Post '[JSON] APISuccess

---------- Verify Login --------
data AuthVerifyReq = AuthVerifyReq
  { otp :: Text,
    deviceToken :: FCMRecipientToken
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
