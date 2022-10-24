module Dashboard.Common.Driver.Registration
  ( module Dashboard.Common.Driver.Registration,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Id
import Dashboard.Common as Reexport
import Servant

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

newtype UploadDocumentResp = UploadDocumentResp
  { imageId :: Id Image
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

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
    dateOfRegistration :: Maybe UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
