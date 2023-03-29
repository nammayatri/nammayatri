{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Lib.Verification.Interface.Types
  ( module Lib.Verification.Interface.Types,
  )
where

import Deriving.Aeson
import Kernel.Prelude
import qualified Lib.Verification.Idfy.Config as Idfy
import qualified Lib.Verification.Idfy.Types.Response as Idfy

newtype VerificationServiceConfig = IdfyConfig Idfy.IdfyCfg
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data VerifyDLAsyncReq = VerifyDLAsyncReq
  { dlNumber :: Text,
    dateOfBirth :: UTCTime
  }
  deriving stock (Show, Generic)

type VerifyDLAsyncResp = VerifyAsyncResp

newtype VerifyRCAsyncReq = VerifyRCAsyncReq
  { rcNumber :: Text
  }
  deriving stock (Show, Generic)

type VerifyRCAsyncResp = VerifyAsyncResp

newtype VerifyAsyncResp = VerifyAsyncResp
  { requestId :: Text
  }
  deriving stock (Show, Generic)

data ValidateImageReq = ValidateImageReq
  { image :: Text,
    imageType :: ImageType
  }
  deriving stock (Show, Generic)

data ImageType = DriverLicense | VehicleRegistrationCertificate
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data ValidateImageResp = ValidateImageResp
  { validationAvailable :: Bool,
    detectedImage :: Maybe DetectedImage
  }
  deriving stock (Show, Generic)

data DetectedImage = DetectedImage
  { imageType :: ImageType,
    isReadable :: Maybe Bool,
    confidence :: Maybe Int
  }
  deriving stock (Show, Generic)

type ExtractRCImageReq = ExtractImageReq

type ExtractDLImageReq = ExtractImageReq

data ExtractImageReq = ExtractImageReq
  { image1 :: Text,
    image2 :: Maybe Text
  }
  deriving stock (Show, Generic)

newtype ExtractRCImageResp = ExtractRCImageResp
  { extractedRC :: Maybe ExtractedRC
  }
  deriving stock (Show, Generic)

newtype ExtractedRC = ExtractedRC
  { rcNumber :: Maybe Text
  }
  deriving stock (Show, Generic)

newtype ExtractDLImageResp = ExtractDLImageResp
  { extractedDL :: Maybe ExtractedDL
  }
  deriving stock (Show, Generic)

newtype ExtractedDL = ExtractedDL
  { dlNumber :: Maybe Text
  }
  deriving stock (Show, Generic)

-- not used in interface

type GetTaskReq = Text

type GetTaskResp = Idfy.VerificationResponse
