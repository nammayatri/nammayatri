module SharedLogic.External.Umis.Types where

import Data.Aeson
import Kernel.Prelude
import Kernel.Utils.Dhall (FromDhall)

data UmisConfig = UmisConfig
  { url :: BaseUrl,
    authToken :: Text
  }
  deriving (Generic, FromDhall)

data UmisStudentResp = UmisStudentResp
  { studentName :: Maybe Text,
    gender :: Maybe Text,
    emailId :: Maybe Text,
    mobileNumber :: Maybe Text,
    communicationPostalAddress :: Maybe Text,
    communicationAddressDistrict :: Maybe Text,
    communicationAddressVillage :: Maybe Text,
    currentStatusOfStudent :: Maybe Text,
    instituteName :: Maybe Text,
    academicYearOfJoining :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON)
