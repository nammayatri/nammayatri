{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Dashcam.Domain.Cautio.Types where

import Data.Aeson
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Prelude hiding (error)

data CautioConfig = CautioConfig
  { url :: BaseUrl,
    merchantId :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data InstallationStatusReq = InstallationStatusReq
  { params :: ParamsCautio
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ParamsCautio = ParamsCautio
  { path :: PathCautio,
    querystring :: QueryStringCautio
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PathCautio = PathCautio
  { organisation_id :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data QueryStringCautio = QueryStringCautio
  { page :: Int,
    pageSize :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data InstallationStatus = NOT_INSTALLED | INSTALLED deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data InstallationRespEntity = InstallationRespEntity
  { installed :: Bool,
    status :: InstallationStatus,
    installedAt :: Maybe UTCTime,
    cautioDriverId :: Text
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnumAndList ''InstallationStatus)
