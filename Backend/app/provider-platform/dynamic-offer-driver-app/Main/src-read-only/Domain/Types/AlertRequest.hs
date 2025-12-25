{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.AlertRequest where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.Alert.AlertRequestData
import qualified Domain.Types.Alert.AlertRequestStatus
import qualified Domain.Types.Alert.AlertRequestType
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data AlertRequest = AlertRequest
  { body :: Data.Text.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.AlertRequest.AlertRequest,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    reason :: Kernel.Prelude.Maybe Data.Text.Text,
    requestData :: Domain.Types.Alert.AlertRequestData.AlertRequestData,
    requestType :: Domain.Types.Alert.AlertRequestType.AlertRequestType,
    requesteeId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    requesteeType :: Domain.Types.AlertRequest.RequesteeType,
    requestorId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    requestorType :: Domain.Types.AlertRequest.RequestorType,
    status :: Domain.Types.Alert.AlertRequestStatus.AlertRequestStatus,
    title :: Data.Text.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RequesteeType = FleetOwner | Driver deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data RequestorType = SystemGenerated | DriverGenerated deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum ''RequestorType)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum ''RequestorType)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum ''RequesteeType)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum ''RequesteeType)
