{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module DashboardAlert.Domain.Types.DashboardAlert where

import qualified DashboardAlert.Domain.Types.Common
import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.Alert.AlertEntityType
import qualified Domain.Types.Alert.AlertRequestData
import qualified Domain.Types.Alert.AlertRequestStatus
import qualified Domain.Types.Alert.AlertRequestType
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data DashboardAlert = DashboardAlert
  { body :: Data.Text.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    entityId :: Kernel.Prelude.Maybe Data.Text.Text,
    entityType :: Kernel.Prelude.Maybe Domain.Types.Alert.AlertEntityType.AlertEntityType,
    id :: Kernel.Types.Id.Id DashboardAlert.Domain.Types.DashboardAlert.DashboardAlert,
    merchantId :: Kernel.Types.Id.Id DashboardAlert.Domain.Types.Common.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id DashboardAlert.Domain.Types.Common.MerchantOperatingCity,
    reason :: Kernel.Prelude.Maybe Data.Text.Text,
    requestData :: Domain.Types.Alert.AlertRequestData.AlertRequestData,
    requestType :: Domain.Types.Alert.AlertRequestType.AlertRequestType,
    requesteeId :: Kernel.Types.Id.Id DashboardAlert.Domain.Types.Common.Person,
    requesteeType :: DashboardAlert.Domain.Types.DashboardAlert.RequesteeType,
    requestorId :: Kernel.Types.Id.Id DashboardAlert.Domain.Types.Common.Person,
    requestorType :: DashboardAlert.Domain.Types.DashboardAlert.RequestorType,
    status :: Domain.Types.Alert.AlertRequestStatus.AlertRequestStatus,
    title :: Data.Text.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RequesteeType = FleetOwner | Driver deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

data RequestorType = SystemGenerated | DriverGenerated deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''RequestorType))

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''RequestorType))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''RequesteeType))

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''RequesteeType))
