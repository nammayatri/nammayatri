{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module DashboardAlert.Storage.Beam.DashboardAlert where

import qualified DashboardAlert.Domain.Types.DashboardAlert
import qualified Data.Text
import qualified Database.Beam as B
import qualified Domain.Types.Alert.AlertEntityType
import qualified Domain.Types.Alert.AlertRequestData
import qualified Domain.Types.Alert.AlertRequestStatus
import qualified Domain.Types.Alert.AlertRequestType
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude

data DashboardAlertT f = DashboardAlertT
  { body :: (B.C f Data.Text.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    entityId :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    entityType :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Alert.AlertEntityType.AlertEntityType)),
    id :: (B.C f Data.Text.Text),
    merchantId :: (B.C f Data.Text.Text),
    merchantOperatingCityId :: (B.C f Data.Text.Text),
    reason :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    requestData :: (B.C f Domain.Types.Alert.AlertRequestData.AlertRequestData),
    requestType :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Alert.AlertRequestType.AlertRequestType)),
    requesteeId :: (B.C f Data.Text.Text),
    requesteeType :: (B.C f (Kernel.Prelude.Maybe DashboardAlert.Domain.Types.DashboardAlert.RequesteeType)),
    requestorId :: (B.C f Data.Text.Text),
    requestorType :: (B.C f (Kernel.Prelude.Maybe DashboardAlert.Domain.Types.DashboardAlert.RequestorType)),
    status :: (B.C f Domain.Types.Alert.AlertRequestStatus.AlertRequestStatus),
    title :: (B.C f Data.Text.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table DashboardAlertT where
  data PrimaryKey DashboardAlertT f = DashboardAlertId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = DashboardAlertId . id

type DashboardAlert = DashboardAlertT Identity

$(enableKVPG (''DashboardAlertT) [('id)] [[('requesteeId)], [('requestorId)]])

$(mkTableInstancesGenericSchema (''DashboardAlertT) "approval_request")
