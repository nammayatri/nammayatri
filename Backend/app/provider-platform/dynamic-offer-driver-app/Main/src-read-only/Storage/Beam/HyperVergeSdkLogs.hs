{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.HyperVergeSdkLogs where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DocumentVerificationConfig
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data HyperVergeSdkLogsT f = HyperVergeSdkLogsT
  { callbackResponse :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    docType :: B.C f (Kernel.Prelude.Maybe Domain.Types.DocumentVerificationConfig.DocumentType),
    driverId :: B.C f Kernel.Prelude.Text,
    failureReason :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    hvFlowId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    status :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    txnId :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table HyperVergeSdkLogsT where
  data PrimaryKey HyperVergeSdkLogsT f = HyperVergeSdkLogsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = HyperVergeSdkLogsId . txnId

type HyperVergeSdkLogs = HyperVergeSdkLogsT Identity

$(enableKVPG ''HyperVergeSdkLogsT ['txnId] [['driverId]])

$(mkTableInstances ''HyperVergeSdkLogsT "hyperverge_sdk_logs")
