{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DigiLockerLogs where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DocumentVerificationConfig
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DigiLockerLogsT f = DigiLockerLogsT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    digiLockerState :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    digiLockerUri :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    docType :: (B.C f (Kernel.Prelude.Maybe Domain.Types.DocumentVerificationConfig.DocumentType)),
    driverId :: (B.C f Kernel.Prelude.Text),
    errorCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    errorDescription :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    flowType :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    requestPayload :: (B.C f Kernel.Prelude.Text),
    responsePayload :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table DigiLockerLogsT where
  data PrimaryKey DigiLockerLogsT f = DigiLockerLogsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DigiLockerLogsId . id

type DigiLockerLogs = DigiLockerLogsT Identity

$(enableKVPG (''DigiLockerLogsT) [('id)] [[('driverId)]])

$(mkTableInstances (''DigiLockerLogsT) "digilocker_logs")
