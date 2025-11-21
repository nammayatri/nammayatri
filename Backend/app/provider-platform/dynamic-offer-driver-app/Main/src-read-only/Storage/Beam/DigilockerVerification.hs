{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DigilockerVerification where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DigilockerVerification
import qualified Domain.Types.DocStatus
import qualified Domain.Types.VehicleCategory
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DigilockerVerificationT f = DigilockerVerificationT
  { accessToken :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    accessTokenExpiresAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    authorizationCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    codeChallenge :: (B.C f Kernel.Prelude.Text),
    codeMethod :: (B.C f Kernel.Prelude.Text),
    codeVerifier :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    docStatus :: (B.C f Domain.Types.DocStatus.DocStatusMap),
    driverId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    responseCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    responseDescription :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    scope :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    sessionStatus :: (B.C f Domain.Types.DigilockerVerification.SessionStatus),
    stateId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    vehicleCategory :: (B.C f Domain.Types.VehicleCategory.VehicleCategory)
  }
  deriving (Generic, B.Beamable)

instance B.Table DigilockerVerificationT where
  data PrimaryKey DigilockerVerificationT f = DigilockerVerificationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DigilockerVerificationId . id

type DigilockerVerification = DigilockerVerificationT Identity

$(enableKVPG (''DigilockerVerificationT) [('id)] [[('driverId)], [('stateId)]])

$(mkTableInstances (''DigilockerVerificationT) "digilocker_verification")
