{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RegistrationToken where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.RegistrationToken
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data RegistrationTokenT f = RegistrationTokenT
  { alternateNumberAttempts :: B.C f Kernel.Prelude.Int,
    attempts :: B.C f Kernel.Prelude.Int,
    authExpiry :: B.C f Kernel.Prelude.Int,
    authMedium :: B.C f Domain.Types.RegistrationToken.Medium,
    authType :: B.C f Domain.Types.RegistrationToken.LoginType,
    authValueHash :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    entityId :: B.C f Kernel.Prelude.Text,
    entityType :: B.C f Domain.Types.RegistrationToken.RTEntityType,
    id :: B.C f Kernel.Prelude.Text,
    info :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    token :: B.C f Kernel.Prelude.Text,
    tokenExpiry :: B.C f Kernel.Prelude.Int,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    verified :: B.C f Kernel.Prelude.Bool
  }
  deriving (Generic, B.Beamable)

instance B.Table RegistrationTokenT where
  data PrimaryKey RegistrationTokenT f = RegistrationTokenId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RegistrationTokenId . id

type RegistrationToken = RegistrationTokenT Identity

$(enableKVPG ''RegistrationTokenT ['id] [['entityId], ['token]])

$(mkTableInstances ''RegistrationTokenT "registration_token")
