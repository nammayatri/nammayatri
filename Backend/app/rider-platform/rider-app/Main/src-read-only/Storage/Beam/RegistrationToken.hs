{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RegistrationToken where

import qualified Database.Beam as B
import qualified Domain.Types.RegistrationToken
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data RegistrationTokenT f = RegistrationTokenT
  { id :: B.C f Kernel.Prelude.Text,
    token :: B.C f Kernel.Prelude.Text,
    attempts :: B.C f Kernel.Prelude.Int,
    authMedium :: B.C f Domain.Types.RegistrationToken.Medium,
    authType :: B.C f Domain.Types.RegistrationToken.LoginType,
    authValueHash :: B.C f Kernel.Prelude.Text,
    verified :: B.C f Kernel.Prelude.Bool,
    authExpiry :: B.C f Kernel.Prelude.Int,
    tokenExpiry :: B.C f Kernel.Prelude.Int,
    entityId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    entityType :: B.C f Domain.Types.RegistrationToken.RTEntityType,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    info :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdViaPartnerOrgId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table RegistrationTokenT where
  data PrimaryKey RegistrationTokenT f = RegistrationTokenId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RegistrationTokenId . id

type RegistrationToken = RegistrationTokenT Identity

$(enableKVPG ''RegistrationTokenT ['id] [['token], ['entityId]])

$(mkTableInstances ''RegistrationTokenT "registration_token")
