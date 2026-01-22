{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RegistrationToken where

import qualified Data.Text
import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Tools.Beam.UtilsTH

data RegistrationTokenT f = RegistrationTokenT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    enabled :: (B.C f Kernel.Prelude.Bool),
    id :: (B.C f Data.Text.Text),
    merchantId :: (B.C f Data.Text.Text),
    operatingCity :: (B.C f Kernel.Types.Beckn.Context.City),
    personId :: (B.C f Data.Text.Text),
    token :: (B.C f Data.Text.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table RegistrationTokenT where
  data PrimaryKey RegistrationTokenT f = RegistrationTokenId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = RegistrationTokenId . id

type RegistrationToken = RegistrationTokenT Identity

$(enableKVPG (''RegistrationTokenT) [('id)] [[('token)]])

$(mkTableInstances (''RegistrationTokenT) "registration_token")
