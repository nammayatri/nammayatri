{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Exophone where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Exophone
import qualified Kernel.External.Call.Types
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data ExophoneT f = ExophoneT
  { backupPhone :: B.C f Data.Text.Text,
    callService :: B.C f Kernel.External.Call.Types.CallService,
    exophoneType :: B.C f Domain.Types.Exophone.ExophoneType,
    id :: B.C f Data.Text.Text,
    isPrimaryDown :: B.C f Kernel.Prelude.Bool,
    merchantId :: B.C f Data.Text.Text,
    merchantOperatingCityId :: B.C f Data.Text.Text,
    primaryPhone :: B.C f Data.Text.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table ExophoneT where
  data PrimaryKey ExophoneT f = ExophoneId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = ExophoneId . id

type Exophone = ExophoneT Identity

$(enableKVPG ''ExophoneT ['id] [['backupPhone], ['primaryPhone]])

$(mkTableInstances ''ExophoneT "exophone")
