{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Beam.UserData where

import qualified Data.Aeson
import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Yudhishthira.Types
import Tools.Beam.UtilsTH

data UserDataT f = UserDataT
  { batchNumber :: B.C f Kernel.Prelude.Int,
    chakra :: B.C f Lib.Yudhishthira.Types.Chakra,
    eventId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    userDataValue :: B.C f Data.Aeson.Value,
    userId :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table UserDataT where
  data PrimaryKey UserDataT f = UserDataId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = UserDataId . id

type UserData = UserDataT Identity

$(enableKVPG ''UserDataT ['id] [])

$(mkTableInstancesGenericSchema ''UserDataT "user_data")
