{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverRequest where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DriverRequest
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DriverRequestT f = DriverRequestT
  { description :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    id :: (B.C f Data.Text.Text),
    reason :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    requestType :: (B.C f Data.Text.Text),
    status :: (B.C f (Kernel.Prelude.Maybe Domain.Types.DriverRequest.RequestStatus)),
    tripTransactionId :: (B.C f Data.Text.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverRequestT where
  data PrimaryKey DriverRequestT f = DriverRequestId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverRequestId . id

type DriverRequest = DriverRequestT Identity

$(enableKVPG (''DriverRequestT) [('id)] [])

$(mkTableInstances (''DriverRequestT) "driver_request")
