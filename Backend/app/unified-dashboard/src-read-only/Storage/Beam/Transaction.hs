{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Transaction where

import qualified Data.Text
import qualified Database.Beam as B
import qualified Domain.Types.AccessMatrix
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data TransactionT f = TransactionT
  { commonDriverId :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    commonRideId :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    endpoint :: (B.C f Domain.Types.AccessMatrix.UserActionType),
    id :: (B.C f Data.Text.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    request :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    requestorId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    response :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    responseError :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    serverName :: (B.C f (Kernel.Prelude.Maybe Domain.Types.AccessMatrix.ServerName)),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table TransactionT where
  data PrimaryKey TransactionT f = TransactionId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = TransactionId . id

type Transaction = TransactionT Identity

$(enableKVPG (''TransactionT) [('id)] [])

$(mkTableInstances (''TransactionT) "transaction")
