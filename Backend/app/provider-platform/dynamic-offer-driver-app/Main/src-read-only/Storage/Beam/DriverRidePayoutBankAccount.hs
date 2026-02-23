{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverRidePayoutBankAccount where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DriverRidePayoutBankAccountT f = DriverRidePayoutBankAccountT
  { bankAccountNumberEncrypted :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    bankAccountNumberHash :: (B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash)),
    bankIfscCodeEncrypted :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    bankIfscCodeHash :: (B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash)),
    driverId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    rcId :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverRidePayoutBankAccountT where
  data PrimaryKey DriverRidePayoutBankAccountT f = DriverRidePayoutBankAccountId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverRidePayoutBankAccountId . id

type DriverRidePayoutBankAccount = DriverRidePayoutBankAccountT Identity

$(enableKVPG (''DriverRidePayoutBankAccountT) [('id)] [[('driverId)], [('rcId)]])

$(mkTableInstances (''DriverRidePayoutBankAccountT) "driver_ride_payout_bank_account")
