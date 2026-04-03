{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Lib.Payment.Storage.Beam.PersonWallet where
import Kernel.Prelude
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Lib.Payment.Storage.Beam.BeamFlow ()
import qualified Kernel.Types.Common
import qualified Kernel.Prelude
import qualified Database.Beam as B



data PersonWalletT f
    = PersonWalletT {cashAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                     cashFromPointsRedemption :: (B.C f Kernel.Types.Common.HighPrecMoney),
                     createdAt :: (B.C f Kernel.Prelude.UTCTime),
                     expiredBalance :: (B.C f Kernel.Types.Common.HighPrecMoney),
                     id :: (B.C f Kernel.Prelude.Text),
                     merchantId :: (B.C f Kernel.Prelude.Text),
                     merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                     personId :: (B.C f Kernel.Prelude.Text),
                     pointsAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                     updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                     usableCashAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                     usablePointsAmount :: (B.C f Kernel.Types.Common.HighPrecMoney)}
    deriving (Generic, B.Beamable)
instance B.Table PersonWalletT
    where data PrimaryKey PersonWalletT f = PersonWalletId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = PersonWalletId . id
type PersonWallet = PersonWalletT Identity

$(enableKVPG (''PersonWalletT) [('id)] [])

$(mkTableInstancesGenericSchema (''PersonWalletT) "person_wallet")

