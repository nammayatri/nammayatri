{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.FRFSStageFare where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Types.Common
import qualified Kernel.Prelude
import qualified Database.Beam as B



data FRFSStageFareT f
    = FRFSStageFareT {amount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                      currency :: (B.C f Kernel.Types.Common.Currency),
                      farePolicyId :: (B.C f Kernel.Prelude.Text),
                      merchantId :: (B.C f Kernel.Prelude.Text),
                      merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                      stage :: (B.C f Kernel.Prelude.Int),
                      createdAt :: (B.C f Kernel.Prelude.UTCTime),
                      updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table FRFSStageFareT
    where data PrimaryKey FRFSStageFareT f = FRFSStageFareId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Int) deriving (Generic, B.Beamable)
          primaryKey = FRFSStageFareId <$> farePolicyId <*> stage
type FRFSStageFare = FRFSStageFareT Identity

$(enableKVPG (''FRFSStageFareT) [('farePolicyId), ('stage)] [])

$(mkTableInstances (''FRFSStageFareT) "frfs_stage_fare")

