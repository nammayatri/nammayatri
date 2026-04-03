{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.TDSDistributionRecord where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.TDSDistributionRecord
import qualified Database.Beam as B



data TDSDistributionRecordT f
    = TDSDistributionRecordT {assessmentYear :: (B.C f Kernel.Prelude.Text),
                              createdAt :: (B.C f Kernel.Prelude.UTCTime),
                              driverId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                              emailAddress :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              fileName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              id :: (B.C f Kernel.Prelude.Text),
                              merchantId :: (B.C f Kernel.Prelude.Text),
                              merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                              quarter :: (B.C f Kernel.Prelude.Text),
                              retryCount :: (B.C f Kernel.Prelude.Int),
                              status :: (B.C f Domain.Types.TDSDistributionRecord.TDSDistributionStatus),
                              updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table TDSDistributionRecordT
    where data PrimaryKey TDSDistributionRecordT f = TDSDistributionRecordId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = TDSDistributionRecordId . id
type TDSDistributionRecord = TDSDistributionRecordT Identity

$(enableKVPG (''TDSDistributionRecordT) [('id)] [[('driverId)]])

$(mkTableInstances (''TDSDistributionRecordT) "tds_distribution_record")

