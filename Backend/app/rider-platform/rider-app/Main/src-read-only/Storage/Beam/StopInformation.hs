{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.StopInformation where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data StopInformationT f
    = StopInformationT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                        id :: (B.C f Kernel.Prelude.Text),
                        rideId :: (B.C f Kernel.Prelude.Text),
                        stopId :: (B.C f Kernel.Prelude.Text),
                        stopOrder :: (B.C f Kernel.Prelude.Int),
                        updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                        waitingTimeEnd :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                        waitingTimeStart :: (B.C f Kernel.Prelude.UTCTime),
                        merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                        merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))}
    deriving (Generic, B.Beamable)
instance B.Table StopInformationT
    where data PrimaryKey StopInformationT f = StopInformationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = StopInformationId . id
type StopInformation = StopInformationT Identity

$(enableKVPG (''StopInformationT) [('id)] [[('rideId)]])

$(mkTableInstancesWithTModifier (''StopInformationT) "stop_information" [])

