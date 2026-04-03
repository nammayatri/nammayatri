{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.StationsExtraInformation where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data StationsExtraInformationT f
    = StationsExtraInformationT {address :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                 createdAt :: (B.C f Kernel.Prelude.UTCTime),
                                 id :: (B.C f Kernel.Prelude.Text),
                                 merchantId :: (B.C f Kernel.Prelude.Text),
                                 merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                                 stationId :: (B.C f Kernel.Prelude.Text),
                                 suggestedDestinations :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                 updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table StationsExtraInformationT
    where data PrimaryKey StationsExtraInformationT f = StationsExtraInformationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = StationsExtraInformationId . id
type StationsExtraInformation = StationsExtraInformationT Identity

$(enableKVPG (''StationsExtraInformationT) [('id)] [])

$(mkTableInstances (''StationsExtraInformationT) "stations_extra_information")

