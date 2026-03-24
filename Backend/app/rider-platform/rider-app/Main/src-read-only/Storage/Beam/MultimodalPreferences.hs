{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.MultimodalPreferences where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Domain.Types.Common
import qualified Kernel.Prelude
import qualified BecknV2.FRFS.Enums
import qualified Domain.Types.MultimodalPreferences
import qualified Database.Beam as B



data MultimodalPreferencesT f
    = MultimodalPreferencesT {allowedTransitModes :: (B.C f [Domain.Types.Common.MultimodalTravelMode]),
                              busTransitTypes :: (B.C f (Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceTierType])),
                              journeyOptionsSortingType :: (B.C f Domain.Types.MultimodalPreferences.JourneyOptionsSortingType),
                              personId :: (B.C f Kernel.Prelude.Text),
                              subwayTransitTypes :: (B.C f (Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceTierType])),
                              merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                              merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                              createdAt :: (B.C f Kernel.Prelude.UTCTime),
                              updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table MultimodalPreferencesT
    where data PrimaryKey MultimodalPreferencesT f = MultimodalPreferencesId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = MultimodalPreferencesId . personId
type MultimodalPreferences = MultimodalPreferencesT Identity

$(enableKVPG (''MultimodalPreferencesT) [('personId)] [])

$(mkTableInstances (''MultimodalPreferencesT) "multimodal_preferences")

