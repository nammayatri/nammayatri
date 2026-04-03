{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.SurgePricing where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Kernel.Types.Common
import qualified Domain.Types.Common
import qualified Tools.Beam.UtilsTH



data SurgePricing
    = SurgePricing {dayOfWeek :: Kernel.Prelude.Text,
                    hourOfDay :: Kernel.Prelude.Int,
                    id :: Kernel.Types.Id.Id Domain.Types.SurgePricing.SurgePricing,
                    sourceHex :: Kernel.Prelude.Text,
                    surgeMultiplier :: Kernel.Types.Common.Centesimal,
                    vehicleServiceTier :: Domain.Types.Common.ServiceTierType,
                    createdAt :: Kernel.Prelude.UTCTime,
                    updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



