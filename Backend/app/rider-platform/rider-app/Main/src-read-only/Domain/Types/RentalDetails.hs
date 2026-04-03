{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.RentalDetails (module Domain.Types.RentalDetails, module ReExport) where
import Kernel.Prelude
import Data.Aeson
import Domain.Types.Extra.RentalDetails as ReExport
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Domain.Types.Extra.RentalDetails
import qualified Tools.Beam.UtilsTH



data RentalDetails
    = RentalDetails {baseFare :: Kernel.Types.Common.Price,
                     deadKmFare :: Kernel.Types.Common.Price,
                     id :: Kernel.Types.Id.Id Domain.Types.RentalDetails.RentalDetails,
                     includedDistancePerHr :: Kernel.Types.Common.Distance,
                     nightShiftInfo :: Kernel.Prelude.Maybe Domain.Types.Extra.RentalDetails.NightShiftInfo,
                     perExtraKmRate :: Kernel.Types.Common.Price,
                     perExtraMinRate :: Kernel.Types.Common.Price,
                     perHourCharge :: Kernel.Types.Common.Price,
                     plannedPerKmRate :: Kernel.Types.Common.Price}
    deriving (Generic, ( Show))



