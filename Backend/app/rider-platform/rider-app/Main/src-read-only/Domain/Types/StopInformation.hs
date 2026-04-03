{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.StopInformation where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Ride
import qualified Domain.Types.Location
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data StopInformation
    = StopInformation {createdAt :: Kernel.Prelude.UTCTime,
                       id :: Kernel.Types.Id.Id Domain.Types.StopInformation.StopInformation,
                       rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
                       stopId :: Kernel.Types.Id.Id Domain.Types.Location.Location,
                       stopOrder :: Kernel.Prelude.Int,
                       updatedAt :: Kernel.Prelude.UTCTime,
                       waitingTimeEnd :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
                       waitingTimeStart :: Kernel.Prelude.UTCTime,
                       merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
                       merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



