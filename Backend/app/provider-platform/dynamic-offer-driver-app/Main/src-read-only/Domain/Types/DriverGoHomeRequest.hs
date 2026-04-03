{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.DriverGoHomeRequest where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.External.Maps.HasCoordinates
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data DriverGoHomeRequest
    = DriverGoHomeRequest {createdAt :: Kernel.Prelude.UTCTime,
                           driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                           id :: Kernel.Types.Id.Id Domain.Types.DriverGoHomeRequest.DriverGoHomeRequest,
                           lat :: Kernel.Prelude.Double,
                           lon :: Kernel.Prelude.Double,
                           mbReachedHome :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
                           merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
                           merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
                           numCancellation :: Kernel.Prelude.Int,
                           status :: Domain.Types.DriverGoHomeRequest.DriverGoHomeRequestStatus,
                           updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, ( Kernel.External.Maps.HasCoordinates.HasCoordinates), ( Show))
data DriverGoHomeRequestStatus = ACTIVE | SUCCESS | FAILED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''DriverGoHomeRequestStatus))

