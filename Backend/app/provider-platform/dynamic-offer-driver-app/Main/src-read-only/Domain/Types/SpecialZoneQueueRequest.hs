{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.SpecialZoneQueueRequest where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data SpecialZoneQueueRequest
    = SpecialZoneQueueRequest {createdAt :: Kernel.Prelude.UTCTime,
                               driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                               gateId :: Kernel.Prelude.Text,
                               gateName :: Kernel.Prelude.Text,
                               id :: Kernel.Types.Id.Id Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest,
                               merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                               merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                               response :: Kernel.Prelude.Maybe Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequestResponse,
                               specialLocationId :: Kernel.Prelude.Text,
                               specialLocationName :: Kernel.Prelude.Text,
                               status :: Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequestStatus,
                               updatedAt :: Kernel.Prelude.UTCTime,
                               validTill :: Kernel.Prelude.UTCTime,
                               vehicleType :: Kernel.Prelude.Text}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
data SpecialZoneQueueRequestResponse = Accept | Reject | Ignored | NoShow deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)
data SpecialZoneQueueRequestStatus = Active | Expired deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''SpecialZoneQueueRequestResponse))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''SpecialZoneQueueRequestStatus))

