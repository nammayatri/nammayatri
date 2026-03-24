{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.ClientPersonInfo where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Client
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified BecknV2.OnDemand.Enums
import qualified Tools.Beam.UtilsTH



data ClientPersonInfo
    = ClientPersonInfo {clientId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Client.Client),
                        createdAt :: Kernel.Prelude.UTCTime,
                        id :: Kernel.Types.Id.Id Domain.Types.ClientPersonInfo.ClientPersonInfo,
                        merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                        merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                        personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                        rideCount :: Kernel.Prelude.Int,
                        updatedAt :: Kernel.Prelude.UTCTime,
                        vehicleCategory :: Kernel.Prelude.Maybe BecknV2.OnDemand.Enums.VehicleCategory}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



