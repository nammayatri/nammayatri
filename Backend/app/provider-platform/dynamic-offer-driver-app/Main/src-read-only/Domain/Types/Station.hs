{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.Station where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.StationType
import qualified Kernel.Types.TimeBound
import qualified Domain.Types.VehicleCategory
import qualified Tools.Beam.UtilsTH



data Station
    = Station {address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
               code :: Kernel.Prelude.Text,
               id :: Kernel.Types.Id.Id Domain.Types.Station.Station,
               lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
               lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
               merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
               merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
               name :: Kernel.Prelude.Text,
               possibleTypes :: Kernel.Prelude.Maybe [Domain.Types.StationType.StationType],
               timeBounds :: Kernel.Types.TimeBound.TimeBound,
               vehicleType :: Domain.Types.VehicleCategory.VehicleCategory,
               createdAt :: Kernel.Prelude.UTCTime,
               updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



