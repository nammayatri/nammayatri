{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.Location (module Domain.Types.Location, module ReExport) where
import Kernel.Prelude
import Data.Aeson
import Domain.Types.Extra.Location as ReExport
import qualified Kernel.External.Maps.HasCoordinates
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Data.OpenApi
import qualified Tools.Beam.UtilsTH



data Location
    = Location {address :: Domain.Types.Location.LocationAddress,
                createdAt :: Kernel.Prelude.UTCTime,
                id :: Kernel.Types.Id.Id Domain.Types.Location.Location,
                lat :: Kernel.Prelude.Double,
                lon :: Kernel.Prelude.Double,
                updatedAt :: Kernel.Prelude.UTCTime,
                merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
                merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)}
    deriving (Generic, Show, Eq, Kernel.External.Maps.HasCoordinates.HasCoordinates, ToJSON, FromJSON, ToSchema)
data Location'
    = Location' {address :: Domain.Types.Location.LocationAddress,
                 createdAt :: Kernel.Prelude.UTCTime,
                 id :: Kernel.Types.Id.Id Domain.Types.Location.Location,
                 lat :: Kernel.Prelude.Double,
                 lon :: Kernel.Prelude.Double,
                 merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
                 updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
data LocationAPIEntity
    = LocationAPIEntity {area :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                         areaCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                         building :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                         city :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                         country :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                         door :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                         extras :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                         fullAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                         id :: Kernel.Types.Id.Id Domain.Types.Location.Location,
                         instructions :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                         lat :: Kernel.Prelude.Double,
                         lon :: Kernel.Prelude.Double,
                         state :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                         street :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
    deriving (Generic, Show, FromJSON, ToJSON, Data.OpenApi.ToSchema)
data LocationAddress
    = LocationAddress {area :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                       areaCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                       building :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                       city :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                       country :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                       door :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                       extras :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                       fullAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                       instructions :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                       state :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                       street :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
    deriving (Generic, Show, Eq, ToJSON, FromJSON, Data.OpenApi.ToSchema)



