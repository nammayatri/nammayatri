{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSSearch where

import qualified API.Types.UI.RiderLocation
import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PartnerOrganization
import qualified Domain.Types.Person
import qualified Domain.Types.RecentLocation
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FRFSSearch = FRFSSearch
  { busLocationData :: [API.Types.UI.RiderLocation.BusLocation],
    fromStationAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fromStationCode :: Kernel.Prelude.Text,
    fromStationName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fromStationPoint :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch,
    integratedBppConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
    isOnSearchReceived :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    minimalData :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    multimodalSearchRequestId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    onSearchFailed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    partnerOrgId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrganization),
    partnerOrgTransactionId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrgTransaction),
    quantity :: Kernel.Prelude.Int,
    recentLocationId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.RecentLocation.RecentLocation),
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    routeCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    searchAsParentStops :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    toStationAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    toStationCode :: Kernel.Prelude.Text,
    toStationName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    toStationPoint :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    validTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    vehicleNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
