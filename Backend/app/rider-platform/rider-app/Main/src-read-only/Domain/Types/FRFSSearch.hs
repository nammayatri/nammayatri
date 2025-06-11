{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSSearch where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PartnerOrganization
import qualified Domain.Types.Person
import qualified Domain.Types.RecentLocation
import qualified Domain.Types.Route
import qualified Domain.Types.Station
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.JourneyLeg.Types
import qualified Tools.Beam.UtilsTH

data FRFSSearch = FRFSSearch
  { fromStationId :: Kernel.Types.Id.Id Domain.Types.Station.Station,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch,
    integratedBppConfigId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig),
    isOnSearchReceived :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    journeyLegInfo :: Kernel.Prelude.Maybe Lib.JourneyLeg.Types.JourneySearchData,
    journeyLegStatus :: Kernel.Prelude.Maybe Lib.JourneyLeg.Types.JourneyLegStatus,
    journeyRouteDetails :: [Lib.JourneyLeg.Types.MultiModalJourneyRouteDetails],
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    partnerOrgId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrganization),
    partnerOrgTransactionId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrgTransaction),
    quantity :: Kernel.Prelude.Int,
    recentLocationId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.RecentLocation.RecentLocation),
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    routeId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Route.Route),
    toStationId :: Kernel.Types.Id.Id Domain.Types.Station.Station,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
