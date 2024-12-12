{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSSearch where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PartnerOrganization
import qualified Domain.Types.Person
import qualified Domain.Types.Route
import qualified Domain.Types.Station
import qualified Domain.Types.StationType
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.JourneyPlannerTypes
import qualified Tools.Beam.UtilsTH

data FRFSSearch = FRFSSearch
  { fromStationId :: Kernel.Types.Id.Id Domain.Types.Station.Station,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch,
    journeyLegInfo :: Kernel.Prelude.Maybe Lib.JourneyPlannerTypes.JourneySearchData,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    partnerOrgId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrganization),
    partnerOrgTransactionId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrgTransaction),
    quantity :: Kernel.Prelude.Int,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    routeId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Route.Route),
    stationCategory :: Kernel.Prelude.Maybe Domain.Types.StationType.StationCategory,
    toStationId :: Kernel.Types.Id.Id Domain.Types.Station.Station,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
