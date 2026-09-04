{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSQuote where

import qualified API.Types.UI.RiderLocation
import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PartnerOrganization
import qualified Domain.Types.Person
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data FRFSQuote = FRFSQuote
  { _type :: Domain.Types.FRFSQuote.FRFSQuoteType,
    arrivalDate :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    arrivalTime :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    availableSeats :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    bppDelayedInterest :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    bppItemId :: Kernel.Prelude.Text,
    bppSubscriberId :: Kernel.Prelude.Text,
    bppSubscriberUrl :: Kernel.Prelude.Text,
    busLocationData :: [API.Types.UI.RiderLocation.BusLocation],
    concessionTypeId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    departureTime :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    discountedTickets :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    eventDiscountAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    extraFees :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    fareDetails :: Kernel.Prelude.Maybe Domain.Types.FRFSQuote.FRFSFareDetails,
    fromStationAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fromStationCode :: Kernel.Prelude.Text,
    fromStationName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fromStationPoint :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote,
    integratedBppConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    multimodalSearchRequestId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    offerSegment :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    oldCacheDump :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    partnerOrgId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrganization),
    partnerOrgTransactionId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrgTransaction),
    providerClassId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    providerDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    providerId :: Kernel.Prelude.Text,
    providerLayoutId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    providerName :: Kernel.Prelude.Text,
    providerRefNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    providerServiceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    providerTripCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    routeStationsJson :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    searchId :: Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch,
    stationsJson :: Kernel.Prelude.Text,
    toStationAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    toStationCode :: Kernel.Prelude.Text,
    toStationName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    toStationPoint :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    tripCategory :: Kernel.Prelude.Maybe Domain.Types.FRFSQuote.FRFSTripCategory,
    validTill :: Kernel.Prelude.UTCTime,
    vehicleNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)

data FRFSFareDetails = FRFSFareDetails
  { appSession :: Kernel.Prelude.Int,
    distance :: Kernel.Types.Common.Meters,
    providerRouteId :: Kernel.Prelude.Text,
    ticketTypeCode :: Kernel.Prelude.Text,
    trainTypeCode :: Kernel.Prelude.Text,
    via :: Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FRFSQuoteType = SingleJourney | ReturnJourney | Pass | SpecialFareSingleJourney deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data FRFSTripCategory = INTRACITY | INTERCITY deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''FRFSQuoteType)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''FRFSTripCategory)

$(mkHttpInstancesForEnum ''FRFSTripCategory)
