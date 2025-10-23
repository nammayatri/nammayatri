{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSQuote where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PartnerOrganization
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FRFSQuote = FRFSQuote
  { _type :: Domain.Types.FRFSQuote.FRFSQuoteType,
    bppDelayedInterest :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    bppItemId :: Kernel.Prelude.Text,
    bppSubscriberId :: Kernel.Prelude.Text,
    bppSubscriberUrl :: Kernel.Prelude.Text,
    childPrice :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    childTicketQuantity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    discountedTickets :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    estimatedPrice :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    eventDiscountAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    fareDetails :: Kernel.Prelude.Maybe Domain.Types.FRFSQuote.FRFSFareDetails,
    fromStationCode :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote,
    integratedBppConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    multimodalSearchRequestId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    oldCacheDump :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    partnerOrgId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrganization),
    partnerOrgTransactionId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrgTransaction),
    price :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    providerDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    providerId :: Kernel.Prelude.Text,
    providerName :: Kernel.Prelude.Text,
    quantity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    routeStationsJson :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    searchId :: Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch,
    stationsJson :: Kernel.Prelude.Text,
    toStationCode :: Kernel.Prelude.Text,
    validTill :: Kernel.Prelude.UTCTime,
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

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''FRFSQuoteType)
