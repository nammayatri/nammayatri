module Lib.JourneyLeg.Types.Bus where

import API.Types.UI.FRFSTicketService
import qualified API.Types.UI.MultimodalConfirm as ApiTypes
import qualified BecknV2.FRFS.Enums as Spec
import Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import Domain.Types.FRFSRouteDetails
import qualified Domain.Types.FRFSSearch as FRFSSearch
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.RecentLocation as DRecentLocation
import Kernel.External.Maps.Google.MapsClient.Types
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import qualified Lib.JourneyModule.Types as JL

data BusLegRequestSearchData = BusLegRequestSearchData
  { quantity :: Int,
    personId :: Id DPerson.Person,
    merchantId :: Id DMerchant.Merchant,
    city :: Context.City,
    journeyLeg :: DJourneyLeg.JourneyLeg,
    multimodalSearchRequestId :: Maybe Text,
    recentLocationId :: Maybe (Id DRecentLocation.RecentLocation),
    serviceTier :: Maybe Spec.ServiceTierType,
    upsertJourneyLegAction :: forall m r c. JL.SearchRequestFlow m r c => Text -> m (),
    blacklistedServiceTiers :: [Spec.ServiceTierType],
    blacklistedFareQuoteTypes :: [DFRFSQuote.FRFSQuoteType],
    isSingleMode :: Bool
  }

data BusLegRequestConfirmData = BusLegRequestConfirmData
  { quoteId :: Maybe (Id FRFSQuote),
    searchId :: Id FRFSSearch.FRFSSearch,
    bookLater :: Bool,
    bookingAllowed :: Bool,
    personId :: Id DPerson.Person,
    merchantId :: Id DMerchant.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    categorySelectionReq :: [FRFSCategorySelectionReq],
    mbEnableOffer :: Maybe Bool,
    isSingleMode :: Maybe Bool,
    mbIsMockPayment :: Maybe Bool
  }

data BusLegRequestUpdateData = BusLegRequestUpdateData

data BusLegRequestCancelData = BusLegRequestCancelData
  { searchId :: Id FRFSSearch.FRFSSearch,
    cancellationType :: Spec.CancellationType
  }

data BusLegRequestGetInfoData = BusLegRequestGetInfoData
  { searchId :: Id FRFSSearch.FRFSSearch,
    journeyLeg :: DJourneyLeg.JourneyLeg,
    journeyLegs :: [DJourneyLeg.JourneyLeg]
  }

data BusLegRequestGetStateData = BusLegRequestGetStateData
  { searchId :: Id FRFSSearch.FRFSSearch,
    riderLastPoints :: [ApiTypes.RiderLocationReq],
    movementDetected :: Bool,
    routeCodeForDetailedTracking :: Maybe Text,
    journeyLeg :: DJourneyLeg.JourneyLeg
  }

data BusLegRequestGetFareData = BusLegRequestGetFareData
  { startLocation :: LatLngV2,
    endLocation :: LatLngV2,
    agencyGtfsId :: Maybe Text,
    routeDetails :: [FRFSRouteDetails],
    merchant :: DMerchant.Merchant,
    riderId :: Id DPerson.Person,
    fromArrivalTime :: Maybe UTCTime,
    serviceType :: Maybe Spec.ServiceTierType,
    merchantOpCity :: DMOC.MerchantOperatingCity,
    blacklistedServiceTiers :: [Spec.ServiceTierType],
    blacklistedFareQuoteTypes :: [DFRFSQuote.FRFSQuoteType],
    isSingleMode :: Bool,
    userPreferredServiceTier :: Maybe Spec.ServiceTierType
  }

data BusLegRequest
  = BusLegRequestSearch BusLegRequestSearchData
  | BusLegRequestConfirm BusLegRequestConfirmData
  | BusLegRequestUpdate BusLegRequestUpdateData
  | BusLegRequestCancel BusLegRequestCancelData
  | BusLegRequestGetFare BusLegRequestGetFareData
  | BusLegRequestGetState BusLegRequestGetStateData
  | BusLegRequestGetInfo BusLegRequestGetInfoData
