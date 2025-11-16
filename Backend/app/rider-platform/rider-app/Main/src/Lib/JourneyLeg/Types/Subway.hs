module Lib.JourneyLeg.Types.Subway where

import API.Types.UI.FRFSTicketService
import qualified API.Types.UI.MultimodalConfirm as ApiTypes
import qualified BecknV2.FRFS.Enums as Spec
import Domain.Types.FRFSQuote
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

data SubwayLegRequestSearchData = SubwayLegRequestSearchData
  { quantity :: Int,
    personId :: Id DPerson.Person,
    merchantId :: Id DMerchant.Merchant,
    city :: Context.City,
    journeyLeg :: DJourneyLeg.JourneyLeg,
    multimodalSearchRequestId :: Maybe Text,
    recentLocationId :: Maybe (Id DRecentLocation.RecentLocation),
    upsertJourneyLegAction :: forall m r c. JL.SearchRequestFlow m r c => Text -> m ()
  }

data SubwayLegRequestUpdateData = SubwayLegRequestUpdateData

data SubwayLegRequestConfirmData = SubwayLegRequestConfirmData
  { quoteId :: Maybe (Id FRFSQuote),
    searchId :: Id FRFSSearch.FRFSSearch,
    bookLater :: Bool,
    bookingAllowed :: Bool,
    personId :: Id DPerson.Person,
    merchantId :: Id DMerchant.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    crisSdkResponse :: Maybe ApiTypes.CrisSdkResponse,
    isSingleMode :: Maybe Bool,
    mbEnableOffer :: Maybe Bool,
    categorySelectionReq :: [FRFSCategorySelectionReq]
  }

data SubwayLegRequestCancelData = SubwayLegRequestCancelData
  { searchId :: Id FRFSSearch.FRFSSearch,
    cancellationType :: Spec.CancellationType
  }

data SubwayLegRequestGetStateData = SubwayLegRequestGetStateData
  { searchId :: Id FRFSSearch.FRFSSearch,
    riderLastPoints :: [ApiTypes.RiderLocationReq],
    journeyLeg :: DJourneyLeg.JourneyLeg
  }

data SubwayLegRequestGetInfoData = SubwayLegRequestGetInfoData
  { searchId :: Id FRFSSearch.FRFSSearch,
    journeyLeg :: DJourneyLeg.JourneyLeg,
    journeyLegs :: [DJourneyLeg.JourneyLeg]
  }

data SubwayLegRequest
  = SubwayLegRequestSearch SubwayLegRequestSearchData
  | SubwayLegRequestConfirm SubwayLegRequestConfirmData
  | SubwayLegRequestUpdate SubwayLegRequestUpdateData
  | SubwayLegRequestCancel SubwayLegRequestCancelData
  | SubwayLegRequestGetFare SubwayLegRequestGetFareData
  | SubwayLegRequestGetState SubwayLegRequestGetStateData
  | SubwayLegRequestGetInfo SubwayLegRequestGetInfoData

data SubwayLegRequestGetFareData = SubwayLegRequestGetFareData
  { startLocation :: LatLngV2,
    endLocation :: LatLngV2,
    agencyGtfsId :: Maybe Text,
    routeDetails :: [FRFSRouteDetails],
    fromArrivalTime :: Maybe UTCTime,
    merchant :: DMerchant.Merchant,
    merchantOpCity :: DMOC.MerchantOperatingCity,
    riderId :: Id DPerson.Person,
    searchReqId :: Maybe Text
  }
