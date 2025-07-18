module Lib.JourneyLeg.Types.Subway where

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
import Kernel.Utils.Common

data SubwayLegRequestSearchData = SubwayLegRequestSearchData
  { quantity :: Int,
    personId :: Id DPerson.Person,
    merchantId :: Id DMerchant.Merchant,
    city :: Context.City,
    journeyLeg :: DJourneyLeg.JourneyLeg,
    recentLocationId :: Maybe (Id DRecentLocation.RecentLocation)
  }

data SubwayLegRequestUpdateData = SubwayLegRequestUpdateData

data SubwayLegRequestConfirmData = SubwayLegRequestConfirmData
  { quoteId :: Maybe (Id FRFSQuote),
    searchId :: Id FRFSSearch.FRFSSearch,
    skipBooking :: Bool,
    bookingAllowed :: Bool,
    personId :: Id DPerson.Person,
    merchantId :: Id DMerchant.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    crisSdkResponse :: Maybe ApiTypes.CrisSdkResponse,
    quantity :: Maybe Int,
    childTicketQuantity :: Maybe Int
  }

data SubwayLegRequestCancelData = SubwayLegRequestCancelData
  { searchId :: Id FRFSSearch.FRFSSearch,
    cancellationType :: Spec.CancellationType,
    isSkipped :: Bool
  }

data SubwayLegRequestIsCancellableData = SubwayLegRequestIsCancellableData

data SubwayLegRequestGetStateData = SubwayLegRequestGetStateData
  { searchId :: Id FRFSSearch.FRFSSearch,
    riderLastPoints :: [ApiTypes.RiderLocationReq],
    isLastCompleted :: Bool
  }

data SubwayLegRequestGetInfoData = SubwayLegRequestGetInfoData
  { searchId :: Id FRFSSearch.FRFSSearch,
    fallbackFare :: Maybe HighPrecMoney,
    distance :: Maybe Distance,
    duration :: Maybe Seconds,
    journeyLeg :: DJourneyLeg.JourneyLeg,
    ignoreOldSearchRequest :: Bool,
    startTime :: Maybe UTCTime
  }

data SubwayLegRequest
  = SubwayLegRequestSearch SubwayLegRequestSearchData
  | SubwayLegRequestConfirm SubwayLegRequestConfirmData
  | SubwayLegRequestUpdate SubwayLegRequestUpdateData
  | SubwayLegRequestCancel SubwayLegRequestCancelData
  | SubwayLegRequestIsCancellable SubwayLegRequestIsCancellableData
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
    riderId :: Id DPerson.Person
  }
