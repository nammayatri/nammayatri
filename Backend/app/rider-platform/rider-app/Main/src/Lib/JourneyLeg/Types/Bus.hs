module Lib.JourneyLeg.Types.Bus where

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

data BusLegRequestSearchData = BusLegRequestSearchData
  { quantity :: Int,
    personId :: Id DPerson.Person,
    merchantId :: Id DMerchant.Merchant,
    city :: Context.City,
    journeyLeg :: DJourneyLeg.JourneyLeg,
    recentLocationId :: Maybe (Id DRecentLocation.RecentLocation)
  }

data BusLegRequestConfirmData = BusLegRequestConfirmData
  { quoteId :: Maybe (Id FRFSQuote),
    searchId :: Id FRFSSearch.FRFSSearch,
    skipBooking :: Bool,
    bookingAllowed :: Bool,
    personId :: Id DPerson.Person,
    merchantId :: Id DMerchant.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    quantity :: Maybe Int,
    childTicketQuantity :: Maybe Int
  }

data BusLegRequestUpdateData = BusLegRequestUpdateData

data BusLegRequestCancelData = BusLegRequestCancelData
  { searchId :: Id FRFSSearch.FRFSSearch,
    cancellationType :: Spec.CancellationType,
    isSkipped :: Bool
  }

data BusLegRequestIsCancellableData = BusLegRequestIsCancellableData
  { searchId :: Id FRFSSearch.FRFSSearch
  }

data BusLegRequestGetInfoData = BusLegRequestGetInfoData
  { searchId :: Id FRFSSearch.FRFSSearch,
    fallbackFare :: Maybe HighPrecMoney,
    distance :: Maybe Distance,
    duration :: Maybe Seconds,
    journeyLeg :: DJourneyLeg.JourneyLeg
  }

data BusLegRequestGetStateData = BusLegRequestGetStateData
  { searchId :: Id FRFSSearch.FRFSSearch,
    riderLastPoints :: [ApiTypes.RiderLocationReq],
    isLastCompleted :: Bool,
    movementDetected :: Bool,
    routeCodeForDetailedTracking :: Maybe Text
  }

data BusLegRequestGetFareData = BusLegRequestGetFareData
  { startLocation :: LatLngV2,
    endLocation :: LatLngV2,
    routeDetails :: [FRFSRouteDetails],
    merchant :: DMerchant.Merchant,
    riderId :: Id DPerson.Person,
    fromArrivalTime :: Maybe UTCTime,
    merchantOpCity :: DMOC.MerchantOperatingCity
  }

data BusLegRequest
  = BusLegRequestSearch BusLegRequestSearchData
  | BusLegRequestConfirm BusLegRequestConfirmData
  | BusLegRequestUpdate BusLegRequestUpdateData
  | BusLegRequestCancel BusLegRequestCancelData
  | BusLegRequestIsCancellable BusLegRequestIsCancellableData
  | BusLegRequestGetFare BusLegRequestGetFareData
  | BusLegRequestGetState BusLegRequestGetStateData
  | BusLegRequestGetInfo BusLegRequestGetInfoData
