module Lib.JourneyLeg.Types.Metro where

import qualified API.Types.UI.MultimodalConfirm as ApiTypes
import Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSSearch as FRFSSearch
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DPerson
import Kernel.External.Maps.Google.MapsClient.Types
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common

data MetroLegRequestSearchData = MetroLegRequestSearchData
  { quantity :: Int,
    personId :: Id DPerson.Person,
    merchantId :: Id DMerchant.Merchant,
    city :: Context.City,
    journeyLeg :: DJourneyLeg.JourneyLeg
  }

data MetroLegRequestUpdateData = MetroLegRequestUpdateData

data MetroLegRequestConfirmData = MetroLegRequestConfirmData
  { quoteId :: Maybe (Id FRFSQuote),
    skipBooking :: Bool,
    bookingAllowed :: Bool,
    personId :: Id DPerson.Person,
    merchantId :: Id DMerchant.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity
  }

data MetroLegRequestCancelData = MetroLegRequestCancelData

data MetroLegRequestIsCancellableData = MetroLegRequestIsCancellableData

data MetroLegRequestGetStateData = MetroLegRequestGetStateData
  { searchId :: Id FRFSSearch.FRFSSearch,
    riderLastPoints :: [ApiTypes.RiderLocationReq],
    isLastJustCompleted :: Bool
  }

data MetroLegRequestGetInfoData = MetroLegRequestGetInfoData
  { searchId :: Id FRFSSearch.FRFSSearch,
    fallbackFare :: Maybe HighPrecMoney
  }

data MetroLegRequest
  = MetroLegRequestSearch MetroLegRequestSearchData
  | MetroLegRequestConfirm MetroLegRequestConfirmData
  | MetroLegRequestUpdate MetroLegRequestUpdateData
  | MetroLegRequestCancel MetroLegRequestCancelData
  | MetroLegRequestIsCancellable MetroLegRequestIsCancellableData
  | MetroLegRequestGetFare MetroLegRequestGetFareData
  | MetroLegRequestGetState MetroLegRequestGetStateData
  | MetroLegRequestGetInfo MetroLegRequestGetInfoData

data MetroLegRequestGetFareData = MetroLegRequestGetFareData
  { startLocation :: LatLngV2,
    endLocation :: LatLngV2
  }
