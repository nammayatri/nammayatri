module Lib.JourneyLeg.Types.Taxi where

import qualified API.Types.UI.MultimodalConfirm as ApiTypes
import qualified Domain.Types.Estimate as DE
import qualified Domain.Types.Extra.Ride as DR
import qualified Domain.Types.JourneyLeg as DJourenyLeg
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DR
import qualified Domain.Types.SearchRequest as DSR
import Kernel.External.Maps.Google.MapsClient.Types
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Search

data TaxiLegRequestSearchData = TaxiLegRequestSearchData
  { parentSearchReq :: DSR.SearchRequest,
    journeyLegData :: DJourenyLeg.JourneyLeg,
    origin :: SearchReqLocation,
    stops :: [SearchReqLocation]
  }

data TaxiLegRequestConfirmData = TaxiLegRequestConfirmData
  { skipBooking :: Bool,
    startTime :: UTCTime,
    personId :: Id DP.Person,
    merchantId :: Id DM.Merchant,
    estimateId :: Maybe (Id DE.Estimate)
  }

data ChangeServiceTierData = ChangeServiceTierData
  { searchRequestId :: Id DSR.SearchRequest,
    estimateId :: Id DE.Estimate
  }

data EditLocationRequest = EditLocationRequest
  { origin :: Maybe DR.EditLocation,
    destination :: Maybe DR.EditLocation,
    personId :: Id DP.Person,
    merchantId :: Id DM.Merchant,
    rideId :: Id DR.Ride
  }

data TaxiLegRequestUpdateData = EditLocation EditLocationRequest | ChangeServiceTier ChangeServiceTierData

data TaxiLegRequestCancelData = TaxiLegRequestCancelData

data TaxiLegRequestIsCancellableData = TaxiLegRequestIsCancellableData

newtype TaxiLegRequestGetInfoData = TaxiLegRequestGetInfoData
  { searchId :: Id DSR.SearchRequest
  }

data TaxiLegRequestGetStateData = TaxiLegRequestGetStateData
  { searchId :: Id DSR.SearchRequest,
    riderLastPoints :: [ApiTypes.RiderLocationReq],
    isLastJustCompleted :: Bool
  }

data TaxiLegRequestGetFareData = TaxiLegRequestGetFareData
  { startLocation :: LatLngV2,
    endLocation :: LatLngV2,
    distance :: Distance,
    duration :: Seconds,
    merchant :: DM.Merchant,
    merchantOpCity :: DMOC.MerchantOperatingCity
  }

data TaxiLegRequest
  = TaxiLegRequestSearch TaxiLegRequestSearchData
  | TaxiLegRequestConfirm TaxiLegRequestConfirmData
  | TaxiLegRequestUpdate TaxiLegRequestUpdateData
  | TaxiLegRequestCancel TaxiLegRequestCancelData
  | TaxiLegRequestIsCancellable TaxiLegRequestIsCancellableData
  | TaxiLegRequestGetInfo TaxiLegRequestGetInfoData
  | TaxiLegRequestGetState TaxiLegRequestGetStateData
  | TaxiLegRequestGetFare TaxiLegRequestGetFareData
