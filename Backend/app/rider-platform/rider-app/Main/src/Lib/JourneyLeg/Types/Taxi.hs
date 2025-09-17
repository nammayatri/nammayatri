module Lib.JourneyLeg.Types.Taxi where

import qualified API.Types.UI.MultimodalConfirm as ApiTypes
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.Estimate as DE
import qualified Domain.Types.Extra.Ride as DR
import qualified Domain.Types.Journey as DJourney
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DR
import qualified Domain.Types.SearchRequest as DSR
import Kernel.External.Maps.Google.MapsClient.Types
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.JourneyModule.Types as JL
import SharedLogic.Search

data TaxiLegRequestSearchData = TaxiLegRequestSearchData
  { journey :: DJourney.Journey,
    journeyLegData :: DJourneyLeg.JourneyLeg,
    origin :: SearchReqLocation,
    stops :: [SearchReqLocation],
    multimodalSearchRequestId :: Maybe Text,
    upsertJourneyLegAction :: forall m r c. JL.SearchRequestFlow m r c => Text -> m ()
  }

data TaxiLegRequestConfirmData = TaxiLegRequestConfirmData
  { bookLater :: Bool,
    forcedBooked :: Bool,
    startTime :: UTCTime,
    personId :: Id DP.Person,
    merchantId :: Id DM.Merchant,
    searchId :: Text,
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
  { searchRequestId :: Id DSR.SearchRequest,
    reasonCode :: SCR.CancellationReasonCode,
    -- reasonStage :: SCR.CancellationStage,
    additionalInfo :: Maybe Text,
    reallocate :: Maybe Bool,
    blockOnCancellationRate :: Maybe Bool,
    cancellationSource :: SBCR.CancellationSource,
    cancelEstimateId :: Maybe (Id DE.Estimate),
    journeyLeg :: DJourneyLeg.JourneyLeg
  }

data TaxiLegRequestGetInfoData = TaxiLegRequestGetInfoData
  { searchId :: Id DSR.SearchRequest,
    journeyLeg :: DJourneyLeg.JourneyLeg
  }

data TaxiLegRequestGetStateData = TaxiLegRequestGetStateData
  { searchId :: Id DSR.SearchRequest,
    riderLastPoints :: [ApiTypes.RiderLocationReq],
    journeyLeg :: DJourneyLeg.JourneyLeg
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
  | TaxiLegRequestGetInfo TaxiLegRequestGetInfoData
  | TaxiLegRequestGetState TaxiLegRequestGetStateData
  | TaxiLegRequestGetFare TaxiLegRequestGetFareData
