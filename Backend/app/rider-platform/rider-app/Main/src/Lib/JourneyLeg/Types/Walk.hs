module Lib.JourneyLeg.Types.Walk where

import qualified API.Types.UI.MultimodalConfirm as ApiTypes
import qualified Domain.Types.Journey as DJourney
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.Station as Station
import qualified Domain.Types.WalkLegMultimodal as DWalkLeg
import Kernel.Prelude
import Kernel.Types.Id
import SharedLogic.Search

data WalkLegRequestSearchData = WalkLegRequestSearchData
  { journey :: DJourney.Journey,
    journeyLegData :: DJourneyLeg.JourneyLeg,
    origin :: SearchReqLocation,
    destination :: SearchReqLocation
  }

data WalkLegRequestGetStateData = WalkLegRequestGetStateData
  { walkLegId :: Id DWalkLeg.WalkLegMultimodal,
    riderLastPoints :: [ApiTypes.RiderLocationReq],
    isLastCompleted :: Bool,
    mbToStation :: Maybe Station.Station
  }

data WalkLegRequestGetInfoData = WalkLegRequestGetInfoData
  { walkLegId :: Id DWalkLeg.WalkLegMultimodal,
    journeyLeg :: DJourneyLeg.JourneyLeg,
    ignoreOldSearchRequest :: Bool
  }

data WalkLegRequestConfirmData = WalkLegRequestConfirmData

data WalkLegRequestGetFareData = WalkLegRequestGetFareData

data WalkLegRequestCancelData = WalkLegRequestCancelData
  { walkLegId :: Id DWalkLeg.WalkLegMultimodal
  }

data WalkLegRequestIsCancellableData = WalkLegRequestIsCancellableData
  { walkLegId :: Id DWalkLeg.WalkLegMultimodal
  }

data WalkLegRequestUpdateData = WalkLegRequestUpdateData

data WalkLegRequest
  = WalkLegRequestSearch WalkLegRequestSearchData
  | WalkLegRequestConfirm WalkLegRequestConfirmData
  | WalkLegRequestUpdate WalkLegRequestUpdateData
  | WalkLegRequestCancel WalkLegRequestCancelData
  | WalkLegRequestIsCancellable WalkLegRequestIsCancellableData
  | WalkLegRequestGetInfo WalkLegRequestGetInfoData
  | WalkLegRequestGetState WalkLegRequestGetStateData
  | WalkLegRequestGetFare WalkLegRequestGetFareData
