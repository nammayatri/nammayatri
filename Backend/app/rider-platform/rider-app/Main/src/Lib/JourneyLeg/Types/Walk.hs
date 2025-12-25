module Lib.JourneyLeg.Types.Walk where

import qualified API.Types.UI.MultimodalConfirm as ApiTypes
import qualified Domain.Types.Journey as DJourney
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.Person as DPerson
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
  { riderLastPoints :: [ApiTypes.RiderLocationReq],
    journeyLeg :: DJourneyLeg.JourneyLeg
  }

data WalkLegRequestGetInfoData = WalkLegRequestGetInfoData
  { journeyLeg :: DJourneyLeg.JourneyLeg,
    personId :: Id DPerson.Person
  }

data WalkLegRequestConfirmData = WalkLegRequestConfirmData

data WalkLegRequestGetFareData = WalkLegRequestGetFareData

data WalkLegRequestCancelData = WalkLegRequestCancelData
  { journeyLegId :: Id DJourneyLeg.JourneyLeg
  }

data WalkLegRequestUpdateData = WalkLegRequestUpdateData

data WalkLegRequest
  = WalkLegRequestSearch WalkLegRequestSearchData
  | WalkLegRequestConfirm WalkLegRequestConfirmData
  | WalkLegRequestUpdate WalkLegRequestUpdateData
  | WalkLegRequestCancel WalkLegRequestCancelData
  | WalkLegRequestGetInfo WalkLegRequestGetInfoData
  | WalkLegRequestGetState WalkLegRequestGetStateData
  | WalkLegRequestGetFare WalkLegRequestGetFareData
