module Lib.JourneyLeg.Types.Walk where

import qualified Domain.Types.JourneyLeg as DJourenyLeg
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.WalkLegMultimodal as DWalkLeg
import Kernel.Prelude
import Kernel.Types.Id
import SharedLogic.Search

data WalkLegRequestSearchData = WalkLegRequestSearchData
  { parentSearchReq :: DSR.SearchRequest,
    journeyLegData :: DJourenyLeg.JourneyLeg,
    origin :: SearchReqLocation,
    destination :: SearchReqLocation,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  }

newtype WalkLegRequestGetStateData = WalkLegRequestGetStateData
  { walkLegId :: Id DWalkLeg.WalkLegMultimodal
  }

newtype WalkLegRequestGetInfoData = WalkLegRequestGetInfoData
  { walkLegId :: Id DWalkLeg.WalkLegMultimodal
  }

data WalkLegRequestConfirmData = WalkLegRequestConfirmData

data WalkLegRequestGetFareData = WalkLegRequestGetFareData

data WalkLegRequestCancelData = WalkLegRequestCancelData

data WalkLegRequestUpdateData = WalkLegRequestUpdateData

data WalkLegRequest
  = WalkLegRequestSearch WalkLegRequestSearchData
  | WalkLegRequestConfirm WalkLegRequestConfirmData
  | WalkLegRequestUpdate WalkLegRequestUpdateData
  | WalkLegRequestCancel WalkLegRequestCancelData
  | WalkLegRequestGetInfo WalkLegRequestGetInfoData
  | WalkLegRequestGetState WalkLegRequestGetStateData
  | WalkLegRequestGetFare WalkLegRequestGetFareData
