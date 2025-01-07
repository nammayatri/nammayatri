module Lib.JourneyLeg.Types.Metro where

import qualified Domain.Types.FRFSSearch as FRFSSearch
import Kernel.External.Maps.Google.MapsClient.Types
import Kernel.Prelude
import Kernel.Types.Id

data MetroLegRequestSearchData = MetroLegRequestSearchData
  { fromStationCode :: Text,
    toStationCode :: Text,
    routeCode :: Maybe Text,
    quantity :: Int
  }

data MetroLegRequestUpdateData = MetroLegRequestUpdateData

data MetroLegRequestConfirmData = MetroLegRequestConfirmData

data MetroLegRequestCancelData = MetroLegRequestCancelData

data MetroLegRequestGetStateData = MetroLegRequestGetStateData

newtype MetroLegRequestGetInfoData = MetroLegRequestGetInfoData
  { searchId :: Id FRFSSearch.FRFSSearch
  }

data MetroLegRequest
  = MetroLegRequestSearch MetroLegRequestSearchData
  | MetroLegRequestConfirm MetroLegRequestConfirmData
  | MetroLegRequestUpdate MetroLegRequestUpdateData
  | MetroLegRequestCancel MetroLegRequestCancelData
  | MetroLegRequestGetFare MetroLegRequestGetFareData
  | MetroLegRequestGetState MetroLegRequestGetStateData
  | MetroLegRequestGetInfo MetroLegRequestGetInfoData

data MetroLegRequestGetFareData = MetroLegRequestGetFareData
  { startLocation :: LatLngV2,
    endLocation :: LatLngV2
  }
