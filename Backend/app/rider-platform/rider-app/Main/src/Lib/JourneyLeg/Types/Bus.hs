module Lib.JourneyLeg.Types.Bus where

import Kernel.External.Maps.Google.MapsClient.Types

data BusLegRequestSearchData = BusLegRequestSearchData

data BusLegRequestConfirmData = BusLegRequestConfirmData

data BusLegRequestUpdateData = BusLegRequestUpdateData

data BusLegRequestCancelData = BusLegRequestCancelData

data BusLegRequestGetInfoData = BusLegRequestGetInfoData

data BusLegRequestGetStateData = BusLegRequestGetStateData

data BusLegRequestGetFareData = BusLegRequestGetFareData
  { startLocation :: LatLngV2,
    endLocation :: LatLngV2
  }

data BusLegRequest
  = BusLegRequestSearch BusLegRequestSearchData
  | BusLegRequestConfirm BusLegRequestConfirmData
  | BusLegRequestUpdate BusLegRequestUpdateData
  | BusLegRequestCancel BusLegRequestCancelData
  | BusLegRequestGetFare BusLegRequestGetFareData
  | BusLegRequestGetState BusLegRequestGetStateData
  | BusLegRequestGetInfo BusLegRequestGetInfoData
