module Lib.JourneyLeg.Walk where

import qualified Domain.Types.WalkLegMultimodal as DWalkLeg
import qualified Lib.JourneyModule.Types as JT
import qualified Storage.Queries.WalkLegMultimodal as QWalkLeg

data WalkLegRequestSearchData = WalkLegRequestSearchData
  { parentSearchReq :: DSR.SearchRequest,
    journeyLegData :: DJourenyLeg.JourneyLeg,
    origin :: SearchReqLocation,
    destination :: SearchReqLocation
  }

newtype WalkLegRequestGetStateData = WalkLegRequestGetStateData
  { walkLegId :: Id DWalkLeg.WalkLegMultimodal
  }

newtype WalkLegRequestGetInfoData = WalkLegRequestGetInfoData
  { walkLegId :: Id DWalkLeg.WalkLegMultimodal
  }

data WalkLegRequestGetFareData

data WalkLegRequest
  = WalkLegRequestSearch WalkLegRequestSearchData
  | WalkLegRequestConfirm WalkLegRequestConfirmData
  | WalkLegRequestUpdate WalkLegRequestUpdateData
  | WalkLegRequestCancel WalkLegRequestCancelData
  | WalkLegRequestGetInfo WalkLegRequestGetInfoData
  | WalkLegRequestGetState WalkLegRequestGetStateData
  | WalkLegRequestGetFare WalkLegRequestGetFareData

instance JourneyLeg WalkLeg m where
  search (WalkLegRequestSearch WalkLegRequestSearchData {..}) = do
    fromLocation <- buildSearchReqLoc merchantId merchantOperatingCityId origin
    toLocation <- buildSearchReqLoc merchantId merchantOperatingCityId destination
    now <- getCurrentTime
    id <- generateGUID
    let journeySearchData =
          JT.JourneySearchData
            { journeyId = journeyLegData.journeyId.getId,
              journeyLegOrder = journeyLegData.sequenceNumber,
              agency = journeyLegData.agency <&> (.name),
              skipBooking = False,
              convenienceCost = 0,
              pricingId = Nothing
            }
    let walkLeg =
          DWalkLeg.WalkLegMultimodal
            { id,
              estimatedDistance = journeyLegData.distance,
              estimatedDuration = Just journeyLegData.duration,
              fromLocation = fromLocation,
              toLocation = Just toLocation,
              journeyLegInfo = Just journeySearchData,
              riderId = parentSearchReq.riderId,
              startTime = fromMaybe now journeyLegData.fromArrivalTime,
              merchantId = Just parentSearchReq.merchantId,
              status = InPlan,
              merchantOperatingCityId = Just parentSearchReq.merchantOperatingCityId,
              createdAt = now,
              updatedAt = now
            }
    QWalkLeg.create walkLeg

  confirm (WalkLegRequestConfirm _) = return () -- return nothing

  update (WalkLegRequestUpdate _) = return ()

  cancel (WalkLegRequestCancel _) = return ()

  getState (WalkLegRequestGetState req) = do
    legData <- QWalkLeg.findById req.walkLegId
    let status = getWalkLegStatusFromWalkLeg legData
    return $ JT.JourneyLegState {status = status, currentPosition = Nothing}

  getInfo (WalkLegRequestGetInfo req) = do
    legData <- QWalkLeg.findById req.walkLegId
    return $ mkWalkLegInfoFromWalkLegData legData

  getFare (WalkLegRequestGetFare _) = do
    return $
      JT.GetFareResponse
        { estimatedMinFare = HighPrecMoney {getHighPrecMoney = 0},
          estimatedMaxFare = HighPrecMoney {getHighPrecMoney = 0}
        }
