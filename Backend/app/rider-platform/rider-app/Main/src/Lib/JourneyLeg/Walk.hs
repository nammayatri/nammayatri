{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.JourneyLeg.Walk where

import Domain.Types.Trip as DTrip
import qualified Domain.Types.WalkLegMultimodal as DWalkLeg
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Types as JLT
import Lib.JourneyLeg.Types.Walk
import Lib.JourneyModule.Location
import qualified Lib.JourneyModule.Types as JT
import SharedLogic.Search
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.WalkLegMultimodal as QWalkLeg

instance JT.JourneyLeg WalkLegRequest m where
  search (WalkLegRequestSearch WalkLegRequestSearchData {..}) = do
    fromLocation <- buildSearchReqLoc parentSearchReq.merchantId parentSearchReq.merchantOperatingCityId origin
    toLocation <- buildSearchReqLoc parentSearchReq.merchantId parentSearchReq.merchantOperatingCityId destination
    now <- getCurrentTime
    id <- generateGUID
    let journeySearchData =
          JLT.JourneySearchData
            { journeyId = journeyLegData.journeyId.getId,
              journeyLegOrder = journeyLegData.sequenceNumber,
              agency = journeyLegData.agency <&> (.name),
              skipBooking = False,
              convenienceCost = 0,
              pricingId = Nothing,
              isDeleted = Just False
            }
    let walkLeg =
          DWalkLeg.WalkLegMultimodal
            { id,
              estimatedDistance = journeyLegData.distance,
              estimatedDuration = journeyLegData.duration,
              fromLocation = fromLocation,
              toLocation = Just toLocation,
              journeyLegInfo = Just journeySearchData,
              riderId = parentSearchReq.riderId,
              startTime = fromMaybe now journeyLegData.fromArrivalTime,
              merchantId = parentSearchReq.merchantId,
              status = DWalkLeg.InPlan,
              merchantOperatingCityId = parentSearchReq.merchantOperatingCityId,
              createdAt = now,
              updatedAt = now
            }
    QWalkLeg.createWalkLeg walkLeg
    return $ JT.SearchResponse {id = id.getId}
  search _ = throwError (InternalError "Not supported")

  confirm (WalkLegRequestConfirm _) = return ()
  confirm _ = throwError (InternalError "Not supported")

  update (WalkLegRequestUpdate _) = return ()
  update _ = throwError (InternalError "Not supported")

  cancel (WalkLegRequestCancel legData) = do
    QWalkLeg.updateIsCancelled legData.walkLegId (Just True)
    QJourneyLeg.updateIsDeleted (Just True) (Just legData.walkLegId.getId)
  cancel _ = throwError (InternalError "Not supported")

  isCancellable ((WalkLegRequestIsCancellable legData)) = do
    walkLeg <- QWalkLeg.findById legData.walkLegId >>= fromMaybeM (InvalidRequest "WalkLeg Data not found")
    case walkLeg.status of
      DWalkLeg.InPlan -> return $ JT.IsCancellableResponse {canCancel = True}
      DWalkLeg.Ongoing -> return $ JT.IsCancellableResponse {canCancel = True}
      _ -> return $ JT.IsCancellableResponse {canCancel = False}
  isCancellable _ = throwError (InternalError "Not Supported")

  getState (WalkLegRequestGetState req) = do
    legData <- QWalkLeg.findById req.walkLegId >>= fromMaybeM (InvalidRequest "WalkLeg Data not found")
    journeyLegInfo <- legData.journeyLegInfo & fromMaybeM (InvalidRequest "WalkLeg journey legInfo data missing")
    toLocation <- legData.toLocation & fromMaybeM (InvalidRequest "ToLocation of walkleg journey data is missing")
    let status = JT.getWalkLegStatusFromWalkLeg legData journeyLegInfo
    let (statusChanged, newStatus) = updateJourneyLegStatus DTrip.Walk req.riderLastPoints (locationToLatLng toLocation) status req.isLastCompleted
    when statusChanged $ do
      let walkLegStatus = JT.castWalkLegStatusFromLegStatus newStatus
      QWalkLeg.updateStatus walkLegStatus req.walkLegId
    return $
      JT.JourneyLegState
        { status = newStatus,
          userPosition = (.latLong) <$> listToMaybe req.riderLastPoints,
          vehiclePosition = Nothing,
          nextStop = Nothing,
          nextStopTravelDistance = Nothing,
          nextStopTravelTime = Nothing,
          legOrder = journeyLegInfo.journeyLegOrder,
          statusChanged,
          mode = DTrip.Walk
        }
  getState _ = throwError (InternalError "Not supported")

  getInfo (WalkLegRequestGetInfo req) = do
    legData <- QWalkLeg.findById req.walkLegId >>= fromMaybeM (InvalidRequest "WalkLeg Data not found")
    JT.mkWalkLegInfoFromWalkLegData legData
  getInfo _ = throwError (InternalError "Not supported")

  getFare (WalkLegRequestGetFare _) = do
    return $
      Just $
        JT.GetFareResponse
          { estimatedMinFare = HighPrecMoney {getHighPrecMoney = 0},
            estimatedMaxFare = HighPrecMoney {getHighPrecMoney = 0}
          }
  getFare _ = throwError (InternalError "Not supported")
