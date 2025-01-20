{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.JourneyLeg.Bus where

import qualified API.Types.UI.FRFSTicketService as API
import qualified BecknV2.FRFS.Enums as Spec
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Common.FRFS as CFRFS
import qualified Lib.JourneyLeg.Types as JPT
import Lib.JourneyLeg.Types.Bus
import qualified Lib.JourneyModule.Types as JT
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.FRFSTicketBooking as QTBooking

instance JT.JourneyLeg BusLegRequest m where
  search (BusLegRequestSearch BusLegRequestSearchData {..}) = do
    let journeySearchData =
          JPT.JourneySearchData
            { journeyId = journeyLeg.journeyId.getId,
              journeyLegOrder = journeyLeg.sequenceNumber,
              agency = journeyLeg.agency <&> (.name),
              skipBooking = False,
              isDeleted = Just False,
              convenienceCost = 0,
              pricingId = Nothing
            }
    frfsSearchReq <- buildFRFSSearchReq (Just journeySearchData)
    let colorName = journeyLeg.routeDetails >>= (.shortName)
    let routeColorCode = journeyLeg.routeDetails >>= (.color)
    let frequency = Just 300
    res <- FRFSTicketService.postFrfsSearchHandler (Just personId, merchantId) (Just city) Spec.BUS frfsSearchReq Nothing Nothing colorName routeColorCode frequency
    return $ JT.SearchResponse {id = res.searchId.getId}
    where
      buildFRFSSearchReq journeySearchData = do
        fromStationCode <- journeyLeg.fromStopDetails >>= (.stopCode) & fromMaybeM (InvalidRequest "From station code not found")
        toStationCode <- journeyLeg.toStopDetails >>= (.stopCode) & fromMaybeM (InvalidRequest "To station code not found")
        let routeCode = Nothing
        return $ API.FRFSSearchAPIReq {..}
  search _ = throwError (InternalError "Not supported")

  confirm (BusLegRequestConfirm req) = do
    when (not req.skipBooking && req.bookingAllowed) $ do
      quoteId <- req.quoteId & fromMaybeM (InvalidRequest "You can't confirm bus before getting the fare")
      void $ FRFSTicketService.postFrfsQuoteConfirm (Just req.personId, req.merchantId) quoteId
  confirm _ = throwError (InternalError "Not supported")

  update (BusLegRequestUpdate _) = do
    -- let customerLocation = get eta between user location and bus station
    -- let busLocation = get eta between bus and bus station
    -- let threshold = 50
    -- mark status with respect to bus -  Ontime, Departed, Delayed, Arriving, Finishing, Completed
    -- mark status with respect to user -  AtRiskOfMissing, Missed
    throwError (InternalError "Not supported")
  update _ = throwError (InternalError "Not supported")

  cancel (BusLegRequestCancel legData) = do
    mbBusBooking <- B.runInReplica $ QTBooking.findBySearchId legData.searchId
    case mbBusBooking of
      Just busBooking -> do
        QTBooking.updateIsCancelled busBooking.id (Just True)
      Nothing -> do
        QFRFSSearch.updateIsCancelled legData.searchId (Just True)
  cancel _ = throwError (InternalError "Not supported")

  getState (BusLegRequestGetState req) = CFRFS.getState req.searchId req.riderLastPoints req.isLastJustCompleted
  getState _ = throwError (InternalError "Not supported")

  getInfo (BusLegRequestGetInfo req) = CFRFS.getInfo req.searchId req.fallbackFare
  getInfo _ = throwError (InternalError "Not supported")

  getFare (BusLegRequestGetFare _) = do
    return (Just $ JT.GetFareResponse {estimatedMinFare = HighPrecMoney {getHighPrecMoney = 20}, estimatedMaxFare = HighPrecMoney {getHighPrecMoney = 20}})
  getFare _ = throwError (InternalError "Not supported")
