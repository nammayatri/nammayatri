{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.JourneyLeg.Metro where

import qualified API.Types.UI.FRFSTicketService as API
import qualified BecknV2.FRFS.Enums as Spec
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import qualified Domain.Types.TicketBookingService as TBS
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Types as JPT
import Lib.JourneyLeg.Types.Metro
import qualified Lib.JourneyModule.Types as JT
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.FRFSTicketBooking as QTBooking

mapServiceStatusToJourneyLegStatus :: TBS.ServiceStatus -> JT.JourneyLegStatus
mapServiceStatusToJourneyLegStatus status = case status of
  TBS.Pending -> JT.InPlan -- Indicates the service is yet to start planning.
  TBS.Failed -> JT.Cancelled -- Indicates a failure in the service.
  TBS.Confirmed -> JT.Booked -- Indicates the service has been confirmed/booked.
  TBS.Verified -> JT.Assigning -- Indicates the service is verified and assigning resources.
  TBS.Cancelled -> JT.Cancelled -- Indicates the service has been cancelled.

instance JT.JourneyLeg MetroLegRequest m where
  search (MetroLegRequestSearch MetroLegRequestSearchData {..}) = do
    let journeySearchData =
          JPT.JourneySearchData
            { journeyId = journeyLeg.journeyId.getId,
              journeyLegOrder = journeyLeg.sequenceNumber,
              agency = journeyLeg.agency <&> (.name),
              skipBooking = False,
              convenienceCost = 0,
              pricingId = Nothing
            }
    frfsSearchReq <- buildFRFSSearchReq (Just journeySearchData)
    let colorName = journeyLeg.routeDetails >>= (.shortName)
    let routeColorCode = journeyLeg.routeDetails >>= (.color)
    let frequency = Just 300 -- hard code to 5 seconds
    res <- FRFSTicketService.postFrfsSearchHandler (Just personId, merchantId) (Just city) Spec.METRO frfsSearchReq Nothing Nothing colorName routeColorCode frequency
    return $ JT.SearchResponse {id = res.searchId.getId}
    where
      buildFRFSSearchReq journeySearchData = do
        fromStationCode <- journeyLeg.fromStopDetails >>= (.stopCode) & fromMaybeM (InvalidRequest "From station code not found")
        toStationCode <- journeyLeg.toStopDetails >>= (.stopCode) & fromMaybeM (InvalidRequest "To station code not found")
        let routeCode = Nothing
        return $ API.FRFSSearchAPIReq {..}
  search _ = throwError (InternalError "Not supported")

  confirm (MetroLegRequestConfirm req) = do
    unless req.skipBooking $ do
      void $ FRFSTicketService.postFrfsQuoteConfirm (Just req.personId, req.merchantId) req.quoteId
  confirm _ = throwError (InternalError "Not supported")

  update (MetroLegRequestUpdate _) = return ()
  update _ = throwError (InternalError "Not supported")

  cancel (MetroLegRequestCancel _) = return ()
  cancel _ = throwError (InternalError "Not supported")

  getState (MetroLegRequestGetState req) = do
    mbMetroBooking <- B.runInReplica $ QTBooking.findBySearchId req.searchId
    case mbMetroBooking of
      Just metroBooking -> do
        return $ JT.JourneyLegState {status = JT.InPlan, currentPosition = Nothing, legOrder = metroBooking.journeyLegOrder}
      Nothing -> do
        -- search <- QFRFSSearch.findById req.searchId >>= fromMaybeM (InvalidRequest "Invalid search id")
        -- return $ JT.JourneyLegState {status = JT.InPlan, currentPosition = Nothing, legOrder = fromMaybe 0 search.journeyLegInfo.journeyLegOrder}
        return $ JT.JourneyLegState {status = JT.InPlan, currentPosition = Nothing, legOrder = Nothing}
  getState _ = throwError (InternalError "Not supported")

  getInfo (MetroLegRequestGetInfo req) = do
    -- TODO: No booking for metro, we can handle for bookings once booking is there
    searchReq <- QFRFSSearch.findById req.searchId >>= fromMaybeM (SearchRequestNotFound req.searchId.getId)
    JT.mkLegInfoFromFrfsSearchRequest searchReq
  getInfo _ = throwError (InternalError "Not supported")

  getFare (MetroLegRequestGetFare _) = do
    return (Just $ JT.GetFareResponse {estimatedMinFare = HighPrecMoney {getHighPrecMoney = 10}, estimatedMaxFare = HighPrecMoney {getHighPrecMoney = 10}})
  getFare _ = throwError (InternalError "Not supported")
