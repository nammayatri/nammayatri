{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.JourneyLeg.Metro where

import qualified API.Types.UI.FRFSTicketService as API
import qualified BecknV2.FRFS.Enums as Spec
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Common.FRFS as CFRFS
import qualified Lib.JourneyLeg.Types as JPT
import Lib.JourneyLeg.Types.Metro
import qualified Lib.JourneyModule.Types as JT
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.FRFSTicketBooking as QTBooking

instance JT.JourneyLeg MetroLegRequest m where
  search (MetroLegRequestSearch MetroLegRequestSearchData {..}) = do
    let journeySearchData =
          JPT.JourneySearchData
            { journeyId = journeyLeg.journeyId.getId,
              journeyLegOrder = journeyLeg.sequenceNumber,
              agency = journeyLeg.agency <&> (.name),
              skipBooking = False,
              convenienceCost = 0,
              pricingId = Nothing,
              isDeleted = Just False
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
    when (not req.skipBooking && req.bookingAllowed) $ do
      quoteId <- req.quoteId & fromMaybeM (InvalidRequest "You can't confrim metro before getting the fare")
      void $ FRFSTicketService.postFrfsQuoteConfirm (Just req.personId, req.merchantId) quoteId
  confirm _ = throwError (InternalError "Not supported")

  update (MetroLegRequestUpdate _) = return ()
  update _ = throwError (InternalError "Not supported")

  cancel (MetroLegRequestCancel legData) = do
    mbMetroBooking <- B.runInReplica $ QTBooking.findBySearchId legData.searchId
    case mbMetroBooking of
      Just metroBooking -> do
        QTBooking.updateIsCancelled metroBooking.id (Just True)
      Nothing -> do
        QFRFSSearch.updateIsCancelled legData.searchId (Just True)
  cancel _ = throwError (InternalError "Not supported")

  getState (MetroLegRequestGetState req) = CFRFS.getState req.searchId req.riderLastPoints req.isLastJustCompleted
  getState _ = throwError (InternalError "Not supported")

  getInfo (MetroLegRequestGetInfo req) = CFRFS.getInfo req.searchId req.fallbackFare
  getInfo _ = throwError (InternalError "Not supported")

  getFare (MetroLegRequestGetFare _) = do
    return (Just $ JT.GetFareResponse {estimatedMinFare = HighPrecMoney {getHighPrecMoney = 10}, estimatedMaxFare = HighPrecMoney {getHighPrecMoney = 10}})
  getFare _ = throwError (InternalError "Not supported")
