{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.JourneyLeg.Metro where

import qualified API.Types.UI.FRFSTicketService as API
import qualified API.Types.UI.FRFSTicketService as DFRFSTypes
import qualified BecknV2.FRFS.Enums as Spec
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import qualified Domain.Types.TicketBookingService as TBS
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Types as JPT
import Lib.JourneyLeg.Types.Metro
import qualified Lib.JourneyModule.Types as JT
import qualified Storage.Queries.FRFSSearch as QFRFSSearch

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
    let colorName = Nothing -- journeyLeg.routeDetails >>= (.routeColorName)
    let routeColorCode = Nothing -- journeyLeg.routeDetails >>= (.routeColorCode)
    let frequency = Nothing -- journeyLeg.routeDetails >>= (.frequency)
    void $ FRFSTicketService.postFrfsSearchHandler (Just personId, merchantId) (Just city) Spec.METRO frfsSearchReq Nothing Nothing colorName routeColorCode frequency
    where
      -- buildFRFSSearchReq :: Maybe JPT.JourneySearchData -> m DFRFSTypes.FRFSSearchAPIReq
      buildFRFSSearchReq journeySearchData = do
        fromStationCode <- journeyLeg.fromStopDetails >>= (.stopCode) & fromMaybeM (InvalidRequest "From station code not found")
        toStationCode <- journeyLeg.toStopDetails >>= (.stopCode) & fromMaybeM (InvalidRequest "To station code not found")
        let routeCode = Nothing
        return $ DFRFSTypes.FRFSSearchAPIReq {..}
  search _ = throwError (InternalError "Not supported")

  confirm (MetroLegRequestConfirm _) = return ()
  confirm _ = throwError (InternalError "Not supported")

  update (MetroLegRequestUpdate _) = return ()
  update _ = throwError (InternalError "Not supported")

  cancel (MetroLegRequestCancel _) = return ()
  cancel _ = throwError (InternalError "Not supported")

  getState (MetroLegRequestGetState _) = return $ JT.JourneyLegState {status = JT.InPlan, currentPosition = Nothing}
  getState _ = throwError (InternalError "Not supported")

  getInfo (MetroLegRequestGetInfo req) = do
    -- TODO: No booking for metro, we can handle for bookings once booking is there
    searchReq <- QFRFSSearch.findById req.searchId >>= fromMaybeM (SearchRequestNotFound req.searchId.getId)
    JT.mkLegInfoFromFrfsSearchRequest searchReq
  getInfo _ = throwError (InternalError "Not supported")

  getFare (MetroLegRequestGetFare _) = do
    return (Just $ JT.GetFareResponse {estimatedMinFare = HighPrecMoney {getHighPrecMoney = 10}, estimatedMaxFare = HighPrecMoney {getHighPrecMoney = 10}})
  getFare _ = throwError (InternalError "Not supported")
