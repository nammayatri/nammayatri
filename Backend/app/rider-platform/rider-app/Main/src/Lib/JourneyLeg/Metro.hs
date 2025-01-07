{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.JourneyLeg.Metro where

import qualified Domain.Types.TicketBookingService as TBS
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.JourneyLeg.Types.Metro
import qualified Lib.JourneyModule.Types as JT
import qualified Storage.Queries.FRFSSearch as QFRFSSearch

-- import qualified Domain.Types.FRFSSearch as DFRFSSearch
-- import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
-- import qualified API.Types.UI.FRFSTicketService as API
-- import qualified Storage.Queries.JourneyLeg as QJourneyLeg
-- import qualified BecknV2.FRFS.APIs as Spec
-- import qualified BecknV2.FRFS.Enums as Spec
-- import qualified BecknV2.FRFS.Types as Spec
-- import qualified Lib.JourneyLeg.Types as JPT
-- import qualified API.Types.UI.FRFSTicketService as DFRFSTypes

mapServiceStatusToJourneyLegStatus :: TBS.ServiceStatus -> JT.JourneyLegStatus
mapServiceStatusToJourneyLegStatus status = case status of
  TBS.Pending -> JT.InPlan -- Indicates the service is yet to start planning.
  TBS.Failed -> JT.Cancelled -- Indicates a failure in the service.
  TBS.Confirmed -> JT.Booked -- Indicates the service has been confirmed/booked.
  TBS.Verified -> JT.Assigning -- Indicates the service is verified and assigning resources.
  TBS.Cancelled -> JT.Cancelled -- Indicates the service has been cancelled.

instance JT.JourneyLeg MetroLegRequest m where
  search (MetroLegRequestSearch _) = return ()
  -- let frfsSearchReq = buildFRFSSearchReq req.fromStationCode req.toStationCode req.routeCode 1 Nothing
  -- journeyLegInfo <- QJourneyLeg.findByPrimaryKey journeyLegId >>= fromMaybeM (JourneyLegIdNotFound journeyLegId)
  -- res <- FRFSTicketService.postFrfsSearchHandler (Just personId, merchantId) (Just merchantOperatingCity.city) Spec.METRO frfsSearchReq Nothing Nothing journeyLeg.routeDetails.routeColorName  journeyLeg.routeDetails.routeColorCode journeyLeg.routeDetails.frequency  --(Just partnerOrg.orgId)
  -- return $ API.FRFSSearchAPIRes res
  -- where
  --   buildFRFSSearchReq :: Text -> Text -> Maybe Text -> Int -> Maybe JPT.JourneySearchData -> DFRFSTypes.FRFSSearchAPIReq
  --   buildFRFSSearchReq fromStationCode toStationCode routeCode quantity journeySearchData = DFRFSTypes.FRFSSearchAPIReq {..}
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
    return JT.GetFareResponse {estimatedMinFare = HighPrecMoney {getHighPrecMoney = 10}, estimatedMaxFare = HighPrecMoney {getHighPrecMoney = 10}}
  getFare _ = throwError (InternalError "Not supported")
