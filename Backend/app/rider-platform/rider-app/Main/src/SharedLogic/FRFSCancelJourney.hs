module SharedLogic.FRFSCancelJourney where

import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.Journey as DJourney
import Environment
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Lib.JourneyModule.State.Types as JMState
import qualified Lib.JourneyModule.State.Utils as JMStateUtils
import SharedLogic.FRFSUtils (getJourneyIdFromBooking)
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.JourneyExtra as QJourneyExtra
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import Tools.Error

cancelJourney :: DFRFSTicketBooking.FRFSTicketBooking -> Flow ()
cancelJourney booking = do
  mbJourneyId <- getJourneyIdFromBooking booking
  now <- getCurrentTime
  whenJust mbJourneyId $ \journeyId -> do
    legs <- QJourneyLeg.getJourneyLegs journeyId
    forM_ legs $ \journeyLeg -> do
      mapM_
        ( \rd -> do
            JMStateUtils.setJourneyLegTrackingStatus journeyLeg rd.subLegOrder JMState.Finished now
            -- Mirror the subLegOrder 0 → 1 workaround from markLegStatus
            whenJust rd.subLegOrder $ \subLegOrder ->
              when (subLegOrder == 0) $
                JMStateUtils.setJourneyLegTrackingStatus journeyLeg (Just 1) JMState.Finished now
        )
        journeyLeg.routeDetails
    journey <- QJourney.findByPrimaryKey journeyId >>= fromMaybeM (JourneyNotFound journeyId.getId)
    when (DJourney.CANCELLED > journey.status) $
      QJourneyExtra.updateStatusAndEndTime DJourney.CANCELLED journeyId
