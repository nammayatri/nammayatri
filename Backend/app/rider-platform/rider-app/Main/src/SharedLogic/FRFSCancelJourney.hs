module SharedLogic.FRFSCancelJourney where

import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import Environment
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Types as JL
import qualified Lib.JourneyModule.Base as JM
import qualified Lib.JourneyModule.State.Types as JMState
import SharedLogic.FRFSUtils (getJourneyIdFromBooking)
import qualified Storage.Queries.JourneyLeg as QJourneyLeg

cancelJourney :: DFRFSTicketBooking.FRFSTicketBooking -> Flow ()
cancelJourney booking = do
  mbJourneyId <- getJourneyIdFromBooking booking
  now <- getCurrentTime
  whenJust mbJourneyId $ \journeyId -> do
    legs <- QJourneyLeg.getJourneyLegs journeyId
    forM_ legs $ \journeyLeg -> do
      mapM_ (\rd -> JM.markLegStatus (Just JL.Cancelled) (Just JMState.Finished) journeyLeg rd.subLegOrder now) journeyLeg.routeDetails
    journey <- JM.getJourney journeyId
    updatedLegStatus <- JM.getAllLegsStatus journey Nothing
    JM.checkAndMarkTerminalJourneyStatus journey updatedLegStatus
