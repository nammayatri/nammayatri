module Lib.JourneyModule.Base where

import qualified Kernel.External.MultiModal.Interface as ExternalInterface
import qualified Storage.Journey.Queries as JQ
import qualified Domain.Types.Journey as DJ
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Lib.JourneyModule.Types

mkJourneyLeg :: JourneyLeg a m => ExternalInterface.MultiModalLeg -> m a
mkJourneyLeg multimodalLeg = do
  case multimodalLeg.mode of
    Bus -> BusLeg multimodalLeg
    MetroRail -> MetroLeg multimodalLeg
    Walk -> WalkLeg multimodalLeg
    Unspecified -> TaxiLeg multimodalLeg

init :: JourneyLeg leg m => JourneyInitData leg -> m Journey
init journeyReq = do
  journey = mkJourney journeyReq
  JQ.create journey -- it will internally create journey leg entries as well

getJourney :: Id Journey -> m Journey
getJourney id = JQ.findById id

getAllLegs :: Id Journey -> m LegInfo
getAllLegs journeyId = do
  taxiLegs <- QSearchRequest.findAllByJourneyId journeyId >>= mapM mkLegInfoFromSearchRequest
  publicTransportLegs <- QFRFSSearch.findAllByJourneyId journeyId >>= mapM mkLegInfoFromFrfsSearchRequest
  return $ sortBy (.order) (taxiLegs <> publicTransportLegs)

startJourney :: Id Journey -> ConfirmReq -> m () -- confirm request
startJourney journeyId = do
  allLegs <- getAllLegs journeyId
  mapM (JL.confirm . mkConfirmReq) allLegs

addLeg :: JourneyLeg leg m => leg -> m ()
addLeg leg = JL.search leg -- output could be the search request

getCurrentLeg :: JourneyLeg leg m => Journey -> m leg
getCurrentLeg
  -- get the current leg based on status
  -- loop through all legs and based on getState
  journeyLegs <- getAllLegs journeyId
  let currentLeg = find (\leg -> notElem leg.status [completedStatus]) journeyLegs
  return currentLeg

getRemainingLegs :: JourneyLeg leg m => Id Journey -> m [leg]
getRemainingLegs journeyId = do
  -- get the remaining legs
  journeyLegs <- getAllLegs journeyId
  let remainingLegs = filter (\leg -> notElem leg.status [completedStatus]) journeyLegs
  return remainingLegs
--   filter -> based on status

-- createLeg :: RequiedData (OTP response for each leg)
deleteLeg :: JourneyLeg leg m => leg -> m ()
  JL.cancel leg
  -- remove it from the journey mark it deleted/skipped

updateLeg :: JourneyLeg leg m => UpdateJourneyLeg -> leg -> m ()
  -- update the leg
  -- update it in the journey

skipJourney :: Journey -> [leg] -> m ()
-- skipJourney journey
    -- getRemainingLegs
    map update [leg]
    -- @@ call cancel for current leg

endJourney :: Journey -> m ()
endJourney
-- if last leg then update leg
-- loop through and delete/update legs and journey as required
-- call leg level cancel

replaceLeg :: JourneyLeg leg1 leg2 m => Journey -> [leg1] -> leg2 -> m () -- leg2 can be an array
replaceLeg journey oldLegs newLeg =
  forM_ (deleteLeg journey) oldLegs >> addLeg journey newLeg

extendLeg :: JourneyLeg leg1 leg2 m => Journey -> [leg1] -> leg2 -> m ()
  forM_ (deleteLeg journey) oldLegs >> updateLeg journey newLeg


