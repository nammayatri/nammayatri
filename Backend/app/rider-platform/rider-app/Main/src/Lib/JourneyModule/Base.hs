module Lib.JourneyModule.Base where

import qualified Kernel.External.MultiModal.Interface as ExternalInterface
import qualified Storage.Journey.Queries as JQ

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

startJourney :: Id Journey -> ConfirmReq -> m () -- confirm request
startJourney journeyId = do
  -- journey <- getJourney journeyId
  allLegs <- getAllLegs journeyId
  mapM JL.confirm allLegs

getAllLegs :: Id Journey -> m ()
getAllLegs journeyId = do
  searchRequests <- findAllByJourneyId journeyId -- in searchRequest
  frfsSearchRequests <- findAllByJourneyId journeyId -- in FRFS search
  let legs = sortBy order $ transformSearchReqToJourneyLeg searchRequests  <> transformFRFSSearchReqToJourneyLegfrfs SearchRequests 
  return legs

replaceLeg :: JourneyLeg leg1 leg2 m => Journey -> [leg1] -> leg2 -> m () -- leg2 can be an array
replaceLeg journey oldLegs newLeg =
  forM_ (deleteLeg journey) oldLegs >> addLeg journey newLeg

extendLeg :: JourneyLeg leg1 leg2 m => Journey -> [leg1] -> leg2 -> m ()
  forM_ (deleteLeg journey) oldLegs >> updateLeg journey newLeg


