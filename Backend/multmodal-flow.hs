JM => Journey Module
JL => Journey Leg

@yashika
// normal search api call
multimodalOptions <- otp
journeys <-
  multimodalOptions `forM` \multimodalOption -> do
    journeyLegs = traverse mkJourneyLeg multimodalOption.legs
    initReq =
      JourneyInitData
        { parentSearchId
        , merchantId
        , merchantOperatingCityId
        , legs = journeyLegs
        }
    JM.init initReq
Initial Search Stage
1. Making changes inside search api call
2. Expose fare api from BPP, also logic to catch metro and bus fare
3, Add legs inside journey table and create journey_leg table, update journey table from and to ttypes
4. Make sure getQuotes function, getJounery is just fetching the data from journey tables that's all

@kavyashree
// jounery info {jouneryId}, also pass the geocoder responses of addreeses
jounery <- JM.getJourney journeyId
searchLegs <- forM (JM.addLeg . mkJourneyLegSearch person req.info) jounery.legs
return {searchLegs}

mkJourneyLegSearch person info multimodalLeg = do
  taxiLegInfo = mkTaxiLegInfo info person
  case multimodalLeg.mode of
    Walk -> WalkLegRequestSearch $ multimodalLeg info
    Taxi -> TaxilegRequestSearch $
1. Info api will give leg ids for all legs, along with route
2. To get the fare of the legs, poll particular leg api
  e.g. rideSearch/{searchId}/results
3. Switch Variant, estimates are already there because of polling of results
  1. /multimodal/{searchRequestId}/switch/{estimateId}
  2. internally called updateLeg function

@prakhar
1. Swith to auto, switch to walk
2. check if current leg is auto, if yes and if replacing the very next leg then extend the leg
3.

@hemant
confirm

data JourneyConfirmReq = JourneyConfirmReq {journeyConfirmReqElements :: [JourneyConfirmReqElement]}

data JourneyConfirmReqElement = JourneyConfirmReqElement {journeyLegOrder :: Int, skipBooking :: Bool}

-- add journeyLegInfo.isSkipped :: Bool, journeyLegInfo.isDeleted :: Bool to SearchRequest, FRFSSearch, WalkLegMultimodal

// confirm api call
multimodalConfirm :: (Maybe Id Person, Id Merchant) -> Id Journey -> JourneyConfirmReq -> Flow Success
multimodalConfirm (personId, merchantId) journeyId journeyConfirmReq = do
  JM.startJourney journeyId journeyConfirmReq
