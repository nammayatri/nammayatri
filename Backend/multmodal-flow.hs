JM => Journey Module
JL => Journey Leg

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

// jounery info {jouneryId}, also pass the geocoder responses of addreeses
jounery <- JM.getJourney journeyId
searchLegs <- forM (JM.addLeg . mkJourneyLegSearch person req.info) jounery.legs
return {searchLegs}

mkJourneyLegSearch person info multimodalLeg = do
  taxiLegInfo = mkTaxiLegInfo info person
  case multimodalLeg.mode of
    Walk -> WalkLegRequestSearch $ multimodalLeg info


data JourneyConfirmReq = JourneyConfirmReq {journeyConfirmReqElements :: [JourneyConfirmReqElement]}

data JourneyConfirmReqElement = JourneyConfirmReqElement {journeyLegOrder :: Int, skipBooking :: Bool}

-- add journeyLegInfo.isSkipped :: Bool, journeyLegInfo.isDeleted :: Bool to SearchRequest, FRFSSearch, WalkLegMultimodal

// confirm api call
multimodalConfirm :: (Maybe Id Person, Id Merchant) -> Id Journey -> JourneyConfirmReq -> Flow Success
multimodalConfirm (personId, merchantId) journeyId journeyConfirmReq = do
  JM.startJourney journeyId journeyConfirmReq
