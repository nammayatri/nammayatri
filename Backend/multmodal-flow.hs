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
    Unspecified -> WalkLegRequestSearch $ multimodalLeg info

