{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.MultimodalConfirm (postMultimodalConfirm, getMultimodalSwitchTaxi) where

import qualified API.Types.UI.MultimodalConfirm
import Data.OpenApi (ToSchema)
import Domain.Action.UI.FRFSTicketService as FRFSTicketService
import Domain.Action.UI.Select as Select
import Domain.Types.Estimate
import qualified Domain.Types.Journey
import qualified Domain.Types.Journey as DJourney
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import Domain.Types.SearchRequest
import qualified Domain.Types.ServiceTierType as DSTT
import Environment
import EulerHS.Prelude hiding (find, forM_, id)
import Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Id
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Types as JPT
import qualified Lib.JourneyModule.Base as JM
import Servant
import Storage.Queries.Estimate as QEstimate
import Storage.Queries.FRFSQuote as QFRFSQuote
import Storage.Queries.FRFSSearch as QFRFSSearch
import Storage.Queries.Journey as QJourney
import Storage.Queries.SearchRequest as QSearchRequest
import Tools.Auth
import Tools.Error

postMultimodalInfo ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    API.Types.UI.MultimodalConfirm.JourneyInfoReq ->
    Environment.Flow API.Types.UI.MultimodalConfirm.JourneyInfoResp
  )
postMultimodalInfo (_personId, _merchantId) journeyId req = do
  addAllLegs journeyId req.legsReq
  journey <- JM.getJourney journeyId
  legs <- JN.getAllLegsInfo journeyId
  return $
    JourneyInfoResp
      { estimatedDuration = journey.estimatedDuration,
        estimatedFare = journey.estimatedFare,
        estimatedDistance = journey.estimatedDistance,
        legs
      }

postMultimodalConfirm ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postMultimodalConfirm (_, _) journeyId = do
  journey <- JM.getJourney journeyId
  JM.startJourney journey.id
  pure Kernel.Types.APISuccess.Success

getMultimodalBookingInfo ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
getMultimodalBookingInfo (personId, merchantId) journeyId = do
  journey <- JM.getJourney journeyId
  allLegs <- JM.getAllLegsInfo journeyId

getMultimodalSwitchTaxi ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest ->
    Kernel.Types.Id.Id Domain.Types.Estimate.Estimate ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
getMultimodalSwitchTaxi (_, _) searchRequestId estimateId = do
  searchRequest <- QSearchRequest.findById searchRequestId >>= fromMaybeM (InvalidRequest "SearchRequest not found")
  journeyLegInfo <- searchRequest.journeyLegInfo & fromMaybeM (InvalidRequest "Journey Leg for SearchRequest not found")
  oldEstimateId <- journeyLegInfo.pricingId & fromMaybeM (InternalError "Old estimate id not found for search request")
  oldEstimate <- QEstimate.findById (Id oldEstimateId) >>= fromMaybeM (InternalError "Old estimate not found for search request")
  newEstimate <- QEstimate.findById estimateId >>= fromMaybeM (InvalidRequest "New Estimate requested not found")
  QSearchRequest.updatePricingId searchRequestId (Just estimateId.getId)
  let journeyId = journeyLegInfo.journeyId
  journey <- QJourney.findByPrimaryKey (Id journeyId) >>= fromMaybeM (InvalidRequest "Journey not found")
  initialFare <- journey.estimatedFare & fromMaybeM (InvalidRequest "Journey for SearchRequest not found")
  price1 <- initialFare `subtractPrice` oldEstimate.estimatedTotalFare
  newEstimatedPrice <- price1 `addPrice` newEstimate.estimatedTotalFare
  QJourney.updateEstimatedFare (Just newEstimatedPrice) (Id journeyId)
  pure Kernel.Types.APISuccess.Success

postMultimodalLegSwitchTo ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey -> Text -> Text -> API.Types.UI.MultimodalConfirm.SwitchModeReq
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postMultimodalLegSwitchTo _ journeyId legId newMode req = do
  journeyLegs <- JM.getAllLegs journeyId
  let mbJourneyLegToSwitch = JM.getLeg legId journeyLegs
  case mbJourneyLegToSwitch of
    Just journeyLegToSwitch -> do
      currentLeg <- getCurrentLeg journeyId journeyLegs
      journey <- QJourney.findByPrimaryKey (Id journeyId) >>= fromMaybeM (InvalidRequest "Journey not found")
      if currentLeg.order > journeyLegToSwitch.order then 
        throwError "Cannot switch for already passed leg."
      else if journeyLegToSwitch.order == currentLeg.order then 
        switchCurrentLeg currentLeg journeyLegToSwitch journey
      else if currentLeg.order + 1 == journeyLegToSwitch.order then
        switchNextLeg currentLeg journeyLegToSwitch journeyLegs
      else do
          let legsToSwitch = getLegsToSwitch journeyLegToSwitch currentLeg journeyLegs newMode
              newLeg = constructNewLeg journeyLegToSwitch newMode
          JL.replaceLeg legsToSwitch newLeg
    Nothing -> throwError "legId not present for this journey."
  where
     

    getLegsToSwitch journeyLegToSwitch currentLeg journeyLegs newMode = do
      let
        initialState = LegsState
          { legsBeforeCurrentLeg = []
          , legsAfterCurrentLeg = Nothing
          , lastConsecutiveLegOrder = Nothing
          }

        processLeg acc leg
          | journeyLegToSwitch.order == leg.order = acc
          | journeyLegToSwitch.order < leg.order && leg.mode == newMode =
              acc { legsBeforeCurrentLeg = acc.legsBeforeCurrentLeg ++ [leg] }
          | journeyLegToSwitch.order < leg.order && leg.mode /= newMode =
              acc { legsBeforeCurrentLeg = [] }
          | journeyLegToSwitch.order > leg.order && leg.mode == newMode &&
              (isNothing acc.lastConsecutiveLegOrder || acc.lastConsecutiveLegOrder == Just (leg.order + 1)) =
              acc { legsAfterCurrentLeg = Just (maybe [leg] (++ [leg]) (acc.legsAfterCurrentLeg))
                  , lastConsecutiveLegOrder = Just leg.order
                  }
          | otherwise = acc

        finalState = foldl' processLeg initialState (drop (currentLeg.order - 1) journeyLegs)
      finalState.legsBeforeCurrentLeg ++ [journeyLegToSwitch] ++ maybe [] id finalState.legsAfterCurrentLeg

    constructNewLeg journeyLegToSwitch = do
      let startLocation = maybe journeyLegToSwitch.startLocation identity req.currentLocation
      distRes <- 
        Maps.getDistance merchantId merchantOpCityId $
          Maps.GetDistanceReq
            { origin = startLocation,
              destination = journeyLegToSwitch.endLocation,
              travelMode = Just $ if newMode == Walk then Maps.FOOT else Maps.CAR,
              sourceDestinationMapping = Nothing,
              distanceUnit = METER -- need to make config from merchant table
            }
      
      journeyLegToSwitch {
        startLocation = startLocation,
        mode = newMode,
        duration = distRes.duration, 
        distance = distRes.distance,
        fromStopDetails = Nothing,
        toStopDetails = Nothing,
        routeDetails = Nothing,
        fromArrivalTime = Nothing,
        fromDepartureTime = Nothing,
        toArrivalTime = Nothing,
        toDepartureTime = Nothing,
        agency = Nothing
      }
    
    switchCurrentLeg currentLeg journeyLegToSwitch journey = do
      case currentLeg.mode of 
        Unspecified -> throwError "Cannot switch current leg for this mode."
        _ -> do
          let newLeg = constructNewLeg journeyLegToSwitch req.currentLocation
          JL.replaceLeg journey [journeyLegToSwitch] newLeg
        
    switchNextLeg currentLeg journeyLegToSwitch journeyLegs = do
      case currentLeg.mode of
        Unspecified -> do 
          let mbLastLegToSwitch = foldl' (\acc x -> if isNothing acc || x.mode == Unspecified then Just x else Nothing) Nothing (drop (journeyLegToSwitch.order - 1) journeyLegs) 
              lastLegToSwitch = maybe journeyLegToSwitch identity mbLastLegToSwitch
          let req = TaxiLegRequest $ TaxiLegRequestUpdate EditLocationReq {toLocation : lastLegToSwitch.toLocation}
          JL.update req
          -- mapM JM.deleteLeg (filter (\leg -> leg.order <= lastLegToSwitch.order) (drop (journeyLegToSwitch.order - 1) journeyLegs))
        _ -> throwError "Switching not allowed for this mode."