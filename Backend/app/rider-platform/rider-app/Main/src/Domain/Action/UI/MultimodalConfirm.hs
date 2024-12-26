{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.UI.MultimodalConfirm (postMultimodalConfirm, getMultimodalSwitchTaxi) where

import qualified API.Types.UI.MultimodalConfirm
import Domain.Action.UI.FRFSTicketService as FRFSTicketService
import Domain.Action.UI.Select as Select
import Domain.Types.Estimate
import qualified Domain.Types.Journey
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import Domain.Types.SearchRequest
import Environment
import EulerHS.Prelude hiding (find, forM_, id)
import Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Queries.Estimate as QEstimate
import Storage.Queries.FRFSQuote as QFRFSQuote
import Storage.Queries.FRFSSearch as QFRFSSearch
import Storage.Queries.Journey as QJourney
import Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error

postMultimodalConfirm ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    API.Types.UI.MultimodalConfirm.JourneyConfirmReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postMultimodalConfirm (personId, merchantId) journeyId journeyConfirmReq = do
  journey <- QJourney.findByPrimaryKey journeyId >>= fromMaybeM (InvalidRequest "Journey not found")
  let selectReq =
        Select.DSelectReq
          { customerExtraFee = Nothing,
            customerExtraFeeWithCurrency = Nothing,
            autoAssignEnabled = True,
            autoAssignEnabledV2 = Just True,
            paymentMethodId = Nothing,
            otherSelectedEstimates = Nothing,
            isAdvancedBookingEnabled = Nothing,
            deliveryDetails = Nothing,
            disabilityDisable = Nothing
          }
  let skippedBookings = filter (\journeyConfirmReqElement -> journeyConfirmReqElement.skipBooking) journeyConfirmReq.journeyConfirmReqElements

  searchReqs <- QSearchRequest.findAllByJourneyId journeyId
  forM_ searchReqs \searchReq -> do
    journeyLegInfo <- searchReq.journeyLegInfo & fromMaybeM (InvalidRequest "journeyLegInfo not found")
    estimateId <- journeyLegInfo.pricingId & fromMaybeM (InvalidRequest "estimateId not found")
    let searchReqLegOrder = journeyLegInfo.journeyLegOrder
        skippedSearchReq = find (\skippedBooking -> skippedBooking.journeyLegOrder == searchReqLegOrder) skippedBookings
    case skippedSearchReq of
      Just _ -> do
        QSearchRequest.updateSkipBooking searchReq.id (Just True)
        reqEstimate <- QEstimate.findById (Id estimateId) >>= fromMaybeM (InvalidRequest "Estimate not found")
        initialFare <- journey.estimatedFare & fromMaybeM (InvalidRequest "estimatedFare not found")
        newEstimatedPrice <- initialFare `subtractPrice` reqEstimate.estimatedTotalFare
        QJourney.updateEstimatedFare (Just newEstimatedPrice) journeyId
      Nothing -> do
        pId <- personId & fromMaybeM (InvalidRequest "personId not found")
        void $ Select.select pId (Id estimateId) selectReq

  frfsSearchReqs <- QFRFSSearch.findAllByJourneyId journeyId
  forM_ frfsSearchReqs \frfsSearchReq -> do
    journeyLegInfo <- frfsSearchReq.journeyLegInfo & fromMaybeM (InvalidRequest "journeyLegInfo not found")
    quoteId <- journeyLegInfo.pricingId & fromMaybeM (InvalidRequest "estimateId not found")
    let searchReqLegOrder = journeyLegInfo.journeyLegOrder
        skippedSearchReq = find (\skippedBooking -> skippedBooking.journeyLegOrder == searchReqLegOrder) skippedBookings
    case skippedSearchReq of
      Just _ -> do
        QFRFSSearch.updateSkipBooking frfsSearchReq.id (Just True)
        reqQuote <- QFRFSQuote.findById (Id quoteId) >>= fromMaybeM (InvalidRequest "quote not found")
        initialFare <- journey.estimatedFare & fromMaybeM (InvalidRequest "estimatedFare not found")
        newEstimatedPrice <- initialFare `subtractPrice` reqQuote.price
        QJourney.updateEstimatedFare (Just newEstimatedPrice) journeyId
      Nothing -> do
        void $ FRFSTicketService.postFrfsQuoteConfirm (personId, merchantId) (Id quoteId)

  pure Kernel.Types.APISuccess.Success

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
