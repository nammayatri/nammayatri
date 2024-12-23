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
import qualified Lib.JourneyModule.Base as JM
import qualified Lib.JourneyPlannerTypes as JPT
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
  legsInfo <- JM.getAllLegs journeyId

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
