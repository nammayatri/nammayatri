{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.MultimodalSwitchTaxi (getMultimodalSwitchTaxi) where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Estimate
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.SearchRequest
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Queries.Estimate as QEstimate
import Storage.Queries.Journey as QJourney
import Storage.Queries.SearchRequest as QSearchRequest
import Tools.Auth
import Tools.Error

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
  journeyLegInfo <- searchRequest.journeyLegInfo & fromMaybeM (InvalidRequest "journeyLegInfo not found")
  oldEstimateId <- journeyLegInfo.pricingId & fromMaybeM (InvalidRequest "pricingId not found")
  oldEstimate <- QEstimate.findById (Id oldEstimateId) >>= fromMaybeM (InvalidRequest "Estimate not found")
  QSearchRequest.updatePricingId searchRequestId (Just estimateId.getId)
  newEstimate <- QEstimate.findById estimateId >>= fromMaybeM (InvalidRequest "Estimate not found")
  let journeyId = journeyLegInfo.journeyId
  journey <- QJourney.findByPrimaryKey (Id journeyId) >>= fromMaybeM (InvalidRequest "Journey not found")
  initialFare <- journey.estimatedFare & fromMaybeM (InvalidRequest "estimatedFare not found")
  price1 <- initialFare `subtractPrice` oldEstimate.estimatedTotalFare
  newEstimatedPrice <- price1 `addPrice` newEstimate.estimatedTotalFare
  QJourney.updateEstimatedFare (Just newEstimatedPrice) (Id journeyId)
  pure Kernel.Types.APISuccess.Success
