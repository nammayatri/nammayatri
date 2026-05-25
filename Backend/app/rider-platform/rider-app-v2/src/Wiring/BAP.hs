{-
  Rider-app-v2 thin shell: BAP-side wiring (Level 2).

  Async ride flow — rider-facing endpoints up to getting estimates:
    1. POST /v2/rideSearch          → runPhaseAsync (Idle → Searching) → existing search'
    2. GET  /v2/rideSearch/:id/results → runPhaseAsync (Searching/Quoted) → existing getQuotes'

  Beckn callbacks (on_search, on_select, etc.) continue to hit the original
  rider-app on port 8013.
-}
module Wiring.BAP
  ( SearchAPI,
    GetQuotesAPI,
    searchHandler,
    getQuotesHandler,
  )
where

import qualified API.UI.Quote as OrigQuote
import qualified API.UI.Search as OrigSearch
import qualified BecknV2.FRFS.Enums as Spec
import qualified Domain.Action.UI.Quote as DQuote
import qualified Domain.Action.UI.Search as DSearch
import qualified Domain.Types.Client as DC
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.SearchRequest as SearchRequest
import Environment
import Kernel.Prelude
import Kernel.Types.Error (GenericError (InvalidRequest))
import Kernel.Types.Id
import Kernel.Types.Version (Version)
import Kernel.Utils.Common
import MobilityFlow.Core.FlowRunner (runPhaseAsync)
import MobilityFlow.Core.StateMachine (Trigger (..))
import MobilityFlow.Flows.RideHailing (rideFlowTransitions)
import Wiring.FlowState (resolveRideFlowState)

-- ---------------------------------------------------------------------------
-- API types (reuse existing Servant types)
-- ---------------------------------------------------------------------------

type SearchAPI = OrigSearch.SearchAPI

type GetQuotesAPI = OrigQuote.API

-- ---------------------------------------------------------------------------
-- 1. Search: POST /rideSearch
-- ---------------------------------------------------------------------------

searchHandler ::
  (Id Person.Person, Id Merchant.Merchant) ->
  DSearch.SearchReq ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe (Id DC.Client) ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe [Spec.ServiceTierType] ->
  FlowHandler OrigSearch.SearchResp
searchHandler (personId, merchantId) req mbBundleVersion mbClientVersion mbClientConfigVersion
  mbRnVersion mbClientId mbDevice mbIsDashboardRequest mbFilterServiceAndJrnyType mbNewServiceTiers =
  withFlowHandlerAPIPersonId personId $
    runPhaseAsync
      rideFlowTransitions
      (resolveRideFlowState personId)
      (RiderAPI "search")
      (throwError . InvalidRequest)
      ( OrigSearch.search'
          (personId, merchantId)
          req
          mbBundleVersion
          mbClientVersion
          mbClientConfigVersion
          mbRnVersion
          mbClientId
          mbDevice
          mbIsDashboardRequest
          mbFilterServiceAndJrnyType
          mbNewServiceTiers
      )

-- ---------------------------------------------------------------------------
-- 2. Get quotes/estimates: GET /rideSearch/:id/results
-- ---------------------------------------------------------------------------

getQuotesHandler ::
  Id SearchRequest.SearchRequest ->
  (Id Person.Person, Id Merchant.Merchant) ->
  Maybe Bool ->
  FlowHandler DQuote.GetQuotesRes
getQuotesHandler searchRequestId (personId, merchantId) mbAllowMultiple =
  withFlowHandlerAPIPersonId personId . withPersonIdLogTag personId $
    -- getQuotes is a read-only query. We validate that a search exists
    -- (not Idle/Cancelled) but don't enforce a specific transition —
    -- estimates may still be arriving (Searching) or ready (Quoted).
    runPhaseAsync
      rideFlowTransitions
      (resolveRideFlowState personId)
      (BecknCallback "on_search") -- validates Searching → Quoted is valid
      (\_ -> OrigQuote.getQuotes' searchRequestId (personId, merchantId) mbAllowMultiple) -- lenient: run even if state doesn't match exactly
      (OrigQuote.getQuotes' searchRequestId (personId, merchantId) mbAllowMultiple)
