module Domain.Action.Dashboard.AppManagement.FrfsFleetOperator
  ( postFrfsFleetOperatorCurrentOperation,
    postFrfsFleetOperatorTripAction,
    postFrfsFleetOperatorV2CurrentOperation,
    postFrfsFleetOperatorV2TripAction,
  )
where

import qualified API.Types.UI.FRFSFleetOperator
import qualified Domain.Action.UI.FRFSFleetOperator as UIFRFSFleetOperator
import qualified Domain.Types.Merchant
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

postFrfsFleetOperatorCurrentOperation ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationReq ->
  Flow API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationResp
postFrfsFleetOperatorCurrentOperation merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  UIFRFSFleetOperator.postFrfsFleetOperatorCurrentOperation' (Nothing, merchant.id, merchantOpCityId) True req

postFrfsFleetOperatorTripAction ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionReq ->
  Flow API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionResp
postFrfsFleetOperatorTripAction merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  UIFRFSFleetOperator.postFrfsFleetOperatorTripAction' (Nothing, merchant.id, merchantOpCityId) True req

-- | transitV2: ops view of a run (by vehicle / crew token / run id).
postFrfsFleetOperatorV2CurrentOperation ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Text ->
  Maybe Text ->
  API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationV2Req ->
  Flow API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationV2Resp
postFrfsFleetOperatorV2CurrentOperation merchantShortId opCity mbOperatorId _mbRequestorId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  UIFRFSFleetOperator.postFrfsFleetOperatorV2CurrentOperation' (Nothing, merchant.id, merchantOpCityId) True mbOperatorId req

-- | transitV2: ops trip action (start / end / rollback / skip / cancel / uncancel); the dashboard
-- person is recorded as the actor.
postFrfsFleetOperatorV2TripAction ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Text ->
  Maybe Text ->
  API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionV2Req ->
  Flow API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationV2Resp
postFrfsFleetOperatorV2TripAction merchantShortId opCity mbOperatorId mbRequestorId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  UIFRFSFleetOperator.postFrfsFleetOperatorV2TripAction' (Nothing, merchant.id, merchantOpCityId) True mbOperatorId mbRequestorId req
