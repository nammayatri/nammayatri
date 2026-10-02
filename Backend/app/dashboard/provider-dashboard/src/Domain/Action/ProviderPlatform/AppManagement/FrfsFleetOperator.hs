module Domain.Action.ProviderPlatform.AppManagement.FrfsFleetOperator
  ( postFrfsFleetOperatorCurrentOperation,
    postFrfsFleetOperatorTripAction,
    postFrfsFleetOperatorV2CurrentOperation,
    postFrfsFleetOperatorV2TripAction,
  )
where

import qualified API.Client.ProviderPlatform.AppManagement
import qualified "dynamic-offer-driver-app" API.Types.UI.FRFSFleetOperator
import "dynamic-offer-driver-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified "lib-dashboard" SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Merchant

postFrfsFleetOperatorCurrentOperation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationReq -> Environment.Flow API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationResp)
postFrfsFleetOperatorCurrentOperation merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.frfsFleetOperatorDSL.postFrfsFleetOperatorCurrentOperation) req)

postFrfsFleetOperatorTripAction :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionReq -> Environment.Flow API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionResp)
postFrfsFleetOperatorTripAction merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.frfsFleetOperatorDSL.postFrfsFleetOperatorTripAction) req)

postFrfsFleetOperatorV2CurrentOperation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationV2Req -> Environment.Flow API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationV2Resp)
postFrfsFleetOperatorV2CurrentOperation merchantShortId opCity apiTokenInfo operatorId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.frfsFleetOperatorDSL.postFrfsFleetOperatorV2CurrentOperation) operatorId requestorId req)

postFrfsFleetOperatorV2TripAction :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionV2Req -> Environment.Flow API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationV2Resp)
postFrfsFleetOperatorV2TripAction merchantShortId opCity apiTokenInfo operatorId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.frfsFleetOperatorDSL.postFrfsFleetOperatorV2TripAction) operatorId requestorId req)
