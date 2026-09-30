{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.RiderPlatform.Management.RideFeedback
  ( getRideFeedbackConfigList,
    getRideFeedbackConfig,
    postRideFeedbackConfigCreate,
    postRideFeedbackConfigUpdate,
    postRideFeedbackConfigToggle,
    postRideFeedbackConfigClone,
    postRideFeedbackConfigValidate,
    getRideFeedbackRidePreview,
    getRideFeedbackRideResponses,
    postRideFeedbackResponseRetryActions,
    getRideFeedbackMeta,
  )
where

import qualified API.Client.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.RideFeedback
import qualified Dashboard.Common
import qualified Data.Text
import "rider-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.RideFeedbackConfig
import qualified Domain.Types.RideFeedbackResponse
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Merchant

getRideFeedbackConfigList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.Flow API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackConfigListRes)
getRideFeedbackConfigList merchantShortId opCity apiTokenInfo questionKey enabled limit offset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rideFeedbackDSL.getRideFeedbackConfigList) questionKey enabled limit offset

getRideFeedbackConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig -> Environment.Flow API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackConfigItem)
getRideFeedbackConfig merchantShortId opCity apiTokenInfo configId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rideFeedbackDSL.getRideFeedbackConfig) configId

postRideFeedbackConfigCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.Management.RideFeedback.CreateRideFeedbackConfigReq -> Environment.Flow API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackConfigUpsertRes)
postRideFeedbackConfigCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rideFeedbackDSL.postRideFeedbackConfigCreate) req)

postRideFeedbackConfigUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig -> API.Types.RiderPlatform.Management.RideFeedback.UpdateRideFeedbackConfigReq -> Environment.Flow API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackConfigUpsertRes)
postRideFeedbackConfigUpdate merchantShortId opCity apiTokenInfo configId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rideFeedbackDSL.postRideFeedbackConfigUpdate) configId req)

postRideFeedbackConfigToggle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig -> API.Types.RiderPlatform.Management.RideFeedback.ToggleRideFeedbackConfigReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postRideFeedbackConfigToggle merchantShortId opCity apiTokenInfo configId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rideFeedbackDSL.postRideFeedbackConfigToggle) configId req)

postRideFeedbackConfigClone :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig -> API.Types.RiderPlatform.Management.RideFeedback.CloneRideFeedbackConfigReq -> Environment.Flow API.Types.RiderPlatform.Management.RideFeedback.CloneRideFeedbackConfigRes)
postRideFeedbackConfigClone merchantShortId opCity apiTokenInfo configId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rideFeedbackDSL.postRideFeedbackConfigClone) configId req)

postRideFeedbackConfigValidate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.Management.RideFeedback.CreateRideFeedbackConfigReq -> Environment.Flow API.Types.RiderPlatform.Management.RideFeedback.ValidateRideFeedbackConfigRes)
postRideFeedbackConfigValidate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rideFeedbackDSL.postRideFeedbackConfigValidate) req)

getRideFeedbackRidePreview :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.Flow API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackPreviewRes)
getRideFeedbackRidePreview merchantShortId opCity apiTokenInfo rideId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rideFeedbackDSL.getRideFeedbackRidePreview) rideId

getRideFeedbackRideResponses :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.Flow [API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackResponseItem])
getRideFeedbackRideResponses merchantShortId opCity apiTokenInfo rideId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rideFeedbackDSL.getRideFeedbackRideResponses) rideId

postRideFeedbackResponseRetryActions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Domain.Types.RideFeedbackResponse.RideFeedbackResponse -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postRideFeedbackResponseRetryActions merchantShortId opCity apiTokenInfo responseId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rideFeedbackDSL.postRideFeedbackResponseRetryActions) responseId)

getRideFeedbackMeta :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Environment.Flow API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackMetaRes)
getRideFeedbackMeta merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rideFeedbackDSL.getRideFeedbackMeta)
