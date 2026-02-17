{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.RiderPlatform.Management.Sos (getSosTracking, getSosDetails, postSosCallExternalSOS) where

import qualified API.Client.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.Sos
import qualified Dashboard.Common
import qualified "lib-dashboard" Domain.Types.Merchant
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
import Tools.Auth.Api
import Tools.Auth.Merchant

getSosTracking :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Sos -> Environment.Flow API.Types.RiderPlatform.Management.Sos.SosTrackingRes)
getSosTracking merchantShortId opCity sosId = do
  let checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.sosDSL.getSosTracking) sosId

postSosCallExternalSOS :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Sos -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postSosCallExternalSOS merchantShortId opCity apiTokenInfo sosId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.sosDSL.postSosCallExternalSOS) sosId)

getSosDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Sos -> Environment.Flow API.Types.RiderPlatform.Management.Sos.SosDetailsMaybeRes)
getSosDetails merchantShortId opCity sosId = do
  let checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.sosDSL.getSosDetails) sosId
