{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.RiderPlatform.Management.Payout
  ( getPayoutPayoutOrder,
    postPayoutPayoutRetrigger,
  )
where

import qualified API.Client.RiderPlatform.Management as ManagementClient
import qualified API.Types.RiderPlatform.Management.Payout
import "rider-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified "payment" Lib.Payment.API.Payout.Types as PayoutTypes
import qualified "lib-dashboard" SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Merchant

getPayoutPayoutOrder ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo UserActionType ->
  Text ->
  Environment.Flow PayoutTypes.PayoutOrderResp
getPayoutPayoutOrder merchantShortId opCity apiTokenInfo payoutOrderId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.getPayoutPayoutOrder) payoutOrderId (Just apiTokenInfo.personId.getId)

postPayoutPayoutRetrigger :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.Management.Payout.RetriggerPayoutReq -> Environment.Flow API.Types.RiderPlatform.Management.Payout.RetriggerPayoutResp)
postPayoutPayoutRetrigger merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.postPayoutPayoutRetrigger) requestorId req)
