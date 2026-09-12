{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.RiderPlatform.Management.Payout
  ( getPayoutPayoutOrder,
  )
where

import qualified API.Client.RiderPlatform.Management as ManagementClient
import "rider-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified "payment" Lib.Payment.API.Payout.Types as PayoutTypes
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
  ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.getPayoutPayoutOrder) payoutOrderId
