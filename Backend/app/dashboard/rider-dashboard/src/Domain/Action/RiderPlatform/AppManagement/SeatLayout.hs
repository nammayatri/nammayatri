{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.RiderPlatform.AppManagement.SeatLayout
  ( upsertSeatLayout,
    getSeatLayout,
    deleteSeatLayout,
    listSeatLayout,
  )
where

import qualified API.Client.RiderPlatform.AppManagement
import qualified API.Types.Dashboard.AppManagement.SeatLayout
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.SeatLayout
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

upsertSeatLayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.AppManagement.SeatLayout.SeatLayoutUpsertReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
upsertSeatLayout merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.seatLayoutDSL.upsertSeatLayout) req)

getSeatLayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.SeatLayout.SeatLayout -> Environment.Flow API.Types.Dashboard.AppManagement.SeatLayout.SeatLayoutDetailResp)
getSeatLayout merchantShortId opCity apiTokenInfo seatLayoutId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.seatLayoutDSL.getSeatLayout) seatLayoutId

deleteSeatLayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.SeatLayout.SeatLayout -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
deleteSeatLayout merchantShortId opCity apiTokenInfo seatLayoutId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.seatLayoutDSL.deleteSeatLayout) seatLayoutId)

listSeatLayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.Flow [API.Types.Dashboard.AppManagement.SeatLayout.SeatLayoutListItem])
listSeatLayout merchantShortId opCity apiTokenInfo limit offset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.seatLayoutDSL.listSeatLayout) limit offset
