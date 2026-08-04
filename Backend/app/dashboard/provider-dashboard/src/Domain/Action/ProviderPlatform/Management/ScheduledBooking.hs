{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.ProviderPlatform.Management.ScheduledBooking
  ( getScheduledBookingList,
    getScheduledBookingInfo,
    postScheduledBookingOpsNoteAdd,
    putScheduledBookingOpsNoteUpdate,
  )
where

import qualified API.Client.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.ScheduledBooking
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

getScheduledBookingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.ScheduledBooking.AssignmentStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.Flow API.Types.ProviderPlatform.Management.ScheduledBooking.ScheduledBookingListRes)
getScheduledBookingList merchantShortId opCity apiTokenInfo assignmentStatus from limit offset to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.scheduledBookingDSL.getScheduledBookingList) assignmentStatus from limit offset to

getScheduledBookingInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Management.ScheduledBooking.ScheduledBookingInfoRes)
getScheduledBookingInfo merchantShortId opCity apiTokenInfo transactionId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.scheduledBookingDSL.getScheduledBookingInfo) transactionId

postScheduledBookingOpsNoteAdd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.ScheduledBooking.AddOpsNoteReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postScheduledBookingOpsNoteAdd merchantShortId opCity apiTokenInfo transactionId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.scheduledBookingDSL.postScheduledBookingOpsNoteAdd) transactionId (Kernel.Prelude.Just apiTokenInfo.personId.getId) req

putScheduledBookingOpsNoteUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.ScheduledBooking.UpdateOpsNoteReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
putScheduledBookingOpsNoteUpdate merchantShortId opCity apiTokenInfo noteId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.scheduledBookingDSL.putScheduledBookingOpsNoteUpdate) noteId (Kernel.Prelude.Just apiTokenInfo.personId.getId) req
