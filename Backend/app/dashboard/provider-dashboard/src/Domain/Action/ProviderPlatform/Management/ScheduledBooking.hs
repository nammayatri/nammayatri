{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.ProviderPlatform.Management.ScheduledBooking
  ( getScheduledBookingList,
    getScheduledBookingInfo,
    getScheduledBookingDriverDistance,
    getScheduledBookingNearbyDrivers,
    postScheduledBookingAssign,
    postScheduledBookingUnassign,
    postScheduledBookingOpsNote,
  )
where

import qualified API.Client.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.ScheduledBooking
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import Kernel.Beam.Functions
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import qualified "lib-dashboard" Storage.Queries.Person as QP
import Tools.Auth.Api
import Tools.Auth.Merchant

getScheduledBookingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.ScheduledBooking.AssignmentStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.Flow API.Types.ProviderPlatform.Management.ScheduledBooking.ScheduledBookingListRes)
getScheduledBookingList merchantShortId opCity apiTokenInfo assignmentStatus from limit offset to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.scheduledBookingDSL.getScheduledBookingList) assignmentStatus from limit offset to

getScheduledBookingInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Management.ScheduledBooking.ScheduledBookingInfoRes)
getScheduledBookingInfo merchantShortId opCity apiTokenInfo transactionId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  addOpsNoteAuthorNames =<< API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.scheduledBookingDSL.getScheduledBookingInfo) transactionId

getScheduledBookingDriverDistance :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Management.ScheduledBooking.DriverDistanceRes)
getScheduledBookingDriverDistance merchantShortId opCity apiTokenInfo transactionId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.scheduledBookingDSL.getScheduledBookingDriverDistance) transactionId

postScheduledBookingAssign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.ScheduledBooking.AssignDriverReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postScheduledBookingAssign merchantShortId opCity apiTokenInfo transactionId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.scheduledBookingDSL.postScheduledBookingAssign) transactionId (Kernel.Prelude.Just apiTokenInfo.personId.getId) req

postScheduledBookingUnassign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postScheduledBookingUnassign merchantShortId opCity apiTokenInfo transactionId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.scheduledBookingDSL.postScheduledBookingUnassign) transactionId (Kernel.Prelude.Just apiTokenInfo.personId.getId)

postScheduledBookingOpsNote :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.ScheduledBooking.OpsNoteReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postScheduledBookingOpsNote merchantShortId opCity apiTokenInfo transactionId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.scheduledBookingDSL.postScheduledBookingOpsNote) transactionId (Kernel.Prelude.Just apiTokenInfo.personId.getId) req

addOpsNoteAuthorNames :: API.Types.ProviderPlatform.Management.ScheduledBooking.ScheduledBookingInfoRes -> Environment.Flow API.Types.ProviderPlatform.Management.ScheduledBooking.ScheduledBookingInfoRes
addOpsNoteAuthorNames response = do
  opsNotes <- mapM addAuthorName response.opsNotes
  pure response {API.Types.ProviderPlatform.Management.ScheduledBooking.opsNotes = opsNotes}
  where
    addAuthorName opsNote = do
      mbAuthor <- runInReplica $ QP.findById (Kernel.Types.Id.Id opsNote.authorId)
      pure opsNote {API.Types.ProviderPlatform.Management.ScheduledBooking.authorName = formatName <$> mbAuthor}
    formatName author = author.firstName <> " " <> author.lastName

getScheduledBookingNearbyDrivers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Double) -> Environment.Flow API.Types.ProviderPlatform.Management.ScheduledBooking.NearbyDriversRes)
getScheduledBookingNearbyDrivers merchantShortId opCity apiTokenInfo transactionId radiusKm = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.scheduledBookingDSL.getScheduledBookingNearbyDrivers) transactionId radiusKm
