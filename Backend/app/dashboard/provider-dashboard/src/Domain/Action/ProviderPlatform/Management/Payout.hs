{-# OPTIONS_GHC -Wno-orphans #-}

-- NOTE (reviewer, remove before merge): Main's forwarders are unchanged. Added: forwarders for the new endpoints
--   (scheduled-config VIEW / VIEW_DIFF, adhoc lookup / initiate, batch list, batch orders, excluded list) and one HideSecrets
--   instance. As on main, detail / retry / cancel / cash check only the admin's token against the URL's merchant and city;
--   the driver app does not check that the payout request belongs to that merchant or city. Juspay/Stripe: no existing
--   route changes; the driver app refuses the new adhoc route in a Juspay/Stripe city.
module Domain.Action.ProviderPlatform.Management.Payout
  ( getPayoutPayout,
    getPayoutPayoutOrder,
    getPayoutPayoutHistory,
    getPayoutPayoutReferralHistory,
    postPayoutPayoutRetry,
    postPayoutPayoutCancel,
    postPayoutPayoutCash,
    postPayoutPayoutVpaDelete,
    postPayoutPayoutVpaUpdate,
    postPayoutPayoutVpaRefundRegistration,
    postPayoutPayoutScheduledPayoutConfigUpsert,
    getPayoutPayoutScheduledPayoutConfig,
    getPayoutAdhocLookup,
    postPayoutAdhocInitiate,
    getPayoutBatchList,
    getPayoutBatchOrders,
    getPayoutExcluded,
  )
where

import qualified API.Client.ProviderPlatform.Management as ManagementClient
import qualified API.Types.ProviderPlatform.Management.Payout as ApiPayout
import qualified "lib-dashboard" Dashboard.Common as Common
import Data.Time (Day)
import "dynamic-offer-driver-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.Transaction as DT
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified "payment" Lib.Payment.API.Payout.Types as PayoutTypes
import qualified "payment" Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified "payment" Lib.Payment.Domain.Types.PayoutRequest as PayoutRequest
import qualified "lib-dashboard" SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import Tools.Auth.Merchant

instance Common.HideSecrets ApiPayout.UpdateScheduledPayoutConfigReq where
  hideSecrets = identity

-- NOTE (reviewer, remove before merge): Lets the adhoc initiate call (the only new endpoint that writes) be stored as a
--   dashboard transaction (audit row) through buildPayoutManagementServerTransaction, like the other write APIs here.
--   The request holds only person ids, so nothing is hidden (identity). Bulk-only endpoint.
instance Common.HideSecrets ApiPayout.AdhocPayoutInitiateReq where
  hideSecrets = identity

buildPayoutManagementServerTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  ApiTokenInfo UserActionType ->
  Maybe request ->
  m (DT.Transaction UserActionType)
buildPayoutManagementServerTransaction apiTokenInfo =
  T.buildTransaction (DT.ActionAPI apiTokenInfo.userActionType) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing

getPayoutPayout ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo UserActionType ->
  Kernel.Types.Id.Id PayoutRequest.PayoutRequest ->
  Environment.Flow PayoutTypes.PayoutRequestResp
getPayoutPayout merchantShortId opCity apiTokenInfo payoutRequestId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.getPayoutPayout) payoutRequestId (Just apiTokenInfo.personId.getId)

getPayoutPayoutHistory ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo UserActionType ->
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe Bool ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Environment.Flow PayoutTypes.PayoutHistoryRes
getPayoutPayoutHistory merchantShortId opCity apiTokenInfo driverId driverPhoneNo from isFailedOnly limit offset to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.getPayoutPayoutHistory) driverId driverPhoneNo from isFailedOnly limit offset to (Just apiTokenInfo.personId.getId)

getPayoutPayoutReferralHistory ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo UserActionType ->
  Maybe Bool ->
  Maybe Text ->
  Maybe (Kernel.Types.Id.Id Common.Driver) ->
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Environment.Flow ApiPayout.PayoutReferralHistoryRes
getPayoutPayoutReferralHistory merchantShortId opCity apiTokenInfo areActivatedRidesOnly customerPhoneNo driverId driverPhoneCountryCode driverPhoneNo from limit offset to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.getPayoutPayoutReferralHistory) areActivatedRidesOnly customerPhoneNo driverId driverPhoneCountryCode driverPhoneNo from limit offset to (Just apiTokenInfo.personId.getId)

postPayoutPayoutRetry ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo UserActionType ->
  Kernel.Types.Id.Id PayoutRequest.PayoutRequest ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutRetry merchantShortId opCity apiTokenInfo payoutRequestId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $ do
    ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.postPayoutPayoutRetry) payoutRequestId (Just apiTokenInfo.personId.getId)

postPayoutPayoutCancel ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo UserActionType ->
  Kernel.Types.Id.Id PayoutRequest.PayoutRequest ->
  PayoutTypes.PayoutCancelReq ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutCancel merchantShortId opCity apiTokenInfo payoutRequestId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ do
    ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.postPayoutPayoutCancel) payoutRequestId (Just apiTokenInfo.personId.getId) req

postPayoutPayoutCash ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo UserActionType ->
  Kernel.Types.Id.Id PayoutRequest.PayoutRequest ->
  PayoutTypes.PayoutCashUpdateReq ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutCash merchantShortId opCity apiTokenInfo payoutRequestId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ do
    ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.postPayoutPayoutCash) payoutRequestId (Just apiTokenInfo.personId.getId) req

postPayoutPayoutVpaDelete ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo UserActionType ->
  PayoutTypes.DeleteVpaReq ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutVpaDelete merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ do
    ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.postPayoutPayoutVpaDelete) (Just apiTokenInfo.personId.getId) req

postPayoutPayoutVpaUpdate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo UserActionType ->
  PayoutTypes.UpdateVpaReq ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutVpaUpdate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ do
    ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.postPayoutPayoutVpaUpdate) (Just apiTokenInfo.personId.getId) req

postPayoutPayoutVpaRefundRegistration ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo UserActionType ->
  PayoutTypes.RefundRegAmountReq ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutVpaRefundRegistration merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ do
    ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.postPayoutPayoutVpaRefundRegistration) (Just apiTokenInfo.personId.getId) req

postPayoutPayoutScheduledPayoutConfigUpsert ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo UserActionType ->
  ApiPayout.UpdateScheduledPayoutConfigReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postPayoutPayoutScheduledPayoutConfigUpsert merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ do
    ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.postPayoutPayoutScheduledPayoutConfigUpsert) (Just apiTokenInfo.personId.getId) req

getPayoutPayoutOrder ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo UserActionType ->
  Text ->
  Environment.Flow PayoutTypes.PayoutOrderResp
getPayoutPayoutOrder merchantShortId opCity apiTokenInfo payoutOrderId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.getPayoutPayoutOrder) payoutOrderId (Just apiTokenInfo.personId.getId)

-- NOTE (reviewer, remove before merge): Forwarders for the new endpoints. Each does main's merchantCityAccessCheck (the
--   admin's token must be for this merchant and city) and then proxies to the driver app, which also checks the URL's city
--   and, for adhoc, that the city pays through a bulk partner. Only postPayoutAdhocInitiate writes, so only it stores a
--   transaction row. No Juspay/Stripe behaviour change: these routes do not exist on main.
getPayoutAdhocLookup ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo UserActionType ->
  Text ->
  Environment.Flow ApiPayout.AdhocPayoutLookupResp
getPayoutAdhocLookup merchantShortId opCity apiTokenInfo personId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.getPayoutAdhocLookup) (Just apiTokenInfo.personId.getId) personId

postPayoutAdhocInitiate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo UserActionType ->
  ApiPayout.AdhocPayoutInitiateReq ->
  Environment.Flow ApiPayout.AdhocPayoutInitiateResp
postPayoutAdhocInitiate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ do
    ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.postPayoutAdhocInitiate) (Just apiTokenInfo.personId.getId) req

getPayoutBatchList ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo UserActionType ->
  Maybe Int ->
  Maybe Int ->
  Maybe DPayoutBatch.PayoutBatchOrigin ->
  Maybe DPayoutBatch.PayoutBatchRail ->
  Maybe DPayoutBatch.PayoutBatchStatus ->
  Day ->
  Day ->
  Environment.Flow ApiPayout.PayoutBatchListRes
getPayoutBatchList merchantShortId opCity apiTokenInfo limit offset origin payoutRail status fromDate toDate = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.getPayoutBatchList) limit offset origin payoutRail status (Just apiTokenInfo.personId.getId) fromDate toDate

getPayoutBatchOrders ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo UserActionType ->
  Text ->
  Environment.Flow ApiPayout.PayoutBatchOrdersRes
getPayoutBatchOrders merchantShortId opCity apiTokenInfo batchId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.getPayoutBatchOrders) batchId (Just apiTokenInfo.personId.getId)

getPayoutExcluded ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo UserActionType ->
  Maybe Day ->
  Maybe Int ->
  Maybe Int ->
  Maybe Day ->
  Environment.Flow ApiPayout.PayoutExcludedRes
getPayoutExcluded merchantShortId opCity apiTokenInfo from limit offset to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.getPayoutExcluded) from limit offset to (Just apiTokenInfo.personId.getId)

-- | Read-only, so no transaction row: VIEW and VIEW_DIFF proxy straight through, and the commit stays
--   on the upsert above. Written in this module's import style -- the generated stub qualifies
--   @Kernel.Prelude@ and @API.Client.ProviderPlatform.Management@, neither of which is in scope here.
getPayoutPayoutScheduledPayoutConfig ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo UserActionType ->
  Maybe Int ->
  Maybe Bool ->
  -- command, frequency and payoutCategory are Text on the wire and parsed against their enums in
  -- the BPP handler: the generated enum query-param instances only accept a JSON-quoted value.
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe Bool ->
  Maybe Kernel.Types.Common.HighPrecMoney ->
  Maybe Text ->
  Maybe Int ->
  Maybe Text ->
  Environment.Flow ApiPayout.ScheduledPayoutConfigViewResp
getPayoutPayoutScheduledPayoutConfig merchantShortId opCity apiTokenInfo batchSize bufferCheckEnabled command dayOfMonth dayOfWeek frequency intervalDays intervalHours isEnabled minimumPayoutAmount payoutCategory rescheduleBufferMinutes timeOfDay = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.getPayoutPayoutScheduledPayoutConfig) batchSize bufferCheckEnabled command dayOfMonth dayOfWeek frequency intervalDays intervalHours isEnabled minimumPayoutAmount payoutCategory rescheduleBufferMinutes timeOfDay (Just apiTokenInfo.personId.getId)
