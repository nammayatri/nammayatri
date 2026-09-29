-- NOTE (reviewer, remove before merge): Main's file, extended:
--   1. postPayoutPayoutScheduledPayoutConfigUpsert passes the new optional fields and maps the two new frequencies
--      (HOURLY, EVERY_N_DAYS); the upsert's behaviour change is in Domain/Action/Dashboard/PayoutRequest.hs.
--   2. New handlers at the end: GET scheduledPayoutConfig (VIEW / VIEW_DIFF), adhoc lookup / initiate, batch list, batch
--      orders, excluded list.
--   Main's other handlers are unchanged here (the registration refund's bulk-city refusal is in PayoutRequest.hs).
--   Juspay/Stripe: the scheduled-config upsert is shared (changed on purpose); every new endpoint is read-only or
--   bulk-only (adhoc refuses a Juspay/Stripe city).
module Domain.Action.Dashboard.Management.Payout
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

import qualified API.Types.ProviderPlatform.Management.Payout as ApiPayout
import qualified "lib-dashboard" Dashboard.Common as DC
import qualified Data.Text as T
import Data.Time (Day, minutesToTimeZone, utcToLocalTime)
import qualified Domain.Action.Common.PayoutRequest as CommonPayout
import qualified Domain.Action.Dashboard.AdhocPayout as AdhocPayout
import qualified Domain.Action.Dashboard.Common as DCommon
import qualified Domain.Action.Dashboard.PayoutBatch as PayoutBatch
import qualified Domain.Action.Dashboard.PayoutRequest as DashboardPayoutRequest
import qualified Domain.Action.UI.Payout as UIPayout
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RiderDetails as DR
import qualified Domain.Types.ScheduledPayoutConfig as DSPC
import qualified Environment
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Encryption (decrypt, getDbHash)
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Payment.API.Payout as PayoutAPI
import qualified Lib.Payment.API.Payout.Types as PayoutTypes
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutOrder as PayoutOrder
import qualified Lib.Payment.Domain.Types.PayoutRequest as PayoutRequest
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import Servant (ServerT, (:<|>) (..))
import qualified SharedLogic.Payout.RetimeScheduledBatchPayout as Retime
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QR
import qualified Storage.Queries.RiderDetails as QRD
import Tools.Error

-- NOTE (reviewer, remove before merge): Same as main: payout-request detail / retry / cancel / cash do not check that
--   the request belongs to the URL's merchant or city. The new endpoints below all work within the URL's city.
payoutServer ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ServerT PayoutAPI.DashboardAPI Environment.Flow
payoutServer merchantShortId opCity =
  PayoutAPI.payoutDashboardHandler
    PayoutAPI.PayoutDashboardHandlerConfig
      { refreshPayoutRequest = CommonPayout.refreshPayoutRequestStatus,
        executePayoutRetry = CommonPayout.executeSpecialZonePayoutRequest,
        handleDeleteVpa = DashboardPayoutRequest.deleteVpa,
        handleUpdateVpa = DashboardPayoutRequest.updateVpa,
        handleRefundRegistrationAmount = DashboardPayoutRequest.refundRegistrationAmount merchantShortId opCity,
        merchantCity = opCity,
        mkHistoryItemEnricher = buildHistoryItemEnricher merchantShortId opCity
      }

-- | Look up merchant, operating city, and transporter config in one shot.
-- Used by both the payout-history enricher and the referral-history handler
-- to avoid repeating the same three queries + error mapping.
resolveMerchantOpCityAndTz ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Environment.Flow (Domain.Types.Merchant.Merchant, DMOC.MerchantOperatingCity, Minutes)
resolveMerchantOpCityAndTz merchantShortId opCity = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId}) Nothing >>= fromMaybeM (TransporterConfigDoesNotExist merchantOpCity.id.getId)
  pure (merchant, merchantOpCity, secondsToMinutes transporterConfig.timeDiffFromUtc)

-- | Resolves merchant + operating-city + transporter config once per request
-- and returns a closure that does the cheap per-row enrichment (person lookup
-- + phone decrypt + timezone conversion). Avoids N+1 on repeated lookups.
buildHistoryItemEnricher ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Environment.Flow (PayoutOrder.PayoutOrder -> Environment.Flow PayoutTypes.PayoutHistoryItem)
buildHistoryItemEnricher merchantShortId opCity = do
  (_, _, timeZoneDiff) <- resolveMerchantOpCityAndTz merchantShortId opCity
  let timeZone = minutesToTimeZone timeZoneDiff.getMinutes
  pure $ \payoutOrder -> do
    person <- QPerson.findById (Id.Id payoutOrder.customerId) >>= fromMaybeM (PersonNotFound payoutOrder.customerId)
    phoneNo <- decrypt payoutOrder.mobileNo
    pure
      PayoutTypes.PayoutHistoryItem
        { driverName = person.firstName,
          driverPhoneNo = phoneNo,
          driverId = payoutOrder.customerId,
          payoutAmount = payoutOrder.amount.amount,
          payoutStatus = show payoutOrder.status,
          payoutTime = utcToLocalTime timeZone payoutOrder.createdAt,
          payoutEntity = payoutOrder.entityName,
          payoutOrderId = payoutOrder.orderId,
          responseMessage = payoutOrder.responseMessage,
          responseCode = payoutOrder.responseCode,
          payoutRetriedOrderId = payoutOrder.retriedOrderId
        }

getPayoutPayout ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Id.Id PayoutRequest.PayoutRequest ->
  Maybe Text ->
  Environment.Flow PayoutTypes.PayoutRequestResp
getPayoutPayout merchantShortId opCity payoutRequestId _mbRequestorId = do
  let (_history :<|> getById :<|> _retry :<|> _cancel :<|> _cash :<|> _deleteVpa :<|> _updateVpa :<|> _refund) =
        payoutServer merchantShortId opCity
  getById payoutRequestId

getPayoutPayoutOrder ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Text ->
  Maybe Text ->
  Environment.Flow PayoutTypes.PayoutOrderResp
getPayoutPayoutOrder merchantShortId opCity payoutOrderIdText _mbRequestorId = do
  (merchant, _merchantOpCity, _) <- resolveMerchantOpCityAndTz merchantShortId opCity
  payoutOrder <- QPayoutOrder.findByOrderId payoutOrderIdText >>= fromMaybeM (PayoutOrderNotFound payoutOrderIdText)
  unless (payoutOrder.merchantId == merchant.id.getId) $
    throwError $ PayoutOrderNotFound payoutOrderIdText
  unless (payoutOrder.city == show opCity) $
    throwError $ PayoutOrderNotFound payoutOrderIdText
  refreshedOrder <- UIPayout.refreshPayoutOrderWithSettlement payoutOrder
  buildPayoutOrderResp merchantShortId opCity refreshedOrder

buildPayoutOrderResp ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  PayoutOrder.PayoutOrder ->
  Environment.Flow PayoutTypes.PayoutOrderResp
buildPayoutOrderResp merchantShortId opCity payoutOrder = do
  (_, _, timeZoneDiff) <- resolveMerchantOpCityAndTz merchantShortId opCity
  let timeZone = minutesToTimeZone timeZoneDiff.getMinutes
  person <- QPerson.findById (Id.Id payoutOrder.customerId) >>= fromMaybeM (PersonNotFound payoutOrder.customerId)
  phoneNo <- decrypt payoutOrder.mobileNo
  pure
    PayoutTypes.PayoutOrderResp
      { payoutOrderId = payoutOrder.orderId,
        payoutOrderDbId = payoutOrder.id,
        driverId = payoutOrder.customerId,
        driverName = person.firstName,
        driverPhoneNo = phoneNo,
        amount = payoutOrder.amount.amount,
        transferAmount = payoutOrder.transferAmount,
        status = show payoutOrder.status,
        entityName = payoutOrder.entityName,
        entityIds = payoutOrder.entityIds,
        responseMessage = payoutOrder.responseMessage,
        responseCode = payoutOrder.responseCode,
        retriedOrderId = payoutOrder.retriedOrderId,
        vpa = payoutOrder.vpa,
        payoutTime = utcToLocalTime timeZone payoutOrder.createdAt,
        createdAt = payoutOrder.createdAt,
        updatedAt = payoutOrder.updatedAt
      }

getPayoutPayoutHistory ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe Bool ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Maybe Text ->
  Environment.Flow PayoutTypes.PayoutHistoryRes
getPayoutPayoutHistory merchantShortId opCity mbDriverId mbDriverPhoneNo mbFrom mbIsFailedOnly mbLimit mbOffset mbTo _mbRequestorId = do
  let (history :<|> _getById :<|> _retry :<|> _cancel :<|> _cash :<|> _deleteVpa :<|> _updateVpa :<|> _refund) =
        payoutServer merchantShortId opCity
  history mbDriverId mbDriverPhoneNo mbFrom mbIsFailedOnly mbLimit mbOffset mbTo

data RiderDetailsWithRide = RiderDetailsWithRide
  { riderDetail :: DR.RiderDetails,
    ride :: Maybe DRide.Ride
  }

getPayoutPayoutReferralHistory ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Bool ->
  Maybe Text ->
  Maybe (Id.Id DC.Driver) ->
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Maybe Text ->
  Environment.Flow ApiPayout.PayoutReferralHistoryRes
getPayoutPayoutReferralHistory merchantShortId opCity areActivatedRidesOnly_ mbCustomerPhoneNo mbDriverId_ mbDriverPhoneCountryCode mbDriverPhoneNo mbFrom mbLimit mbOffset mbTo _mbRequestorId = do
  let limit = min maxLimit . fromMaybe defaultLimit $ mbLimit
      offset = fromMaybe 0 mbOffset
      areActivatedRidesOnly = fromMaybe False areActivatedRidesOnly_
  (merchant, merchantOpCity, timeZoneDiff) <- resolveMerchantOpCityAndTz merchantShortId opCity
  mbMobileNumberHash <- mapM getDbHash mbCustomerPhoneNo
  mbDriverId <- resolveDriverId merchant merchantOpCity.country
  allRiderDetails <-
    runInReplica $
      QRD.findAllRiderDetailsWithOptions
        merchant.id
        limit
        offset
        mbFrom
        mbTo
        areActivatedRidesOnly
        (Id.cast <$> mbDriverId)
        mbMobileNumberHash
  riderDetailsWithRide_ <- mapM attachFirstRide allRiderDetails
  -- Filter out rows whose first ride is in a different operating city.
  -- Pre-existing semantics: keep rows with no ride (ride = Nothing) and rows
  -- whose ride.merchantOperatingCityId matches. Intentional trade-off: the
  -- filter runs AFTER pagination so `count` may be < `limit` even when more
  -- rows would match; acceptable to preserve pre-removal behavior.
  let riderDetailsWithRide = filter (maybe True ((==) merchantOpCity.id . (.merchantOperatingCityId)) . (.ride)) riderDetailsWithRide_
  now <- getCurrentTime
  history <- mapM (buildReferralHistoryItem timeZoneDiff now) riderDetailsWithRide
  let count = length history
      summary = DC.Summary {totalCount = count, count}
  pure $ ApiPayout.PayoutReferralHistoryRes {history, summary}
  where
    maxLimit = 20
    defaultLimit = 10

    buildReferralHistoryItem tz now riderDetailWithRide = do
      let rd = riderDetailWithRide.riderDetail
          rideEndTime = (.tripEndTime) =<< riderDetailWithRide.ride
      phoneNo <- decrypt rd.mobileNumber
      pure $
        ApiPayout.ReferralHistoryItem
          { referralDate = fromMaybe now rd.referredAt,
            customerPhone = Just phoneNo,
            hasTakenValidActivatedRide = isNothing rd.payoutFlagReason && isJust rd.firstRideId,
            riderDetailsId = rd.id.getId,
            dateOfActivation = utcToIst tz rideEndTime,
            fraudFlaggedReason = castFlagReasonToCommon <$> rd.payoutFlagReason,
            rideId = Id.Id <$> rd.firstRideId,
            driverId = Id.cast <$> rd.referredByDriver,
            isReviewed = isJust rd.isFlagConfirmed
          }

    attachFirstRide riderDetail = do
      mbRide <- forM riderDetail.firstRideId $ \rideId -> runInReplica $ QR.findById (Id.Id rideId) >>= fromMaybeM (RideDoesNotExist rideId)
      pure RiderDetailsWithRide {riderDetail, ride = mbRide}

    resolveDriverId :: Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.Country -> Environment.Flow (Maybe (Id.Id DC.Driver))
    resolveDriverId merchant country = case (mbDriverId_, mbDriverPhoneNo) of
      (Just driverId, _) -> pure $ Just driverId
      (_, Just driverPhoneNo) -> do
        driverNumberHash <- getDbHash driverPhoneNo
        let mobileCountryCode = fromMaybe (P.getCountryMobileCode country) (DCommon.appendPlusInMobileCountryCode mbDriverPhoneCountryCode)
        driver <- QPerson.findByMobileNumberAndMerchantAndRole mobileCountryCode driverNumberHash merchant.id DP.DRIVER >>= fromMaybeM (PersonWithPhoneNotFound driverPhoneNo)
        pure . Just $ Id.cast driver.id
      _ -> pure Nothing

utcToIst :: Minutes -> Maybe UTCTime -> Maybe LocalTime
utcToIst timeZoneDiff = fmap $ utcToLocalTime (minutesToTimeZone timeZoneDiff.getMinutes)

castFlagReasonToCommon :: DR.PayoutFlagReason -> ApiPayout.PayoutFlagReason
castFlagReasonToCommon flag = case flag of
  DR.ExceededMaxReferral -> ApiPayout.ExceededMaxReferral
  DR.MinRideDistanceInvalid -> ApiPayout.MinRideDistanceInvalid
  DR.MinPickupDistanceInvalid -> ApiPayout.MinPickupDistanceInvalid
  DR.RideConstraintInvalid -> ApiPayout.RideConstraintInvalid
  DR.CustomerExistAsDriver -> ApiPayout.CustomerExistAsDriver
  DR.MultipleDeviceIdExists -> ApiPayout.MultipleDeviceIdExists

postPayoutPayoutRetry ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Id.Id PayoutRequest.PayoutRequest ->
  Maybe Text ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutRetry merchantShortId opCity payoutRequestId _mbRequestorId = do
  let (_history :<|> _getById :<|> retry :<|> _cancel :<|> _cash :<|> _deleteVpa :<|> _updateVpa :<|> _refund) =
        payoutServer merchantShortId opCity
  retry payoutRequestId

postPayoutPayoutCancel ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Id.Id PayoutRequest.PayoutRequest ->
  Maybe Text ->
  PayoutTypes.PayoutCancelReq ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutCancel merchantShortId opCity payoutRequestId _mbRequestorId req = do
  let (_history :<|> _getById :<|> _retry :<|> cancelPayout :<|> _cash :<|> _deleteVpa :<|> _updateVpa :<|> _refund) =
        payoutServer merchantShortId opCity
  cancelPayout payoutRequestId req

postPayoutPayoutCash ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Id.Id PayoutRequest.PayoutRequest ->
  Maybe Text ->
  PayoutTypes.PayoutCashUpdateReq ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutCash merchantShortId opCity payoutRequestId _mbRequestorId req = do
  let (_history :<|> _getById :<|> _retry :<|> _cancel :<|> markCash :<|> _deleteVpa :<|> _updateVpa :<|> _refund) =
        payoutServer merchantShortId opCity
  markCash payoutRequestId req

postPayoutPayoutVpaDelete ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Text ->
  PayoutTypes.DeleteVpaReq ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutVpaDelete merchantShortId opCity _mbRequestorId req = do
  let (_history :<|> _getById :<|> _retry :<|> _cancel :<|> _cash :<|> deleteVpa :<|> _updateVpa :<|> _refund) =
        payoutServer merchantShortId opCity
  deleteVpa req

postPayoutPayoutVpaUpdate ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Text ->
  PayoutTypes.UpdateVpaReq ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutVpaUpdate merchantShortId opCity _mbRequestorId req = do
  let (_history :<|> _getById :<|> _retry :<|> _cancel :<|> _cash :<|> _deleteVpa :<|> updateVpa :<|> _refund) =
        payoutServer merchantShortId opCity
  updateVpa req

postPayoutPayoutVpaRefundRegistration ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Text ->
  PayoutTypes.RefundRegAmountReq ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutVpaRefundRegistration merchantShortId opCity _mbRequestorId req = do
  let (_history :<|> _getById :<|> _retry :<|> _cancel :<|> _cash :<|> _deleteVpa :<|> _updateVpa :<|> refundReg) =
        payoutServer merchantShortId opCity
  refundReg req

postPayoutPayoutScheduledPayoutConfigUpsert ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Text ->
  ApiPayout.UpdateScheduledPayoutConfigReq ->
  Environment.Flow APISuccess
postPayoutPayoutScheduledPayoutConfigUpsert merchantShortId opCity _mbRequestorId apiReq = do
  let domainReq =
        DashboardPayoutRequest.UpdateScheduledPayoutConfigReq
          { payoutCategory = apiReq.payoutCategory,
            isEnabled = apiReq.isEnabled,
            frequency = castFrequency <$> apiReq.frequency,
            dayOfWeek = apiReq.dayOfWeek,
            dayOfMonth = apiReq.dayOfMonth,
            timeOfDay = apiReq.timeOfDay,
            batchSize = apiReq.batchSize,
            minimumPayoutAmount = apiReq.minimumPayoutAmount,
            maxRetriesPerDriver = apiReq.maxRetriesPerDriver,
            vehicleCategory = apiReq.vehicleCategory,
            remark = apiReq.remark,
            orderType = apiReq.orderType,
            timeDiffFromUtc = apiReq.timeDiffFromUtc,
            -- NOTE (reviewer, remove before merge): The new optional fields, passed straight through (absent = stored value
            --   kept); castFrequency below maps the two new frequencies. The upsert's shared behaviour (validation, sweep-job
            --   create/move, cache clear) is in DashboardPayoutRequest.upsertScheduledPayoutConfig.
            intervalHours = apiReq.intervalHours,
            intervalDays = apiReq.intervalDays,
            rescheduleBufferMinutes = apiReq.rescheduleBufferMinutes,
            bufferCheckEnabled = apiReq.bufferCheckEnabled,
            itemsPerBatchLimit = apiReq.itemsPerBatchLimit,
            defaultPayoutRail = apiReq.defaultPayoutRail,
            bulkStatusCheckIntervalMinutes = apiReq.bulkStatusCheckIntervalMinutes,
            bulkStatusCheckBatchLimit = apiReq.bulkStatusCheckBatchLimit
          }
  DashboardPayoutRequest.upsertScheduledPayoutConfig merchantShortId opCity domainReq
  where
    castFrequency :: ApiPayout.ScheduledPayoutFrequency -> DSPC.ScheduledPayoutFrequency
    castFrequency = \case
      ApiPayout.DAILY -> DSPC.DAILY
      ApiPayout.WEEKLY -> DSPC.WEEKLY
      ApiPayout.MONTHLY -> DSPC.MONTHLY
      ApiPayout.HOURLY -> DSPC.HOURLY
      ApiPayout.EVERY_N_DAYS -> DSPC.EVERY_N_DAYS

-- NOTE (reviewer, remove before merge): New read-only endpoint (VIEW / VIEW_DIFF); nothing is written. Main has no such
--   route; Juspay/Stripe cities can call it like any other. Known Low issue: a VIEW right after a create can cache
--   "no config" for up to 2 hours (see PayoutRequest.viewScheduledPayoutConfigs). command / frequency / payoutCategory
--   arrive as Text and are parsed here, because the generated enum query-param instances only accept a JSON-quoted value.

-- | The read side of the config the upsert above commits: VIEW returns what is stored, VIEW_DIFF
--   returns what the supplied values would change and what that would do to the queued job. Both are
--   reads; nothing here writes.
getPayoutPayoutScheduledPayoutConfig ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Bool ->
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
  Maybe Text ->
  Environment.Flow ApiPayout.ScheduledPayoutConfigViewResp
getPayoutPayoutScheduledPayoutConfig merchantShortId opCity mbBatchSize mbBufferCheckEnabled mbRawCommand mbDayOfMonth mbDayOfWeek mbRawFrequency mbIntervalDays mbIntervalHours mbIsEnabled mbMinimumPayoutAmount mbRawPayoutCategory mbRescheduleBufferMinutes mbTimeOfDay _mbRequestorId = do
  -- Annotated rather than inferred: each of these is a Text on the wire and an enum inside, and the
  -- signature is the only place that says which enum.
  mbCommand :: Maybe ApiPayout.PayoutConfigCommand <- parseEnumParam "command" "VIEW, VIEW_DIFF" mbRawCommand
  mbFrequency :: Maybe ApiPayout.ScheduledPayoutFrequency <- parseEnumParam "frequency" "DAILY, WEEKLY, MONTHLY, HOURLY, EVERY_N_DAYS" mbRawFrequency
  mbPayoutCategory :: Maybe DPayment.EntityName <- parseEnumParam "payoutCategory" "e.g. DRIVER_WALLET_TRANSACTION" mbRawPayoutCategory
  case fromMaybe ApiPayout.VIEW mbCommand of
    ApiPayout.VIEW -> do
      configs <- DashboardPayoutRequest.viewScheduledPayoutConfigs merchantShortId opCity mbPayoutCategory
      pure ApiPayout.ScheduledPayoutConfigViewResp {configs = map toConfigEntity configs, diff = Nothing, scheduleEffect = Nothing}
    ApiPayout.VIEW_DIFF -> do
      -- A diff is always against one stored config, so the category is required here even though VIEW
      -- can answer for the whole city.
      payoutCategory <- mbPayoutCategory & fromMaybeM (InvalidRequest "payoutCategory is required for VIEW_DIFF")
      let domainReq =
            DashboardPayoutRequest.UpdateScheduledPayoutConfigReq
              { payoutCategory = payoutCategory,
                isEnabled = mbIsEnabled,
                frequency = castFrequency <$> mbFrequency,
                dayOfWeek = mbDayOfWeek,
                dayOfMonth = mbDayOfMonth,
                timeOfDay = mbTimeOfDay,
                intervalHours = mbIntervalHours,
                intervalDays = mbIntervalDays,
                rescheduleBufferMinutes = mbRescheduleBufferMinutes,
                bufferCheckEnabled = mbBufferCheckEnabled,
                batchSize = mbBatchSize,
                minimumPayoutAmount = mbMinimumPayoutAmount,
                -- Not previewable from a GET's query params: they do not affect the schedule, so the
                -- diff simply reports them unchanged.
                maxRetriesPerDriver = Nothing,
                vehicleCategory = Nothing,
                remark = Nothing,
                orderType = Nothing,
                timeDiffFromUtc = Nothing,
                itemsPerBatchLimit = Nothing,
                defaultPayoutRail = Nothing,
                bulkStatusCheckIntervalMinutes = Nothing,
                bulkStatusCheckBatchLimit = Nothing
              }
      (proposed, changes, effect) <- DashboardPayoutRequest.diffScheduledPayoutConfig merchantShortId opCity domainReq
      pure
        ApiPayout.ScheduledPayoutConfigViewResp
          { configs = [toConfigEntity proposed],
            diff = Just (map toFieldDiff changes),
            scheduleEffect = Just (toScheduleEffect effect)
          }
  where
    -- Read the enum from its bare name, the way a caller writes it in a URL. 'Read' matches the
    -- constructor exactly, so this accepts DAILY and rejects daily -- deliberate: guessing at case
    -- would make ?frequency=Daily quietly mean something, and the error names the alternatives.
    parseEnumParam :: Read a => Text -> Text -> Maybe Text -> Environment.Flow (Maybe a)
    parseEnumParam paramName allowed = \case
      Nothing -> pure Nothing
      Just raw ->
        readMaybe (T.unpack raw)
          & fromMaybeM (InvalidRequest $ paramName <> " must be one of " <> allowed <> "; got " <> raw)
          <&> Just
    toFieldDiff d = ApiPayout.ScheduledPayoutConfigFieldDiff {field = d.field, from = d.from, to = d.to}
    toScheduleEffect e =
      let (decidedAction, mbQueuedAt) = case e.decision of
            Retime.NoQueuedJob -> (ApiPayout.NO_QUEUED_JOB, Nothing)
            Retime.LeaveInsideBuffer queuedAt -> (ApiPayout.LEAVE_INSIDE_BUFFER, Just queuedAt)
            Retime.MoveJobTo queuedAt _ -> (ApiPayout.MOVE_JOB, Just queuedAt)
            Retime.NotMoved mbQueued -> (ApiPayout.NOT_MOVED, mbQueued)
       in ApiPayout.ScheduledPayoutScheduleEffect
            { currentNextRunAt = e.currentNextRunAt,
              proposedNextRunAt = e.proposedNextRunAt,
              queuedJobAt = mbQueuedAt,
              action = decidedAction,
              bufferMinutes = e.bufferMinutes
            }
    toConfigEntity c =
      ApiPayout.ScheduledPayoutConfigAPIEntity
        { payoutCategory = c.payoutCategory,
          isEnabled = c.isEnabled,
          frequency = uncastFrequency c.frequency,
          dayOfWeek = c.dayOfWeek,
          dayOfMonth = c.dayOfMonth,
          timeOfDay = c.timeOfDay,
          intervalHours = c.intervalHours,
          intervalDays = c.intervalDays,
          batchSize = c.batchSize,
          itemsPerBatchLimit = c.itemsPerBatchLimit,
          minimumPayoutAmount = c.minimumPayoutAmount,
          maxRetriesPerDriver = c.maxRetriesPerDriver,
          defaultPayoutRail = c.defaultPayoutRail,
          timeDiffFromUtc = c.timeDiffFromUtc,
          rescheduleBufferMinutes = c.rescheduleBufferMinutes,
          bufferCheckEnabled = c.bufferCheckEnabled,
          bulkStatusCheckIntervalMinutes = c.bulkStatusCheckIntervalMinutes,
          bulkStatusCheckBatchLimit = c.bulkStatusCheckBatchLimit,
          vehicleCategory = c.vehicleCategory,
          orderType = c.orderType,
          remark = c.remark,
          updatedAt = c.updatedAt
        }
    castFrequency :: ApiPayout.ScheduledPayoutFrequency -> DSPC.ScheduledPayoutFrequency
    castFrequency = \case
      ApiPayout.DAILY -> DSPC.DAILY
      ApiPayout.WEEKLY -> DSPC.WEEKLY
      ApiPayout.MONTHLY -> DSPC.MONTHLY
      ApiPayout.HOURLY -> DSPC.HOURLY
      ApiPayout.EVERY_N_DAYS -> DSPC.EVERY_N_DAYS
    uncastFrequency :: DSPC.ScheduledPayoutFrequency -> ApiPayout.ScheduledPayoutFrequency
    uncastFrequency = \case
      DSPC.DAILY -> ApiPayout.DAILY
      DSPC.WEEKLY -> ApiPayout.WEEKLY
      DSPC.MONTHLY -> ApiPayout.MONTHLY
      DSPC.HOURLY -> ApiPayout.HOURLY
      DSPC.EVERY_N_DAYS -> ApiPayout.EVERY_N_DAYS

-- NOTE (reviewer, remove before merge): Thin handlers for the new adhoc and batch endpoints; the logic is in
--   Domain/Action/Dashboard/AdhocPayout.hs and Domain/Action/Dashboard/PayoutBatch.hs. Adhoc is bulk-only (refused in a
--   Juspay/Stripe city) and checks the URL's city; the batch and excluded views read only bulk-written rows. No Juspay/Stripe
--   behaviour change.
getPayoutAdhocLookup ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Text ->
  Text ->
  Environment.Flow ApiPayout.AdhocPayoutLookupResp
getPayoutAdhocLookup merchantShortId opCity _mbRequestorId personIdText =
  AdhocPayout.lookupPayoutEligibility merchantShortId opCity (Id.Id personIdText)

postPayoutAdhocInitiate ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Text ->
  ApiPayout.AdhocPayoutInitiateReq ->
  Environment.Flow ApiPayout.AdhocPayoutInitiateResp
postPayoutAdhocInitiate merchantShortId opCity _mbRequestorId req =
  AdhocPayout.initiateAdhocPayouts merchantShortId opCity (map Id.Id req.personIds)

-- | Arg order is the generator's, not YAML order: optional query params alphabetically, the
--   requestor, then the mandatory ones (the execution-date range) alphabetically.
getPayoutBatchList ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe DPayoutBatch.PayoutBatchOrigin ->
  Maybe DPayoutBatch.PayoutBatchRail ->
  Maybe DPayoutBatch.PayoutBatchStatus ->
  Maybe Text ->
  Day ->
  Day ->
  Environment.Flow ApiPayout.PayoutBatchListRes
getPayoutBatchList merchantShortId opCity limit offset origin payoutRail status _mbRequestorId fromDate toDate =
  PayoutBatch.listPayoutBatches merchantShortId opCity limit offset fromDate toDate status origin payoutRail

getPayoutBatchOrders ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Text ->
  Maybe Text ->
  Environment.Flow ApiPayout.PayoutBatchOrdersRes
getPayoutBatchOrders merchantShortId opCity batchId _mbRequestorId =
  PayoutBatch.listPayoutBatchOrders merchantShortId opCity batchId

getPayoutExcluded ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Day ->
  Maybe Int ->
  Maybe Int ->
  Maybe Day ->
  Maybe Text ->
  Environment.Flow ApiPayout.PayoutExcludedRes
getPayoutExcluded merchantShortId opCity from limit offset to _mbRequestorId =
  PayoutBatch.listPayoutExcluded merchantShortId opCity limit offset from to
