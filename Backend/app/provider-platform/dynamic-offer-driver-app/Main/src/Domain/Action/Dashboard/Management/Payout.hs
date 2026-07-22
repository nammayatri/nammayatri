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
  )
where

import qualified API.Types.ProviderPlatform.Management.Payout as ApiPayout
import qualified "dashboard-helper-api" Dashboard.Common as DC
import Data.Time (minutesToTimeZone, utcToLocalTime)
import qualified Domain.Action.Common.PayoutRequest as CommonPayout
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
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Payment.API.Payout as PayoutAPI
import qualified Lib.Payment.API.Payout.Types as PayoutTypes
import qualified Lib.Payment.Domain.Types.PayoutOrder as PayoutOrder
import qualified Lib.Payment.Domain.Types.PayoutRequest as PayoutRequest
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import Servant (ServerT, (:<|>) (..))
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QR
import qualified Storage.Queries.RiderDetails as QRD
import qualified Tools.ActorInfo as ActorInfo
import Tools.Error

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
getPayoutPayout merchantShortId opCity payoutRequestId mbRequestorId = ActorInfo.withDashboardMbPersonIdActorInfo ((Id.Id @DP.Person) <$> mbRequestorId) $ do
  let (_history :<|> getById :<|> _retry :<|> _cancel :<|> _cash :<|> _deleteVpa :<|> _updateVpa :<|> _refund) =
        payoutServer merchantShortId opCity
  getById payoutRequestId

getPayoutPayoutOrder ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Text ->
  Environment.Flow PayoutTypes.PayoutOrderResp
getPayoutPayoutOrder merchantShortId opCity payoutOrderIdText = do
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
getPayoutPayoutHistory merchantShortId opCity mbDriverId mbDriverPhoneNo mbFrom mbIsFailedOnly mbLimit mbOffset mbTo mbRequestorId = ActorInfo.withDashboardMbPersonIdActorInfo ((Id.Id @DP.Person) <$> mbRequestorId) $ do
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
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Environment.Flow ApiPayout.PayoutReferralHistoryRes
getPayoutPayoutReferralHistory merchantShortId opCity areActivatedRidesOnly_ mbCustomerPhoneNo mbDriverId_ mbDriverPhoneNo mbFrom mbLimit mbOffset mbTo = do
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
        driver <- QPerson.findByMobileNumberAndMerchantAndRole (P.getCountryMobileCode country) driverNumberHash merchant.id DP.DRIVER >>= fromMaybeM (PersonWithPhoneNotFound driverPhoneNo)
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
postPayoutPayoutRetry merchantShortId opCity payoutRequestId mbRequestorId = ActorInfo.withDashboardMbPersonIdActorInfo ((Id.Id @DP.Person) <$> mbRequestorId) $ do
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
postPayoutPayoutCancel merchantShortId opCity payoutRequestId mbRequestorId req = ActorInfo.withDashboardMbPersonIdActorInfo ((Id.Id @DP.Person) <$> mbRequestorId) $ do
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
postPayoutPayoutCash merchantShortId opCity payoutRequestId mbRequestorId req = ActorInfo.withDashboardMbPersonIdActorInfo ((Id.Id @DP.Person) <$> mbRequestorId) $ do
  let (_history :<|> _getById :<|> _retry :<|> _cancel :<|> markCash :<|> _deleteVpa :<|> _updateVpa :<|> _refund) =
        payoutServer merchantShortId opCity
  markCash payoutRequestId req

postPayoutPayoutVpaDelete ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Text ->
  PayoutTypes.DeleteVpaReq ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutVpaDelete merchantShortId opCity mbRequestorId req = ActorInfo.withDashboardMbPersonIdActorInfo ((Id.Id @DP.Person) <$> mbRequestorId) $ do
  let (_history :<|> _getById :<|> _retry :<|> _cancel :<|> _cash :<|> deleteVpa :<|> _updateVpa :<|> _refund) =
        payoutServer merchantShortId opCity
  deleteVpa req

postPayoutPayoutVpaUpdate ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Text ->
  PayoutTypes.UpdateVpaReq ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutVpaUpdate merchantShortId opCity mbRequestorId req = ActorInfo.withDashboardMbPersonIdActorInfo ((Id.Id @DP.Person) <$> mbRequestorId) $ do
  let (_history :<|> _getById :<|> _retry :<|> _cancel :<|> _cash :<|> _deleteVpa :<|> updateVpa :<|> _refund) =
        payoutServer merchantShortId opCity
  updateVpa req

postPayoutPayoutVpaRefundRegistration ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Text ->
  PayoutTypes.RefundRegAmountReq ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutVpaRefundRegistration merchantShortId opCity mbRequestorId req = ActorInfo.withDashboardMbPersonIdActorInfo ((Id.Id @DP.Person) <$> mbRequestorId) $ do
  let (_history :<|> _getById :<|> _retry :<|> _cancel :<|> _cash :<|> _deleteVpa :<|> _updateVpa :<|> refundReg) =
        payoutServer merchantShortId opCity
  refundReg req

postPayoutPayoutScheduledPayoutConfigUpsert ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiPayout.UpdateScheduledPayoutConfigReq ->
  Environment.Flow APISuccess
postPayoutPayoutScheduledPayoutConfigUpsert merchantShortId opCity apiReq = do
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
            timeDiffFromUtc = apiReq.timeDiffFromUtc
          }
  DashboardPayoutRequest.upsertScheduledPayoutConfig merchantShortId opCity domainReq
  where
    castFrequency :: ApiPayout.ScheduledPayoutFrequency -> DSPC.ScheduledPayoutFrequency
    castFrequency = \case
      ApiPayout.DAILY -> DSPC.DAILY
      ApiPayout.WEEKLY -> DSPC.WEEKLY
      ApiPayout.MONTHLY -> DSPC.MONTHLY
