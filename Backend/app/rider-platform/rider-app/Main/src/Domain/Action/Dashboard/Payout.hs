{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Payout
  ( getPayoutPayoutOrder,
    postPayoutPayoutRetrigger,
  )
where

import qualified API.Types.RiderPlatform.Management.Payout as API
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import qualified Data.Text as T
import Data.Time (minutesToTimeZone, utcToLocalTime)
import qualified Domain.Action.UI.CustomerReferral as CustomerReferral
import qualified Domain.Action.UI.Payout as UIPayout
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PersonStats as DPS
import qualified Domain.Types.VehicleCategory as DV
import Environment
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payout.Interface as KPayout
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common (Minutes)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified Lib.Payment.API.Payout.Types as PayoutTypes
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DLP
import qualified Lib.Payment.Domain.Types.PayoutOrder as PayoutOrder
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified Lib.Payment.Storage.Queries.PayoutOrderExtra as QPayoutOrderExtra
import qualified SharedLogic.Finance.CashbackPayout as CashbackPayout
import SharedLogic.PayoutStatusCheck (afterPayoutOrderCreated)
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.PayoutConfig (PayoutConfigDimensions (..))
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonStats as QPersonStats
import Tools.Error
import qualified Tools.Payout as TPayout

getPayoutPayoutOrder ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Text ->
  Flow PayoutTypes.PayoutOrderResp
getPayoutPayoutOrder merchantShortId opCity payoutOrderId _mbRequestorId = do
  (merchant, _, timeZoneDiff) <- resolveMerchantOpCityAndTz merchantShortId opCity
  payoutOrder <- QPayoutOrder.findByOrderId payoutOrderId >>= fromMaybeM (PayoutOrderNotFound payoutOrderId)
  unless (payoutOrder.merchantId == merchant.id.getId) $
    throwError $ PayoutOrderNotFound payoutOrderId
  unless (payoutOrder.city == show opCity) $
    throwError $ PayoutOrderNotFound payoutOrderId
  refreshedOrder <- UIPayout.refreshPayoutOrderWithSettlement payoutOrder
  buildPayoutOrderResp timeZoneDiff refreshedOrder

resolveMerchantOpCityAndTz ::
  ShortId DM.Merchant ->
  Context.City ->
  Flow (DM.Merchant, DMOC.MerchantOperatingCity, Minutes)
resolveMerchantOpCityAndTz merchantShortId opCity = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  riderConfig <-
    getConfig
      (RiderConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId})
      Nothing
      >>= fromMaybeM (RiderConfigDoesNotExist $ "merchantOperatingCityId:- " <> merchantOpCity.id.getId)
  pure (merchant, merchantOpCity, secondsToMinutes riderConfig.timeDiffFromUtc)

buildPayoutOrderResp ::
  Minutes ->
  PayoutOrder.PayoutOrder ->
  Flow PayoutTypes.PayoutOrderResp
buildPayoutOrderResp timeZoneDiff payoutOrder = do
  let timeZone = minutesToTimeZone timeZoneDiff.getMinutes
  person <- QPerson.findById (Id payoutOrder.customerId) >>= fromMaybeM (PersonNotFound payoutOrder.customerId)
  phoneNo <- decrypt payoutOrder.mobileNo
  pure
    PayoutTypes.PayoutOrderResp
      { payoutOrderId = payoutOrder.orderId,
        payoutOrderDbId = payoutOrder.id,
        driverId = payoutOrder.customerId,
        driverName = fromMaybe "" person.firstName,
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

-- | Re-sends payout orders Juspay never received (status API 404 E09) under the same order id.
postPayoutPayoutRetrigger ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  API.RetriggerPayoutReq ->
  Flow API.RetriggerPayoutResp
postPayoutPayoutRetrigger merchantShortId opCity _mbRequestorId req = do
  merchantOpCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let orderIds = nub req.orderIds
  when (null orderIds) $ throwError (InvalidRequest "orderIds must not be empty")
  when (length orderIds > maxRetriggerOrders) $ throwError (InvalidRequest $ "Too many orders in one retrigger request (max " <> show maxRetriggerOrders <> ")")
  validated <- forM orderIds $ \orderId -> (orderId,) <$> validateOrder merchantOpCity orderId
  let groups = Map.fromListWith (<>) [(groupKey kind order, [order]) | (_, Right (order, kind)) <- validated]
  groupResults <- Map.fromList . concat <$> forM (Map.toList groups) (\((_, kind, _), orders) -> retriggerGroup kind orders)
  let results = flip map validated $ \case
        (orderId, Left (status, message)) -> mkResult orderId Nothing Nothing status message
        (orderId, Right (order, _)) ->
          let (status, message) = fromMaybe (API.FAILED, Nothing) (Map.lookup orderId groupResults)
           in mkResult orderId (Just order.customerId) (show <$> order.entityName) status message
  pure API.RetriggerPayoutResp {results}
  where
    groupKey :: RetriggerKind -> PayoutOrder.PayoutOrder -> (Text, RetriggerKind, Maybe Text)
    groupKey kind order = (order.customerId, kind, if kind == RetriggerReferralAward then Just order.orderId else Nothing)

    mkResult :: Text -> Maybe Text -> Maybe Text -> API.RetriggerPayoutStatus -> Maybe Text -> API.RetriggerPayoutResult
    mkResult orderId customerId entityName status message = API.RetriggerPayoutResult {orderId, customerId, entityName, status, message}

maxRetriggerOrders :: Int
maxRetriggerOrders = 50

data RetriggerKind = RetriggerCashback | RetriggerReferredBy | RetriggerReferralAward
  deriving (Show, Eq, Ord)

type RetriggerResult = (API.RetriggerPayoutStatus, Maybe Text)

validateOrder :: DMOC.MerchantOperatingCity -> Text -> Flow (Either RetriggerResult (PayoutOrder.PayoutOrder, RetriggerKind))
validateOrder merchantOpCity orderId =
  QPayoutOrder.findByOrderId orderId >>= \case
    Nothing -> pure $ Left (API.ORDER_NOT_FOUND, Nothing)
    Just order
      | order.merchantId /= merchantOpCity.merchantId.getId || order.city /= show merchantOpCity.city -> pure $ Left (API.ORDER_NOT_FOUND, Nothing)
      | Just retriedOrderId <- order.retriedOrderId -> pure $ Left (API.ALREADY_RETRIGGERED, Just $ "Already retried as order " <> retriedOrderId)
      | otherwise -> case retriggerKind order.entityName of
        Nothing -> pure $ Left (API.UNSUPPORTED_TYPE, Just $ "No retrigger path for entity: " <> maybe "none" show order.entityName)
        Just kind
          | not (DPayment.isNeverSentPayoutOrder order) -> pure $ Left (API.NOT_E09, Just $ "Juspay has this order, status: " <> show order.status)
          | otherwise ->
            withTryCatch "retriggerPayout:payoutStatus" (UIPayout.refreshPayoutOrderWithSettlement order) <&> \case
              Left err
                | isOrderNotFoundAtPG (T.pack (displayException err)) -> Right (order, kind)
                | otherwise -> Left (API.STATUS_CHECK_FAILED, Just $ "Juspay status check failed: " <> T.pack (displayException err))
              Right refreshed -> Left (API.NOT_E09, Just $ "Juspay has this order, status: " <> show refreshed.status)
  where
    isOrderNotFoundAtPG :: Text -> Bool
    isOrderNotFoundAtPG errText = "statusCode = 404" `T.isInfixOf` errText && "E09" `T.isInfixOf` errText

    retriggerKind :: Maybe DLP.EntityName -> Maybe RetriggerKind
    retriggerKind = \case
      Just DLP.RIDE_OFFER_CASHBACK -> Just RetriggerCashback
      Just DLP.REFERRAL_AWARD_RIDE -> Just RetriggerReferralAward
      Just entityName | entityName `elem` [DLP.REFERRED_BY_AWARD, DLP.BACKLOG, DLP.REFERRED_BY_AND_BACKLOG_AWARD] -> Just RetriggerReferredBy
      _ -> Nothing

data RetriggerOutcome
  = NotSent RetriggerResult
  | Sent Bool -- True = re-sent under the same order id

retriggerGroup :: RetriggerKind -> [PayoutOrder.PayoutOrder] -> Flow [(Text, RetriggerResult)]
retriggerGroup kind orders = case sortOn (Down . (.createdAt)) orders of
  [] -> pure []
  lead : covered -> do
    startedAt <- getCurrentTime
    outcome <-
      withTryCatch "retriggerPayout" (retriggerLead kind (map (.entityName) orders) lead)
        <&> either (\err -> NotSent (API.FAILED, Just (T.pack (displayException err)))) identity
    (leadResult, mbPaidByOrderId) <- case outcome of
      NotSent result -> pure (result, Nothing)
      Sent True -> checkResent lead >>= orPaidMeanwhile lead
      Sent False -> linkNewOrder startedAt lead >>= orPaidMeanwhile lead
    logTagInfo "dashboard -> retriggerPayout" $ "customerId: " <> lead.customerId <> " kind: " <> show kind <> " lead: " <> lead.orderId <> " result: " <> show (fst leadResult)
    coveredResults <- forM covered $ \order -> case mbPaidByOrderId of
      Just paidByOrderId -> do
        QPayoutOrder.updateRetriedOrderId (Just paidByOrderId) order.orderId
        pure (order.orderId, (API.ALREADY_TRIGGERED, Just $ "Covered by order " <> paidByOrderId))
      Nothing -> pure (order.orderId, (fst leadResult, Just $ "Same rider as order " <> lead.orderId))
    pure $ (lead.orderId, leadResult) : coveredResults
  where
    checkResent :: PayoutOrder.PayoutOrder -> Flow (RetriggerResult, Maybe Text)
    checkResent lead =
      QPayoutOrder.findByOrderId lead.orderId <&> \case
        Just order
          | not (DPayment.isNeverSentPayoutOrder order) -> ((API.RESENT, Just $ "Juspay status: " <> show order.status), Just lead.orderId)
        _ -> ((API.FAILED, Just "Re-send did not reach Juspay, check the rider-app logs"), Nothing)

    -- A concurrent cashback job may have already paid the pending amount.
    orPaidMeanwhile :: PayoutOrder.PayoutOrder -> (RetriggerResult, Maybe Text) -> Flow (RetriggerResult, Maybe Text)
    orPaidMeanwhile lead result@((status, _), _)
      | status == API.FAILED && kind == RetriggerCashback = do
        person <- QPerson.findById (Id lead.customerId) >>= fromMaybeM (PersonNotFound lead.customerId)
        CashbackPayout.checkCashbackPayout person <&> \case
          CashbackPayout.NoPendingCashback -> ((API.NOTHING_PENDING, Just "Paid by another cashback payout meanwhile"), Nothing)
          _ -> result
      | otherwise = pure result

    linkNewOrder :: UTCTime -> PayoutOrder.PayoutOrder -> Flow (RetriggerResult, Maybe Text)
    linkNewOrder startedAt lead = do
      recentOrders <- QPayoutOrderExtra.findAllByCustomerIdWithLimitOffset (Just 5) Nothing lead.customerId
      case find (\order -> order.orderId /= lead.orderId && order.createdAt >= startedAt) recentOrders of
        Just newOrder -> do
          QPayoutOrder.updateRetriedOrderId (Just newOrder.orderId) lead.orderId
          pure ((API.TRIGGERED, Just $ "Amount changed, paid as order " <> newOrder.orderId), Just newOrder.orderId)
        Nothing -> pure ((API.FAILED, Just "No new payout order was created"), Nothing)

retriggerLead :: RetriggerKind -> [Maybe DLP.EntityName] -> PayoutOrder.PayoutOrder -> Flow RetriggerOutcome
retriggerLead kind entityNames order = do
  person <- QPerson.findById (Id order.customerId) >>= fromMaybeM (PersonNotFound order.customerId)
  case kind of
    RetriggerCashback -> retriggerCashback person order
    RetriggerReferredBy -> retriggerReferredBy person (catMaybes entityNames) order
    RetriggerReferralAward -> retriggerReferralAward person order

reuseIfAmountMatches :: PayoutOrder.PayoutOrder -> HighPrecMoney -> Maybe Text
reuseIfAmountMatches order owed = if owed == order.amount.amount then Just order.orderId else Nothing

retriggerCashback :: DP.Person -> PayoutOrder.PayoutOrder -> Flow RetriggerOutcome
retriggerCashback person order =
  CashbackPayout.checkCashbackPayout person >>= \case
    CashbackPayout.NoPayoutVpa -> pure $ NotSent (API.NO_VPA, Nothing)
    CashbackPayout.NoPendingCashback -> pure $ NotSent (API.NOTHING_PENDING, Just "No unsettled cashback")
    CashbackPayout.NoPayoutConfig -> pure $ NotSent (API.FAILED, Just "Payout config not found")
    CashbackPayout.Payable plan -> do
      let mbReuseOrderId = reuseIfAmountMatches order plan.totalAmount
      CashbackPayout.runCashbackPayout person.id mbReuseOrderId
      pure $ Sent (isJust mbReuseOrderId)

retriggerReferredBy :: DP.Person -> [DLP.EntityName] -> PayoutOrder.PayoutOrder -> Flow RetriggerOutcome
retriggerReferredBy person entityNames order = case person.payoutVpa of
  Nothing -> pure $ NotSent (API.NO_VPA, Nothing)
  Just vpa -> do
    personStats <- QPersonStats.findByPersonId person.id >>= fromMaybeM (PersonStatsNotFound person.id.getId)
    let coversReferredBy = any (`elem` [DLP.REFERRED_BY_AWARD, DLP.REFERRED_BY_AND_BACKLOG_AWARD]) entityNames
        coversBacklog = any (`elem` [DLP.BACKLOG, DLP.REFERRED_BY_AND_BACKLOG_AWARD]) entityNames
        isRetriable = (`elem` [Just DPS.Processing, Just DPS.Failed])
        resetReferredBy = coversReferredBy && isRetriable personStats.referredByEarningsPayoutStatus
        resetBacklog = coversBacklog && isRetriable personStats.backlogPayoutStatus
        referredByStatus = if resetReferredBy then Nothing else personStats.referredByEarningsPayoutStatus
        backlogStatus = if resetBacklog then Nothing else personStats.backlogPayoutStatus
        owedReferredBy = if personStats.referredByEarnings > 0 && isNothing referredByStatus then personStats.referredByEarnings else 0
        owedBacklog = if personStats.backlogPayoutAmount > 0 && isNothing backlogStatus then personStats.backlogPayoutAmount else 0
        owed = owedReferredBy + owedBacklog
    if owed <= 0
      then pure $ NotSent (API.NOTHING_PENDING, Just $ "referredByStatus: " <> show personStats.referredByEarningsPayoutStatus <> ", backlogStatus: " <> show personStats.backlogPayoutStatus)
      else do
        when resetReferredBy $ QPersonStats.updateReferredByEarningsPayoutStatus Nothing person.id
        when resetBacklog $ QPersonStats.updateBacklogPayoutStatus Nothing person.id
        let mbReuseOrderId = reuseIfAmountMatches order owed
        CustomerReferral.processBacklogReferralPayout person.id vpa person.merchantOperatingCityId mbReuseOrderId
        pure $ Sent (isJust mbReuseOrderId)

retriggerReferralAward :: DP.Person -> PayoutOrder.PayoutOrder -> Flow RetriggerOutcome
retriggerReferralAward person order = case person.payoutVpa of
  Nothing -> pure $ NotSent (API.NO_VPA, Nothing)
  Just vpa -> do
    let merchantOpCityId = maybe person.merchantOperatingCityId Id order.merchantOperatingCityId
    payoutConfig <-
      getOneConfig (PayoutConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, vehicleCategory = Just DV.AUTO_CATEGORY, isPayoutEnabled = Nothing, payoutEntity = Nothing}) Nothing
        >>= fromMaybeM (PayoutConfigNotFound "AUTO_CATEGORY" merchantOpCityId.getId)
    phoneNo <- decrypt order.mobileNo
    emailId <- decrypt order.customerEmail
    let createPayoutOrderReq = DPayment.mkCreatePayoutServiceReq order.orderId order.amount.amount order.amount.currency (Just phoneNo) (Just emailId) order.customerId payoutConfig.remark person.firstName (Just vpa) payoutConfig.orderType KPayout.JuspayFlow Nothing
        createPayoutOrderCall = TPayout.createPayoutOrder person.clientSdkVersion person.merchantId merchantOpCityId (Just person.id.getId)
    void $ DPayment.createPayoutService (Id order.merchantId) (Just $ cast merchantOpCityId) (cast person.id) order.entityIds (Just DLP.REFERRAL_AWARD_RIDE) order.city createPayoutOrderReq createPayoutOrderCall Nothing afterPayoutOrderCreated
    pure $ Sent True
