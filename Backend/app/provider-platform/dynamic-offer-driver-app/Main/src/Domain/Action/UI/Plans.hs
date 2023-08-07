module Domain.Action.UI.Plans where

import Domain.Action.UI.Payment (createMandateOrder)
import Domain.Types.DriverFee
import qualified Domain.Types.DriverFee as DF
import Domain.Types.DriverPlan
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Domain.Types.PlanDetails
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq hiding (Value)
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDInfo
import qualified Storage.Queries.DriverPlan as QDPlan
import qualified Storage.Queries.PlanDetails as QPD
import Tools.Error
import qualified Tools.Payment as Payment

--- subscription ---

--- To do clarify the amount calculactions and send appropriate amounts ---

subcribe ::
  (Id DP.Person, Id DM.Merchant) ->
  Id PlanDetails ->
  PaymentMode ->
  Flow SubscriptionResponse
subcribe (driverId, merchantId) planId paymentMode = do
  plan <- runInReplica $ QPD.findByIdAndPaymentMode planId paymentMode >>= fromMaybeM (PlanNotFound planId.getId)
  driverPlan <- runInReplica $ QDPlan.findByDriverIdAndPlanId driverId planId
  runNoTransaction $ QDInfo.updatePlanPaymentMode paymentMode (cast driverId)
  when (maybe False (\dp -> dp.planStatus == ACTIVE_PLAN) driverPlan) (throwError $ InvalidPlanStatus driverId.getId)
  now <- getCurrentTime
  driverRegisterationInvoice <- mkDriverFeeForMandate 1 driverId --- amount is rs. 1 needs to be verified ---
  case plan.paymentMode of
    AUTOPAY -> do
      case driverPlan of
        Just dPlan -> do
          when (maybe True checkMandateStatus (dPlan.mandateStatus)) (throwError $ InvalidMandateStatus driverId.getId)
          runNoTransaction $ QDF.create driverRegisterationInvoice
          runNoTransaction $ QDPlan.updatePlanIdByDriverId driverId planId
          order <- createMandateOrder (driverId, merchantId) driverRegisterationInvoice.id 1.0 --- amounts/fees need to be refactored these are just dummies for dev ---
          return $ buildSubscriptionResponse (Just order)
        Nothing -> do
          runNoTransaction $ QDF.create driverRegisterationInvoice
          newDriverPlan <- mkDriverPlanEntry planId driverId paymentMode INACTIVE_PLAN now 0 --- amounts/fees need to be refactored these are just dummies for dev ---
          runNoTransaction $ QDPlan.create newDriverPlan
          order <- createMandateOrder (driverId, merchantId) driverRegisterationInvoice.id 1.0 --- amounts/fees need to be refactored these are just dummies for dev ---
          return $ buildSubscriptionResponse (Just order)
    MANUAL -> do
      case driverPlan of
        Just dPlan -> do
          when (maybe False checkMandateStatus (dPlan.mandateStatus)) (throwError $ InvalidMandateStatus driverId.getId)
          runNoTransaction $ QDPlan.updatePlanIdByDriverId driverId planId
          return $ buildSubscriptionResponse Nothing
        Nothing -> do
          newDriverPlan <- mkDriverPlanEntry planId driverId paymentMode INACTIVE_PLAN now 0
          runNoTransaction $ QDPlan.create newDriverPlan
          return $ buildSubscriptionResponse Nothing

select ::
  (Id DP.Person, Id DM.Merchant) ->
  Id PlanDetails ->
  PaymentMode ->
  Flow AckResponse
select (driverId, _) planId paymentMode = do
  driverInfo <- runInReplica $ QDInfo.findById (cast driverId) >>= fromMaybeM DriverInfoNotFound
  currentDriverPlan <- runInReplica $ QDPlan.findByDriverId driverId >>= fromMaybeM (NoCurrentPlanForDriver driverId.getId)
  when (currentDriverPlan.planStatus == INACTIVE_PLAN) (throwError $ InvalidPlanStatus driverId.getId)
  case driverInfo.planPaymentMode of
    MANUAL -> do
      when (paymentMode /= MANUAL) (throwError $ InvalidMandateStatus driverId.getId)
      runNoTransaction $ QDPlan.updatePlanIdByDriverId driverId planId
      return Ack
    AUTOPAY -> do
      when (paymentMode /= AUTOPAY) (throwError $ InvalidMandateStatus driverId.getId)
      unless (maybe False checkMandateStatus (currentDriverPlan.mandateStatus)) (throwError $ InvalidMandateStatus driverId.getId)
      runNoTransaction $ QDPlan.updatePlanIdByDriverId driverId planId
      return Ack

upgrade ::
  (Id DP.Person, Id DM.Merchant) ->
  Flow Payment.CreateOrderResp
upgrade (driverId, merchantId) = do
  currentDriverPlan <- runInReplica $ QDPlan.findByDriverId driverId >>= fromMaybeM (NoCurrentPlanForDriver driverId.getId)
  driverInfo <- runInReplica $ QDInfo.findById (cast driverId) >>= fromMaybeM DriverInfoNotFound
  when (driverInfo.planPaymentMode == AUTOPAY) (throwError InvalidPaymentMode)
  unless (currentDriverPlan.planStatus == ACTIVE_PLAN) (throwError $ InvalidPlanStatus driverId.getId)
  when (maybe True checkMandateStatus (currentDriverPlan.mandateStatus)) (throwError $ InvalidMandateStatus driverId.getId)
  runNoTransaction $ QDPlan.updatePaymentModeAndStatusByDriverId driverId AUTOPAY INACTIVE_PLAN
  driverRegisterationInvoice <- mkDriverFeeForMandate 1 driverId --- amount is rs. 1 needs to be verified ---
  createMandateOrder (driverId, merchantId) driverRegisterationInvoice.id 1.0

checkMandateStatus :: MandateStatus -> Bool
checkMandateStatus mandateStatus = mandateStatus `elem` [ACTIVE, PAUSED, CREATED]

mkDriverFeeForMandate ::
  ( MonadFlow m
  ) =>
  Money ->
  Id DP.Person ->
  m DF.DriverFee
mkDriverFeeForMandate amount driverId = do
  id <- generateGUID
  shortId <- generateShortId
  now <- getCurrentTime
  return $
    DF.DriverFee
      { payBy = now,
        status = DF.PAYMENT_PENDING,
        numRides = 0,
        createdAt = now,
        updatedAt = now,
        platformFee = DF.PlatformFee amount 0.0 0.0, --- amounts/fees need to be refactored these are just dummies for dev ---
        totalEarnings = 0,
        feeType = DF.MANDATE_REGISTRATION,
        govtCharges = 0, --- amounts/fees need to be refactored these are just dummies for dev ---
        id = id,
        shortId = shortId,
        startTime = now,
        endTime = now,
        driverId = cast driverId
      }

mkDriverPlanEntry ::
  ( MonadFlow m
  ) =>
  Id PlanDetails ->
  Id DP.Person ->
  PaymentMode ->
  PlanStatus ->
  UTCTime ->
  Int ->
  m DriverPlan
mkDriverPlanEntry planId driverId paymentMode planStatus now maxAmount = do
  id <- generateGUID
  return $
    DriverPlan
      { id = id,
        driverId = cast driverId,
        planId = planId,
        planType = paymentMode,
        mandateId = Nothing,
        mandateStatus = Nothing,
        planStatus = planStatus,
        activatedAt = Nothing,
        endAt = Nothing,
        resumeDate = Nothing,
        createdAt = now,
        updatedAt = now,
        maxAmount = maxAmount
      }

data SubscriptionResponse = SubscriptionResponse
  { status :: AckResponse,
    orderResp :: Maybe Payment.CreateOrderResp
  }

buildSubscriptionResponse ::
  Maybe Payment.CreateOrderResp ->
  SubscriptionResponse
buildSubscriptionResponse order =
  SubscriptionResponse
    { status = Ack,
      orderResp = order
    }
