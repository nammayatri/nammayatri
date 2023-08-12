{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Plan where

import Data.OpenApi (ToSchema (..))
import qualified Domain.Types.DriverFee as DF
import qualified Domain.Types.DriverInformation as DI
import Domain.Types.DriverPlan
import qualified Domain.Types.Mandate as DM
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import Domain.Types.Plan
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified SharedLogic.Payment as SPayment
import qualified Storage.CachedQueries.Merchant.TransporterConfig as QTC
import Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as CDI
import qualified Storage.Queries.DriverPlan as QDPlan
import qualified Storage.Queries.Mandate as QM
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Plan as QPD
import Tools.Error
import Tools.Payment as Payment

---------------------------------------------------------------------------------------------------------
--------------------------------------- Request & Response Types ----------------------------------------
---------------------------------------------------------------------------------------------------------

data PlanListAPIRes = PlanListAPIRes
  { list :: [PlanEntity],
    subscriptionStartTime :: UTCTime
  }
  deriving (Generic, ToJSON, ToSchema)

data PlanEntity = PlanEntity
  { id :: Text,
    name :: Text,
    description :: Text,
    planFareBreakup :: [PlanFareBreakup],
    freeRideCount :: Int,
    frequency :: Text,
    offers :: [OfferEntity],
    paymentMode :: PaymentMode
  }
  deriving (Generic, ToJSON, ToSchema)

data PlanFareBreakup = PlanFareBreakup
  { component :: Text,
    amount :: HighPrecMoney
  }
  deriving (Generic, ToJSON, ToSchema)

data OfferEntity = OfferEntity
  { title :: Maybe Text,
    description :: Maybe Text,
    tnc :: Maybe Text
  }
  deriving (Generic, ToJSON, ToSchema)

newtype CurrentPlanRes = CurrentPlanRes
  { currentPlanDetails :: PlanEntity
  }
  deriving (Generic, ToJSON, ToSchema)

data PlanSubscribeRes = PlanSubscribeRes
  { orderId :: Id DOrder.PaymentOrder,
    orderResp :: Payment.CreateOrderResp
  }
  deriving (Generic, ToJSON, ToSchema)

---------------------------------------------------------------------------------------------------------
--------------------------------------------- Controllers -----------------------------------------------
---------------------------------------------------------------------------------------------------------

-- This API is for listing all the AUTO PAY plans
planList :: (Id SP.Person, Id DM.Merchant) -> Maybe Int -> Maybe Int -> Flow PlanListAPIRes
planList (driverId, merchantId) _mbLimit _mbOffset = do
  plans <- Esq.runInReplica $ QPD.findByMerchantIdAndPaymentMode merchantId AUTOPAY
  transporterConfig <- QTC.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  plansList <- mapM (convertPlanToPlanEntity driverId) plans
  return $
    PlanListAPIRes
      { list = plansList,
        subscriptionStartTime = transporterConfig.subscriptionStartTime
      }

-- This API is for listing current driver plan
currentPlan :: (Id SP.Person, Id DM.Merchant) -> Flow CurrentPlanRes
currentPlan (driverId, _merchantId) = do
  driverInfo <- CDI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  driverPlan <- Esq.runInReplica $ QDPlan.findByDriverId driverId >>= fromMaybeM (NoCurrentPlanForDriver driverId.getId)
  plan <- Esq.runInReplica $ QPD.findByIdAndPaymentMode driverPlan.planId (getDriverPaymentMode driverInfo.autoPayStatus) >>= fromMaybeM (PlanNotFound driverPlan.planId.getId)
  currentPlanEntity <- convertPlanToPlanEntity driverId plan
  return CurrentPlanRes {currentPlanDetails = currentPlanEntity}
  where
    getDriverPaymentMode = \case
      Just DI.ACTIVE -> AUTOPAY
      Just DI.SUSPENDED -> MANUAL
      Just DI.PAUSED_PSP -> MANUAL
      Just DI.CANCELLED_PSP -> MANUAL
      _ -> MANUAL

-- This API is to create a mandate order if the driver has not subscribed to Mandate even once or has Cancelled Mandate from PSP App.
planSubscribe :: Id Plan -> (Id SP.Person, Id DM.Merchant) -> Flow PlanSubscribeRes
planSubscribe planId (driverId, merchantId) = do
  driverInfo <- CDI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  unless (isNothing driverInfo.autoPayStatus || driverInfo.autoPayStatus == Just DI.CANCELLED_PSP) $ throwError InvalidAutoPayStatus
  Esq.runNoTransaction $ CDI.updateAutoPayStatus (Just DI.PENDING) (cast driverId)
  plan <- Esq.runInReplica $ QPD.findByIdAndPaymentMode planId MANUAL >>= fromMaybeM (PlanNotFound planId.getId)
  driverPlan <- Esq.runInReplica $ QDPlan.findByDriverId driverId
  when (isNothing driverPlan) $ do
    newDriverPlan <- mkDriverPlan plan
    Esq.runNoTransaction $ QDPlan.create newDriverPlan
  when (isJust driverPlan) $ do
    Esq.runNoTransaction $ QDPlan.updatePlanIdByDriverId driverId planId
  (createOrderResp, orderId) <- createMandateInvoiceAndOrder driverId merchantId plan
  return $
    PlanSubscribeRes
      { orderId = orderId,
        orderResp = createOrderResp
      }
  where
    mkDriverPlan plan = do
      now <- getCurrentTime
      return $
        DriverPlan
          { driverId = cast driverId,
            planId = plan.id,
            planType = plan.paymentMode,
            mandateId = Nothing,
            createdAt = now,
            updatedAt = now,
            ..
          }

-- This API is to switch between plans of current Payment Method Preference.
planSelect :: Id Plan -> (Id SP.Person, Id DM.Merchant) -> Flow APISuccess
planSelect planId (driverId, _) = do
  void $ Esq.runInReplica $ QDPlan.findByDriverId driverId >>= fromMaybeM (NoCurrentPlanForDriver driverId.getId)
  driverInfo <- CDI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  void $ Esq.runInReplica $ QPD.findByIdAndPaymentMode planId (getDriverPaymentMode driverInfo.autoPayStatus) >>= fromMaybeM (PlanNotFound planId.getId)
  Esq.runNoTransaction $ QDPlan.updatePlanIdByDriverId driverId planId
  return Success
  where
    getDriverPaymentMode = \case
      Just DI.ACTIVE -> AUTOPAY
      Just DI.SUSPENDED -> MANUAL
      Just DI.PAUSED_PSP -> MANUAL
      Just DI.CANCELLED_PSP -> MANUAL
      _ -> MANUAL

-- This API is to make Mandate Inactive and switch to Manual plan type from Autopay.
planSuspend :: (Id SP.Person, Id DM.Merchant) -> Flow APISuccess
planSuspend (driverId, _merchantId) = do
  driverInfo <- CDI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  unless (driverInfo.autoPayStatus == Just DI.ACTIVE) $ throwError InvalidAutoPayStatus
  driverPlan <- Esq.runInReplica $ QDPlan.findByDriverId driverId >>= fromMaybeM (NoCurrentPlanForDriver driverId.getId)
  mandate <- validateActiveMandateExists driverId driverPlan
  Redis.whenWithLockRedis (DF.paymentProcessingLockKey driverPlan.driverId.getId) 60 $
    Esq.runTransaction $ do
      QM.updateStatus mandate.id DM.INACTIVE
      QDPlan.updatePaymentModeByDriverId (cast driverPlan.driverId) MANUAL
      CDI.updateAutoPayStatus (Just DI.SUSPENDED) (cast driverId)
  return Success

-- This API is to make Mandate Active and switch to Autopay plan type. If an only if an Auto Pay plan was paused/cancelled by driver from App.
planResume :: (Id SP.Person, Id DM.Merchant) -> Flow APISuccess
planResume (driverId, _merchantId) = do
  driverInfo <- CDI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  unless (driverInfo.autoPayStatus == Just DI.SUSPENDED) $ throwError InvalidAutoPayStatus
  driverPlan <- Esq.runInReplica $ QDPlan.findByDriverId driverId >>= fromMaybeM (NoCurrentPlanForDriver driverId.getId)
  mandate <- validateInActiveMandateExists driverId driverPlan
  Redis.whenWithLockRedis (DF.paymentProcessingLockKey driverPlan.driverId.getId) 60 $
    Esq.runTransaction $ do
      QM.updateStatus mandate.id DM.ACTIVE
      QDPlan.updatePaymentModeByDriverId (cast driverPlan.driverId) AUTOPAY
      CDI.updateAutoPayStatus (Just DI.ACTIVE) (cast driverId)
  return Success

---------------------------------------------------------------------------------------------------------
------------------------------------------ Helper Functions ---------------------------------------------
---------------------------------------------------------------------------------------------------------

validateActiveMandateExists :: Id SP.Person -> DriverPlan -> Flow DM.Mandate
validateActiveMandateExists driverId driverPlan = do
  case driverPlan.mandateId of
    Nothing -> throwError $ ActiveMandateDoNotExist driverId.getId
    Just mandateId -> do
      mandate <- Esq.runInReplica $ QM.findById mandateId >>= fromMaybeM (MandateNotFound mandateId.getId)
      unless (mandate.status == DM.ACTIVE) $ throwError (ActiveMandateDoNotExist driverId.getId)
      return mandate

validateInActiveMandateExists :: Id SP.Person -> DriverPlan -> Flow DM.Mandate
validateInActiveMandateExists driverId driverPlan = do
  case driverPlan.mandateId of
    Nothing -> throwError $ InActiveMandateDoNotExist driverId.getId
    Just mandateId -> do
      mandate <- Esq.runInReplica $ QM.findById mandateId >>= fromMaybeM (MandateNotFound mandateId.getId)
      unless (mandate.status == DM.INACTIVE) $ throwError (InActiveMandateDoNotExist driverId.getId)
      return mandate

createMandateInvoiceAndOrder :: Id SP.Person -> Id DM.Merchant -> Plan -> Flow (Payment.CreateOrderResp, Id DOrder.PaymentOrder)
createMandateInvoiceAndOrder driverId merchantId plan = do
  driverFees <- QDF.findAllPendingAndDueDriverFeeByDriverId driverId
  if not (null driverFees)
    then SPayment.createOrder (driverId, merchantId) driverFees (Just mandateOrder)
    else do
      driverFee <- mkDriverFee
      Esq.runNoTransaction $ QDF.create driverFee
      SPayment.createOrder (driverId, merchantId) [driverFee] (Just mandateOrder)
  where
    mandateOrder =
      SPayment.MandateOrder
        { maxAmount = plan.maxAmount,
          _type = Payment.REQUIRED,
          frequency = Payment.ASPRESENTED
        }
    mkDriverFee = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        DF.DriverFee
          { id = id,
            merchantId = merchantId,
            payBy = now,
            status = DF.PAYMENT_PENDING,
            numRides = 0,
            createdAt = now,
            updatedAt = now,
            platformFee = DF.PlatformFee (round plan.registrationAmount) 0.0 0.0,
            totalEarnings = 0,
            feeType = DF.MANDATE_REGISTRATION,
            govtCharges = 0,
            startTime = now,
            endTime = now,
            driverId = cast driverId
          }

convertPlanToPlanEntity :: Id SP.Person -> Plan -> Flow PlanEntity
convertPlanToPlanEntity driverId plan@Plan {..} = do
  offers <- Payment.offerList merchantId =<< makeOfferReq
  let planFareBreakup = mkPlanFareBreakup offers.offerResp
  planBaseFrequcency <- case planBaseAmount of
    PERRIDE_BASE _ -> return "PER_RIDE"
    DAILY_BASE _ -> return "DAILY"
    WEEKLY_BASE _ -> return "WEEKLY"
    MONTHLY_BASE _ -> return "MONTHLY"
  return
    PlanEntity
      { id = plan.id.getId,
        offers = makeOfferEntity <$> offers.offerResp,
        frequency = planBaseFrequcency,
        ..
      }
  where
    makeOfferEntity offer =
      OfferEntity
        { title = offer.offerDescription.title,
          description = offer.offerDescription.description,
          tnc = offer.offerDescription.tnc
        }
    makeOfferReq = do
      driver <- QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
      let offerOrder = Payment.OfferOrder {orderId = Nothing, amount = plan.maxAmount, currency = Payment.INR}
          customerReq = Payment.OfferCustomer {customerId = driverId.getId, email = driver.email, mobile = Nothing}
      transporterConfig <- QTC.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
      now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
      return
        Payment.OfferListReq
          { order = offerOrder,
            customer = Just customerReq,
            planId = plan.id.getId,
            registrationDate = now
          }
    mkPlanFareBreakup offers = do
      let baseAmount = case plan.planBaseAmount of
            PERRIDE_BASE amount -> amount
            DAILY_BASE amount -> amount
            WEEKLY_BASE amount -> amount
            MONTHLY_BASE amount -> amount
          (discountAmount, finalOrderAmount) =
            if null offers
              then (0.0, baseAmount)
              else do
                let bestOffer = minimumBy (comparing (.finalOrderAmount)) offers
                (bestOffer.discountAmount, bestOffer.finalOrderAmount)
      [ PlanFareBreakup {component = "INITIAL_BASE_FEE", amount = baseAmount},
        PlanFareBreakup {component = "REGISTRATION_FEE", amount = plan.registrationAmount},
        PlanFareBreakup {component = "MAX_FEE_LIMIT", amount = plan.maxAmount},
        PlanFareBreakup {component = "DISCOUNTED_FEE", amount = discountAmount},
        PlanFareBreakup {component = "FINAL_FEE", amount = finalOrderAmount}
        ]
