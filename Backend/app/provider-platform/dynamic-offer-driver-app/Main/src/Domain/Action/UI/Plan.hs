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
import Domain.Types.DriverPlan
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import Domain.Types.Plan
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Payment.Juspay.Types as Payment
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import qualified SharedLogic.Payment as SPayment
import Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverPlan as QDPlan
import qualified Storage.Queries.Plan as QPD
import Tools.Error

data PlanListAPIRes = PlanListAPIRes
  { list :: [PlanEntity],
    subscriptionStartDate :: UTCTime
  }
  deriving (Generic, ToJSON, ToSchema)

data PlanEntity = PlanEntity
  { id :: Text,
    name :: Text,
    description :: Text,
    amount :: Money,
    freeRideCount :: Int,
    frequency :: Frequency,
    offers :: [OfferEntity]
  }
  deriving (Generic, ToJSON, ToSchema)

data OfferEntity = OfferEntity
  { title :: Maybe Text,
    description :: Maybe Text,
    tnc :: Maybe Text
  }
  deriving (Generic, ToJSON, ToSchema)

planList :: (Id SP.Person, Id DM.Merchant) -> Maybe Int -> Maybe Int -> Maybe PaymentMode -> Flow PlanListAPIRes
planList (_driverId, _merchantId) _mbLimit _mbOffset _mbPaymentType = do
  now <- getCurrentTime
  return $
    PlanListAPIRes
      { list = [],
        subscriptionStartDate = now
      }

newtype PlanSubscribeReq = PlanSubscribeReq
  { paymentMode :: PaymentMode
  }
  deriving (Generic, FromJSON, ToSchema)

newtype PlanSubscribeRes = PlanSubscribeRes
  { orderResp :: Maybe Payment.CreateOrderResp
  }
  deriving (Generic, ToJSON, ToSchema)

planSubscribe :: Id Plan -> (Id SP.Person, Id DM.Merchant) -> PlanSubscribeReq -> Flow PlanSubscribeRes
planSubscribe planId (driverId, merchantId) req = do
  plan <- Esq.runInReplica $ QPD.findByIdAndPaymentMode planId req.paymentMode >>= fromMaybeM (PlanNotFound planId.getId)
  driverPlan <- Esq.runInReplica $ QDPlan.findByDriverId driverId
  case driverPlan of
    Nothing -> do
      case plan.paymentMode of
        AUTOPAY -> do
          newDriverPlan <- mkDriverPlan plan INACTIVE
          Esq.runNoTransaction $ QDPlan.create newDriverPlan
          order <- createMandateInvoiceAndOrder driverId merchantId plan
          return $ PlanSubscribeRes (Just order)
        MANUAL -> do
          newDriverPlan <- mkDriverPlan plan ACTIVE
          Esq.runNoTransaction $ QDPlan.create newDriverPlan
          return $ PlanSubscribeRes Nothing
    _ -> throwError (InternalError "Driver is already subscribed. Please use /upgrade or /select APIs.")
  where
    mkDriverPlan plan planStatus = do
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

planSelect :: Id Plan -> (Id SP.Person, Id DM.Merchant) -> Flow APISuccess
planSelect planId (driverId, _) = do
  currentDriverPlan <- Esq.runInReplica $ QDPlan.findByDriverId driverId >>= fromMaybeM (NoCurrentPlanForDriver driverId.getId)
  when (currentDriverPlan.planStatus == INACTIVE) $ throwError (InvalidPlanStatus driverId.getId)
  Esq.runNoTransaction $ QDPlan.updatePlanIdByDriverId driverId planId
  return Success

planPause :: (Id SP.Person, Id DM.Merchant) -> Flow APISuccess
planPause (_driverId, _merchantId) = do
  return Success

planResume :: (Id SP.Person, Id DM.Merchant) -> Flow APISuccess
planResume (_driverId, _merchantId) = do
  return Success

newtype PlanUpgradeRes = PlanUpgradeRes
  { orderResp :: Payment.CreateOrderResp
  }
  deriving (Generic, ToJSON, ToSchema)

planUpgradeToAutopay :: (Id SP.Person, Id DM.Merchant) -> Flow PlanUpgradeRes
planUpgradeToAutopay (driverId, merchantId) = do
  currentDriverPlan <- Esq.runInReplica $ QDPlan.findByDriverId driverId >>= fromMaybeM (NoCurrentPlanForDriver driverId.getId)
  unless (currentDriverPlan.planType == MANUAL) $ throwError InvalidPaymentMode
  unless (currentDriverPlan.planStatus == ACTIVE) $ throwError (InvalidPlanStatus driverId.getId)
  currentPlan <- Esq.runInReplica $ QPD.findByIdAndPaymentMode currentDriverPlan.planId currentDriverPlan.planType >>= fromMaybeM (PlanNotFound currentDriverPlan.planId.getId)
  order <- createMandateInvoiceAndOrder driverId merchantId currentPlan
  return $ PlanUpgradeRes order

planDowngradeToManual :: (Id SP.Person, Id DM.Merchant) -> Flow APISuccess
planDowngradeToManual (_driverId, _merchantId) = do
  return Success

createMandateInvoiceAndOrder :: Id SP.Person -> Id DM.Merchant -> Plan -> Flow Payment.CreateOrderResp
createMandateInvoiceAndOrder driverId merchantId plan = do
  driverFees <- QDF.findAllPendingAndDueDriverFeeByDriverId driverId
  if length driverFees > 0
    then SPayment.createOrder (driverId, merchantId) driverFees (Just mandateOrder)
    else do
      driverFee <- mkDriverFee
      Esq.runNoTransaction $ QDF.create driverFee
      SPayment.createOrder (driverId, merchantId) [driverFee] (Just mandateOrder)
  where
    mandateOrder =
      SPayment.MandateOrder
        { maxAmount = plan.maxAmount,
          _type = castPlanPaymentMode plan.paymentMode,
          frequency = castPlanFrequency plan.frequency
        }
    mkDriverFee = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        DF.DriverFee
          { id = id,
            payBy = now,
            status = DF.PAYMENT_PENDING,
            numRides = 0,
            createdAt = now,
            updatedAt = now,
            platformFee = DF.PlatformFee plan.registrationAmount 0.0 0.0,
            totalEarnings = 0,
            feeType = DF.MANDATE_REGISTRATION,
            govtCharges = 0,
            startTime = now,
            endTime = now,
            driverId = cast driverId
          }
    castPlanFrequency = \case
      DAILY -> Payment.DAILY
      WEEKLY -> Payment.WEEKLY
      MONTHLY -> Payment.MONTHLY
    castPlanPaymentMode = \case
      AUTOPAY -> Payment.REQUIRED
      MANUAL -> Payment.OPTIONAL
