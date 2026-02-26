{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.PPF.PaymentStateMachine
  ( validatePaymentStatusTransition,
    validateSettlementStatusTransition,
    validPaymentTransitions,
    validSettlementTransitions,
    PPFPaymentStatusTransitionError (..),
    PPFSettlementStatusTransitionError (..),
  )
where

import Domain.Types.PPFRecon
import Kernel.Prelude
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common

-- | Errors for invalid payment status transitions
data PPFPaymentStatusTransitionError
  = InvalidPaymentStatusTransition PPFPaymentStatus PPFPaymentStatus
  deriving (Show, Eq)

-- | Errors for invalid settlement status transitions
data PPFSettlementStatusTransitionError
  = InvalidSettlementStatusTransition PPFSettlementStatus PPFSettlementStatus
  deriving (Show, Eq)

-- | Valid PPF payment status transitions:
--
-- INITIATED  -> COLLECTED   (payment received from customer)
-- COLLECTED  -> HELD        (funds held by PA/PG pending fulfillment)
-- HELD       -> RELEASED    (hold removed, settlement triggered after fulfillment)
-- RELEASED   -> SETTLED     (funds credited to accounts)
-- HELD       -> REFUNDED    (refund to customer from hold)
-- COLLECTED  -> REFUNDED    (refund before hold)
-- INITIATED  -> FAILED      (payment initiation failed)
-- COLLECTED  -> FAILED      (payment processing failed)
validPaymentTransitions :: [(PPFPaymentStatus, PPFPaymentStatus)]
validPaymentTransitions =
  [ (INITIATED, COLLECTED),
    (COLLECTED, HELD),
    (HELD, RELEASED),
    (RELEASED, SETTLED),
    (HELD, REFUNDED),
    (COLLECTED, REFUNDED),
    (INITIATED, FAILED),
    (COLLECTED, FAILED)
  ]

-- | Validate a payment status transition.
-- Returns Right () if valid, Left error if invalid.
validatePaymentStatusTransition ::
  (MonadFlow m) =>
  PPFPaymentStatus ->
  PPFPaymentStatus ->
  m ()
validatePaymentStatusTransition fromStatus toStatus = do
  unless ((fromStatus, toStatus) `elem` validPaymentTransitions) $ do
    logError $
      "Invalid PPF payment status transition: "
        <> show fromStatus
        <> " -> "
        <> show toStatus
    throwError . InternalError $
      "Invalid PPF payment status transition from "
        <> show fromStatus
        <> " to "
        <> show toStatus

-- | Valid PPF settlement status transitions:
--
-- PENDING     -> IN_PROGRESS  (recon initiated)
-- IN_PROGRESS -> SETTLEMENT_SETTLED   (settlement confirmed)
-- IN_PROGRESS -> SETTLEMENT_FAILED    (settlement failed)
-- PENDING     -> SETTLEMENT_FAILED    (settlement failed before initiation)
-- SETTLEMENT_FAILED -> PENDING        (retry after failure)
validSettlementTransitions :: [(PPFSettlementStatus, PPFSettlementStatus)]
validSettlementTransitions =
  [ (PENDING, IN_PROGRESS),
    (IN_PROGRESS, SETTLEMENT_SETTLED),
    (IN_PROGRESS, SETTLEMENT_FAILED),
    (PENDING, SETTLEMENT_FAILED),
    (SETTLEMENT_FAILED, PENDING) -- retry
  ]

-- | Validate a settlement status transition.
validateSettlementStatusTransition ::
  (MonadFlow m) =>
  PPFSettlementStatus ->
  PPFSettlementStatus ->
  m ()
validateSettlementStatusTransition fromStatus toStatus = do
  unless ((fromStatus, toStatus) `elem` validSettlementTransitions) $ do
    logError $
      "Invalid PPF settlement status transition: "
        <> show fromStatus
        <> " -> "
        <> show toStatus
    throwError . InternalError $
      "Invalid PPF settlement status transition from "
        <> show fromStatus
        <> " to "
        <> show toStatus
