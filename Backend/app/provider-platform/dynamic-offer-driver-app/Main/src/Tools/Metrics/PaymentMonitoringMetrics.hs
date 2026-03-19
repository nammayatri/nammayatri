{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Metrics.PaymentMonitoringMetrics
  ( module Tools.Metrics.PaymentMonitoringMetrics,
    module Reexport,
  )
where

import EulerHS.Prelude
import GHC.Records.Extra
import Prometheus as P
import Tools.Metrics.PaymentMonitoringMetrics.Types as Reexport

-- | Increment payment gateway request counter
-- method: "upi" | "card" | "wallet" | "netbanking" | "cash"
-- result: "success" | "failure" | "timeout" | "pending"
incrementPaymentGatewayRequest ::
  (MonadIO m, HasPaymentMonitoringMetrics m r) =>
  Text -> -- gateway
  Text -> -- method
  Text -> -- result
  Text -> -- bank
  m ()
incrementPaymentGatewayRequest gateway method result bank = do
  container <- asks (.paymentMonitoringMetrics)
  liftIO $ P.withLabel container.paymentGatewayRequestCounter (gateway, method, result, bank) P.incCounter

-- | Observe payment gateway response latency
observePaymentGatewayLatency ::
  (MonadIO m, HasPaymentMonitoringMetrics m r) =>
  Text -> -- gateway
  Text -> -- method
  Double -> -- latency in seconds
  m ()
observePaymentGatewayLatency gateway method latency = do
  container <- asks (.paymentMonitoringMetrics)
  liftIO $ P.withLabel container.paymentGatewayLatencyHist (gateway, method) (`P.observe` latency)

-- | Increment refund counter
-- reason: "duplicate_charge" | "overcharge" | "cancellation" | "service_issue" | "other"
-- result: "success" | "failure" | "pending"
incrementRefund ::
  (MonadIO m, HasPaymentMonitoringMetrics m r) =>
  Text -> -- gateway
  Text -> -- reason
  Text -> -- result
  m ()
incrementRefund gateway reason result = do
  container <- asks (.paymentMonitoringMetrics)
  liftIO $ P.withLabel container.refundCounter (gateway, reason, result) P.incCounter

-- | Observe refund processing duration in seconds
observeRefundDuration ::
  (MonadIO m, HasPaymentMonitoringMetrics m r) =>
  Text -> -- gateway
  Double -> -- duration in seconds
  m ()
observeRefundDuration gateway duration = do
  container <- asks (.paymentMonitoringMetrics)
  liftIO $ P.withLabel container.refundDurationHist (gateway) (`P.observe` duration)

-- | Increment double deduction detection counter
incrementDoubleDeduction ::
  (MonadIO m, HasPaymentMonitoringMetrics m r) =>
  Text -> -- city
  m ()
incrementDoubleDeduction city = do
  container <- asks (.paymentMonitoringMetrics)
  liftIO $ P.withLabel container.doubleDeductionCounter (city) P.incCounter

-- | Increment payment method switch counter
incrementPaymentMethodSwitch ::
  (MonadIO m, HasPaymentMonitoringMetrics m r) =>
  Text -> -- from_method
  Text -> -- to_method
  m ()
incrementPaymentMethodSwitch fromMethod toMethod = do
  container <- asks (.paymentMonitoringMetrics)
  liftIO $ P.withLabel container.paymentMethodSwitchCounter (fromMethod, toMethod) P.incCounter
