{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Metrics.PaymentMonitoringMetrics.Types
  ( HasPaymentMonitoringMetrics,
    PaymentMonitoringMetricsContainer (..),
    registerPaymentMonitoringMetricsContainer,
  )
where

import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics (DeploymentVersion)
import Kernel.Utils.Common
import Prometheus as P

type HasPaymentMonitoringMetrics m r =
  HasFlowEnv m r ["paymentMonitoringMetrics" ::: PaymentMonitoringMetricsContainer, "version" ::: DeploymentVersion]

-- | Payment gateway request counter: {gateway, method, result, bank}
-- method: "upi" | "card" | "wallet" | "netbanking" | "cash"
-- result: "success" | "failure" | "timeout" | "pending"
type PaymentGatewayRequestMetric = P.Vector P.Label4 P.Counter

-- | Payment gateway latency histogram: {gateway, method}
type PaymentGatewayLatencyMetric = P.Vector P.Label2 P.Histogram

-- | Refund counter: {gateway, reason, result}
-- reason: "duplicate_charge" | "overcharge" | "cancellation" | "service_issue" | "other"
-- result: "success" | "failure" | "pending"
type RefundCounterMetric = P.Vector P.Label3 P.Counter

-- | Refund processing duration histogram: {gateway}
type RefundDurationMetric = P.Vector P.Label1 P.Histogram

-- | Double deduction detection counter: {city}
type DoubleDeductionMetric = P.Vector P.Label1 P.Counter

-- | Payment method switch counter: {from_method, to_method}
type PaymentMethodSwitchMetric = P.Vector P.Label2 P.Counter

data PaymentMonitoringMetricsContainer = PaymentMonitoringMetricsContainer
  { paymentGatewayRequestCounter :: PaymentGatewayRequestMetric,
    paymentGatewayLatencyHist :: PaymentGatewayLatencyMetric,
    refundCounter :: RefundCounterMetric,
    refundDurationHist :: RefundDurationMetric,
    doubleDeductionCounter :: DoubleDeductionMetric,
    paymentMethodSwitchCounter :: PaymentMethodSwitchMetric
  }

registerPaymentMonitoringMetricsContainer :: IO PaymentMonitoringMetricsContainer
registerPaymentMonitoringMetricsContainer = do
  paymentGatewayRequestCounter <- registerPaymentGatewayRequestCounter
  paymentGatewayLatencyHist <- registerPaymentGatewayLatencyHist
  refundCounter <- registerRefundCounter
  refundDurationHist <- registerRefundDurationHist
  doubleDeductionCounter <- registerDoubleDeductionCounter
  paymentMethodSwitchCounter <- registerPaymentMethodSwitchCounter
  return $ PaymentMonitoringMetricsContainer {..}

registerPaymentGatewayRequestCounter :: IO PaymentGatewayRequestMetric
registerPaymentGatewayRequestCounter =
  P.register $
    P.vector ("gateway", "method", "result", "bank") $
      P.counter $
        P.Info
          "payment_gateway_request_total"
          "Payment gateway requests by gateway, method, result, and bank"

registerPaymentGatewayLatencyHist :: IO PaymentGatewayLatencyMetric
registerPaymentGatewayLatencyHist =
  P.register $
    P.vector ("gateway", "method") $
      P.histogram
        (P.Info "payment_gateway_latency_seconds" "Payment gateway response latency")
        gatewayLatencyBuckets
  where
    gatewayLatencyBuckets = [0.1, 0.25, 0.5, 1, 2.5, 5, 10, 30]

registerRefundCounter :: IO RefundCounterMetric
registerRefundCounter =
  P.register $
    P.vector ("gateway", "reason", "result") $
      P.counter $
        P.Info
          "refund_total"
          "Refund requests by gateway, reason, and result"

registerRefundDurationHist :: IO RefundDurationMetric
registerRefundDurationHist =
  P.register $
    P.vector ("gateway") $
      P.histogram
        (P.Info "refund_processing_duration_seconds" "Refund processing time from initiation to completion")
        refundDurationBuckets
  where
    -- Buckets from 1 minute to 7 days
    refundDurationBuckets = [60, 300, 3600, 86400, 172800, 604800]

registerDoubleDeductionCounter :: IO DoubleDeductionMetric
registerDoubleDeductionCounter =
  P.register $
    P.vector ("city") $
      P.counter $
        P.Info
          "double_deduction_detected_total"
          "Double deduction events detected requiring immediate investigation"

registerPaymentMethodSwitchCounter :: IO PaymentMethodSwitchMetric
registerPaymentMethodSwitchCounter =
  P.register $
    P.vector ("from_method", "to_method") $
      P.counter $
        P.Info
          "payment_method_switch_total"
          "Payment method switches during ride (e.g. UPI to cash)"
