/**
 * MetricsTracking.js
 * Client-side telemetry for payment journey (Fix #22), booking funnel (Fix #23),
 * and QR scan monitoring (Fix #24).
 *
 * Collects latency and success/failure metrics, then pushes them to the backend
 * via the existing POST /metrics/increment API so they appear as Prometheus counters
 * and histograms on the server side.
 */

const { JBridge } = window;

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

function now() {
  return typeof performance !== "undefined" ? performance.now() : Date.now();
}

const _timers = {};

function startTimer(key) {
  _timers[key] = now();
}

function stopTimer(key) {
  const start = _timers[key];
  if (start === undefined) return -1;
  delete _timers[key];
  return (now() - start) / 1000; // seconds
}

/**
 * Push a metric event to the backend Prometheus endpoint.
 * Falls back to console.log when the bridge is unavailable (dev/test).
 */
function pushMetric(metricName, labels, value) {
  const payload = {
    metric: metricName,
    labels: labels,
    value: value
  };
  try {
    if (JBridge && JBridge.firebaseLogEventWithParams) {
      JBridge.firebaseLogEventWithParams("ny_metric_" + metricName, "payload", JSON.stringify(payload));
    }
    if (typeof window.__pushMetricToBackend === "function") {
      window.__pushMetricToBackend(payload);
    }
  } catch (e) {
    console.warn("[MetricsTracking] pushMetric failed:", e);
  }
}

// ---------------------------------------------------------------------------
// Fix #22 — Payment journey monitoring
// ---------------------------------------------------------------------------

/** Call when HyperSDK preFetch / initiate begins. */
export const startPaymentPageLoad = function () {
  startTimer("payment_page_load");
  startTimer("hypersdk_init");
};

/** Call when HyperSDK initiate completes (success or failure). */
export const onHyperSDKInitComplete = function (platform) {
  return function (result) {
    return function () {
      const duration = stopTimer("hypersdk_init");
      pushMetric("hypersdk_init_total", { platform: platform, result: result }, 1);
      if (duration >= 0) {
        pushMetric("hypersdk_init_duration_seconds", { platform: platform, result: result }, duration);
      }
    };
  };
};

/** Call when the payment page is fully visible to the user. */
export const onPaymentPageVisible = function (platform) {
  return function () {
    const duration = stopTimer("payment_page_load");
    if (duration >= 0) {
      pushMetric("payment_page_load_duration_seconds", { platform: platform, result: "loaded" }, duration);
    }
  };
};

/** Call when a payment attempt concludes (success, failure, or timeout). */
export const onPaymentAttemptComplete = function (method) {
  return function (transitMode) {
    return function (result) {
      return function () {
        pushMetric("payment_attempt_total", {
          payment_method: method,
          transit_mode: transitMode,
          result: result
        }, 1);
      };
    };
  };
};

// ---------------------------------------------------------------------------
// Fix #23 — Booking funnel per-stage tracking
// ---------------------------------------------------------------------------

const BOOKING_STAGES = ["search", "select", "fare", "pay", "confirm", "ticket", "qr"];

/** Call when entering a booking stage. Starts the stage timer. */
export const onBookingStageStart = function (transitMode) {
  return function (stage) {
    return function () {
      startTimer("booking_stage_" + transitMode + "_" + stage);
    };
  };
};

/**
 * Call when a booking stage completes.
 * @param {string} transitMode - metro | bus | suburban
 * @param {string} stage       - search | select | fare | pay | confirm | ticket | qr
 * @param {string} result      - success | failure | timeout
 */
export const onBookingStageComplete = function (transitMode) {
  return function (stage) {
    return function (result) {
      return function () {
        pushMetric("booking_stage_total", {
          transit_mode: transitMode,
          stage: stage,
          result: result
        }, 1);
        const duration = stopTimer("booking_stage_" + transitMode + "_" + stage);
        if (duration >= 0) {
          pushMetric("booking_stage_duration_seconds", {
            transit_mode: transitMode,
            stage: stage
          }, duration);
        }
      };
    };
  };
};

/** Call when a complete booking flow succeeds end-to-end. */
export const onBookingE2ESuccess = function (transitMode) {
  return function () {
    pushMetric("booking_e2e_success_total", { transit_mode: transitMode }, 1);
  };
};

// ---------------------------------------------------------------------------
// Fix #24 — QR scan success rate monitoring
// ---------------------------------------------------------------------------

/** Call after a QR code is generated for a ticket. */
export const onQRGenerated = function (transitMode) {
  return function (qrVersion) {
    return function (payloadSizeBytes) {
      return function () {
        pushMetric("qr_generation_total", {
          transit_mode: transitMode,
          qr_version: qrVersion
        }, 1);
        pushMetric("qr_payload_size_bytes", {
          transit_mode: transitMode,
          journey_type: "single"
        }, payloadSizeBytes);
      };
    };
  };
};

/**
 * Call after a QR code scan attempt (at AFC gate or conductor device).
 * @param {string} station - station or route identifier
 * @param {string} result  - success | failure
 */
export const onQRScanAttempt = function (station) {
  return function (result) {
    return function () {
      pushMetric("qr_scan_attempt_total", {
        station: station,
        result: result
      }, 1);
    };
  };
};
