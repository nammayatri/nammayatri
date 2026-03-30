import React, { useState, useCallback, useRef, useMemo, useEffect } from 'react';
import { ConfigBar } from './components/ConfigBar';
import { RideFlowTree } from './components/RideFlowTree';
import { LogPanel } from './components/LogPanel';
import axios from 'axios';
import { callStep, startLocationPinger, stopLocationPinger, setGlobalLog } from './services/api';
import { buildApiCatalog } from './api-catalog';
import { getLocationsForCity } from './mock-data/locations';
import { Config, LogEntry, Step, StepResult } from './types';
import './App.css';

const defaultConfig: Config = {
  riderUrl: 'http://localhost:8013',
  driverUrl: 'http://localhost:8016',
  token: '',
  stripeUrl: 'http://localhost:8080/stripe',
};

function loadConfig(): Config {
  try {
    const saved = localStorage.getItem('ny-test-config');
    return saved ? { ...defaultConfig, ...JSON.parse(saved) } : defaultConfig;
  } catch { return defaultConfig; }
}

// --- Flow definitions ---

export interface FlowDef {
  id: string;
  name: string;
  description: string;
  nodes: Record<string, Step[]>;
  nodeOrder: string[];
  hasDriverSetup?: boolean;
}

function buildRideFlowSteps(): Record<string, Step[]> {
  return {
    'driver-setup': [
      { id: 'driver-active', name: 'Set Driver Online', method: 'POST', service: 'driver',
        path: '/driver/setActivity?active=true&mode=%22ONLINE%22', auth: true },
      { id: 'driver-location', name: 'Start Location Ping', method: 'POST', service: 'lts',
        path: '/driver/location', auth: true,
        extraHeaders: (ctx) => ({ 'vt': ctx.driverVehicleVariant || 'SUV', 'dm': 'ONLINE', ...(ctx.driverMerchantId ? { 'mid': ctx.driverMerchantId } : {}) }),
        body: (ctx) => {
          const origin = ctx.searchOrigin || ctx.driverLocation || { lat: 10.0739, lon: 76.2733 };
          return [{ pt: { lat: origin.lat, lon: origin.lon }, ts: new Date().toISOString() }];
        },
        save: (_d, ctx) => { startLocationPinger(ctx); } },
    ],
    'discovery': [
      { id: 'ride-search', name: 'Search Ride', method: 'POST', service: 'rider', path: '/rideSearch', auth: true,
        body: (ctx) => ({
          fareProductType: 'ONE_WAY',
          contents: {
            origin: { gps: ctx.searchOrigin, address: ctx.fromAddress || { area: '', city: '', country: '', state: '' } },
            destination: { gps: ctx.searchDestination, address: ctx.toAddress || { area: '', city: '', country: '', state: '' } },
            isSourceManuallyMoved: false,
            isDestinationManuallyMoved: false,
            isSpecialLocation: false,
            quotesUnifiedFlow: true,
            fareParametersInRateCard: false,
          },
        }),
        save: (d, ctx) => { ctx.searchId = d.searchId; },
        assert: (d) => d.searchId ? null : 'Missing searchId',
        summary: (_d, ctx) => `${ctx.fromAddress?.title || '?'} -> ${ctx.toAddress?.title || '?'}` },
      { id: 'get-estimates', name: 'Get Estimates', method: 'GET', service: 'rider', path: '/placeholder', auth: true,
        useCatalog: 'get-estimates',
        poll: { intervalMs: 2000, timeoutMs: 30000 },
        save: (d, ctx) => {
          ctx.estimates = d.estimates || [];
          ctx.quotes = d.quotes || [];
          if (d.estimates?.length > 0) {
            const driverVariant = ctx.driverVehicleVariant;
            const match = driverVariant && d.estimates.find((e: any) => e.vehicleVariant === driverVariant);
            ctx.selectedEstimateId = match ? match.id : d.estimates[0].id;
          }
        },
        assert: (d) => (d.estimates?.length > 0) ? null : 'No estimates yet',
        summary: (d) => {
          const tiers = (d.estimates || []).map((e: any) => e.vehicleVariant || e.serviceTierType).filter(Boolean);
          return tiers.length ? tiers.join(', ') : 'none';
        } },
      { id: 'estimate-select', name: 'Select Estimate', method: 'POST', service: 'rider',
        path: (ctx) => `/estimate/${ctx.selectedEstimateId}/select2`, auth: true,
        body: (ctx) => ({
          autoAssignEnabled: true,
          autoAssignEnabledV2: true,
          customerExtraFee: null,
          customerExtraFeeWithCurrency: null,
          otherSelectedEstimates: null,
          isAdvancedBookingEnabled: null,
          paymentMethodId: ctx.paymentMethodId || null,
          paymentInstrument: ctx.paymentMethodId
            ? { instrumentType: 'Card', instrumentName: 'DefaultCardType' }
            : { instrumentType: 'Cash' },
          disabilityDisable: null,
          isPetRide: false,
        }),
        summary: (_d, ctx) => {
          const est = (ctx.estimates || []).find((e: any) => e.id === ctx.selectedEstimateId);
          return est ? `${est.vehicleVariant || est.serviceTierType}` : ctx.selectedEstimateId || '?';
        } },
    ],
    'driver-accept': [
      { id: 'driver-nearby', name: 'Get Nearby Requests', method: 'GET', service: 'driver', path: '/driver/nearbyRideRequest', auth: true,
        poll: { intervalMs: 2000, timeoutMs: 30000 },
        save: (d, ctx) => {
          const requests = d.searchRequestsForDriver || [];
          if (requests.length > 0) {
            ctx.searchTryId = requests[0].searchTryId;
            ctx.driverSearchRequestId = requests[0].searchRequestId;
          }
        },
        assert: (d) => (d.searchRequestsForDriver?.length > 0) ? null : 'No nearby requests',
        summary: (d) => `${(d.searchRequestsForDriver || []).length} request(s)` },
      { id: 'driver-respond', name: 'Accept Quote', method: 'POST', service: 'driver', path: '/driver/searchRequest/quote/respond', auth: true,
        body: (ctx) => ({
          searchTryId: ctx.searchTryId,
          offeredFare: null, offeredFareWithCurrency: null,
          response: 'Accept',
          notificationSource: null, renderedAt: null, respondedAt: null,
        }) },
    ],
    'booking': [
      { id: 'poll-booking', name: 'Poll Estimate Results', method: 'GET', service: 'rider',
        path: (ctx) => `/estimate/${ctx.selectedEstimateId}/results`, auth: true,
        poll: { intervalMs: 2000, timeoutMs: 30000 },
        save: (d, ctx) => {
          if (d.bookingId) ctx.bookingId = d.bookingId;
          if (d.bookingIdV2) ctx.bookingId = d.bookingIdV2;
        },
        assert: (d) => (d.bookingId || d.bookingIdV2) ? null : 'No bookingId yet',
        summary: (d) => d.bookingId || d.bookingIdV2 || '' },
      { id: 'get-booking', name: 'Get Booking Details', method: 'POST', service: 'rider',
        path: (ctx) => `/rideBooking/${ctx.bookingId}`, auth: true,
        poll: { intervalMs: 2000, timeoutMs: 30000 },
        save: (d, ctx) => {
          ctx.bookingStatus = d.status;
          const ride = d.rideList?.[0] || d.ride;
          ctx.rideId = ride?.id;
          ctx.rideOtp = ride?.rideOtp || ride?.otp;
          if (d.status === 'CANCELLED' || d.status === 'COMPLETED') {
            ctx.bookingTerminal = true;
            const cr = d.cancellationReason;
            if (cr && typeof cr === 'object') {
              ctx.cancellationReason = `${cr.source || '?'} — ${cr.reasonCode || '?'}${cr.additionalInfo ? ': ' + cr.additionalInfo : ''} (${cr.reasonStage || ''})`;
            } else {
              ctx.cancellationReason = d.status;
            }
          }
        },
        assert: (d) => {
          const status = d.status;
          // Terminal states — stop polling immediately
          if (status === 'CANCELLED') return null;
          if (status === 'COMPLETED') return null;
          const ride = d.rideList?.[0] || d.ride;
          return ride?.id ? null : 'No ride in booking yet';
        },
        summary: (d) => {
          const status = d.status;
          if (status === 'CANCELLED') {
            const cr = d.cancellationReason;
            if (cr && typeof cr === 'object') {
              return `CANCELLED — ${cr.source || '?'}: ${cr.reasonCode || '?'}${cr.additionalInfo ? ' — ' + cr.additionalInfo : ''}`;
            }
            return 'CANCELLED';
          }
          if (status === 'COMPLETED') return 'COMPLETED';
          const ride = d.rideList?.[0] || d.ride;
          if (!ride) return '';
          return `OTP: ${ride.rideOtp || ride.otp || '?'} | Driver: ${ride.driverName || '?'}`;
        } },
    ],
    'fulfillment': [
      { id: 'driver-ride-list', name: 'Get Active Ride (Driver)', method: 'GET', service: 'driver',
        path: '/driver/ride/list?onlyActive=true&limit=1', auth: true,
        poll: { intervalMs: 2000, timeoutMs: 10000 },
        save: (d, ctx) => {
          const rides = d.list || d;
          const active = Array.isArray(rides) ? rides[0] : null;
          if (active) ctx.driverRideId = active.id;
        },
        assert: (d) => {
          const rides = d.list || d;
          return (Array.isArray(rides) && rides.length > 0) ? null : 'No active ride for driver';
        },
        summary: (d) => {
          const rides = d.list || d;
          const r = Array.isArray(rides) ? rides[0] : null;
          return r ? `rideId: ${r.id?.substring(0, 8)}...` : '';
        } },
      { id: 'ride-start', name: 'Start Ride', method: 'POST', service: 'driver',
        path: (ctx) => `/driver/ride/${ctx.driverRideId}/start`, auth: true,
        body: (ctx) => {
          const origin = ctx.searchOrigin || { lat: 10.0739, lon: 76.2733 };
          return { rideOtp: ctx.rideOtp, point: { lat: origin.lat, lon: origin.lon }, odometer: null };
        },
        assert: (d) => (d?.result === 'Success' || d === 'Success') ? null : `Start ride failed: ${JSON.stringify(d)}`,
        summary: (_d, ctx) => `OTP: ${ctx.rideOtp || '?'}` },
      // For upward recompute: inflate traveledDistance in DB so it's >> estimatedDistance
      // This simulates a driver who drove much farther than the estimated route
      { id: 'inflate-distance', name: 'Inflate Traveled Distance (3x)', method: 'POST', service: 'internal',
        path: '/api/inflate-distance',
        auth: false,
        skip: (ctx) => ctx.rideEndMode !== 'upward-recompute',
        body: (ctx) => ({ rideId: ctx.driverRideId, multiplier: 3 }),
        assert: (d) => d?.result === 'Success' ? null : `Failed: ${JSON.stringify(d)}`,
        summary: () => 'traveled_distance set to 3x estimated' },
      { id: 'ride-end', name: 'End Ride', method: 'POST', service: 'driver',
        path: (ctx) => `/driver/ride/${ctx.driverRideId}/end`, auth: true,
        body: (ctx) => {
          const origin = ctx.searchOrigin || { lat: 10.0739, lon: 76.2733 };
          let dest;
          if (ctx.rideEndMode === 'downward-recompute') {
            dest = origin; // same as pickup → distance ≈ 0
          } else if (ctx.rideEndMode === 'upward-recompute') {
            // End far beyond destination (3x overshoot point)
            const realDest = ctx.searchDestination || { lat: origin.lat + 0.15, lon: origin.lon + 0.15 };
            dest = { lat: origin.lat + (realDest.lat - origin.lat) * 3, lon: origin.lon + (realDest.lon - origin.lon) * 3 };
          } else {
            dest = ctx.searchDestination || origin;
          }
          return { point: { lat: dest.lat, lon: dest.lon }, endRideOtp: null, uiDistanceCalculationWithAccuracy: null, uiDistanceCalculationWithoutAccuracy: null, odometer: null, driverGpsTurnedOff: null };
        },
        summary: (_d, ctx) => {
          if (ctx.rideEndMode === 'downward-recompute') return 'Ended at pickup (downward recompute)';
          if (ctx.rideEndMode === 'upward-recompute') return 'Ended 3x beyond destination (upward recompute)';
          return 'Ended at destination';
        } },
    ],
    'add-tip': [
      { id: 'add-tip', name: 'Add Tip', method: 'POST', service: 'rider',
        path: (ctx) => `/payment/${ctx.rideId}/addTip`, auth: true,
        skip: (ctx) => !ctx.paymentMethodId || ctx.skipTip,
        body: (ctx) => ({
          amount: { amount: ctx.tipAmount || 50, currency: ctx.duesCurrency || 'EUR' },
        }),
        assert: (d) => (d?.result === 'Success' || d === 'Success') ? null : `Add tip failed: ${JSON.stringify(d)}`,
        summary: (_d, ctx) => `Tip: ${ctx.tipAmount || 50} ${ctx.duesCurrency || 'EUR'}` },
    ],
    'driver-cancel': [
      { id: 'driver-ride-list-cancel', name: 'Get Active Ride (Driver)', method: 'GET', service: 'driver',
        path: '/driver/ride/list?onlyActive=true&limit=1', auth: true,
        poll: { intervalMs: 2000, timeoutMs: 10000 },
        save: (d, ctx) => {
          const rides = d.list || d;
          const active = Array.isArray(rides) ? rides[0] : null;
          if (active) ctx.driverRideId = active.id;
        },
        assert: (d) => {
          const rides = d.list || d;
          return (Array.isArray(rides) && rides.length > 0) ? null : 'No active ride for driver';
        },
        summary: (d) => {
          const rides = d.list || d;
          const r = Array.isArray(rides) ? rides[0] : null;
          return r ? `rideId: ${r.id?.substring(0, 8)}...` : '';
        } },
      { id: 'driver-cancel-ride', name: 'Cancel Ride (Driver)', method: 'POST', service: 'driver',
        path: (ctx) => `/driver/ride/${ctx.driverRideId}/cancel`, auth: true,
        body: () => ({
          reasonCode: 'OTHER',
          additionalInfo: 'Cancelled via test dashboard',
          doCancellationRateBasedBlocking: false,
        }),
        summary: () => 'Cancelled by driver' },
    ],
    'customer-cancel': [
      { id: 'customer-cancel-ride', name: 'Cancel Ride (Customer)', method: 'POST', service: 'rider',
        path: (ctx) => `/rideBooking/${ctx.bookingId}/cancel`, auth: true,
        body: () => ({
          reasonCode: 'OTHER',
          reasonStage: 'OnAssign',
          additionalInfo: 'Cancelled via test dashboard',
          reallocate: false,
          blockOnCancellationRate: false,
        }),
        summary: () => 'Cancelled by customer' },
    ],
  };
}

function buildDuesFlowSteps(): Record<string, Step[]> {
  return {
    'check-dues': [
      { id: 'get-dues', name: 'Get Due Amount', method: 'GET', service: 'rider', path: '/payment/getDueAmount', auth: true,
        save: (d, ctx) => {
          ctx.totalDueAmount = d.totalDueAmount;
          ctx.duesCurrency = d.currency;
          ctx.dueRides = d.rides || [];
        },
        summary: (d) => {
          const amt = d.totalDueAmount;
          const count = (d.rides || []).length;
          return amt > 0 ? `${d.currency || ''} ${amt} (${count} ride${count !== 1 ? 's' : ''})` : 'No pending dues';
        } },
    ],
    'clear-dues': [
      { id: 'clear-dues', name: 'Clear Dues', method: 'POST', service: 'rider', path: '/payment/clearDues', auth: true,
        body: (ctx) => ({ paymentMethodId: ctx.paymentMethodId || null }),
        summary: (d) => {
          if (d.status === 'SUCCESS') return `Cleared ${d.currency || ''} ${d.amountCleared} (${(d.ridesCleared || []).length} rides)`;
          return d.status || d.errorMessage || 'Unknown';
        } },
    ],
    'capture-payment': [
      { id: 'capture-payment', name: 'Capture Payment', method: 'POST', service: 'rider',
        path: (ctx) => {
          // captureRideId is always freshly set from initCtx before execution
          const rideId = ctx.captureRideId || (ctx.dueRides?.[0]?.rideId) || ctx.rideId;
          return `/payment/ride/${rideId}/capture`;
        }, auth: true,
        assert: (d) => (d?.result === 'Success' || d === 'Success') ? null : `Capture failed: ${JSON.stringify(d)}`,
        summary: (_d, ctx) => {
          const rideId = ctx.captureRideId || (ctx.dueRides?.[0]?.rideId) || ctx.rideId;
          return `rideId: ${rideId?.substring(0, 8) || '?'}...`;
        } },
    ],
    'cancellation-dues': [
      { id: 'get-cancellation-dues', name: 'Get Cancellation Dues', method: 'GET', service: 'rider',
        path: '/rideBooking/cancellationDues', auth: true,
        save: (d, ctx) => { ctx.cancellationDues = d; },
        summary: (d) => {
          return `Dues: ${d.cancellationDues ?? 0} | Rides cancelled: ${d.cancellationDueRides ?? 0}`;
        } },
    ],
  };
}

const FLOWS: FlowDef[] = [
  { id: 'ride-flow', name: 'Ride Flow', description: 'Full ride: search, book, start, end',
    nodes: {}, nodeOrder: ['discovery', 'driver-accept', 'booking', 'fulfillment'], hasDriverSetup: true },
  { id: 'dues-flow', name: 'Clear Pending Dues', description: 'Check and clear payment dues from previous rides',
    nodes: {}, nodeOrder: ['check-dues', 'capture-payment', 'clear-dues', 'cancellation-dues'] },
];

function App() {
  const [config, setConfig] = useState<Config>(loadConfig);
  const [logs, setLogs] = useState<LogEntry[]>([]);
  const [isRunning, setIsRunning] = useState(false);
  const [activeFlowId, setActiveFlowId] = useState('ride-flow');
  const [selectedOutcome, setSelectedOutcome] = useState('fulfillment');
  const [runningNodeId, setRunningNodeId] = useState<string | null>(null);
  const [selectedCity, setSelectedCity] = useState('Kochi');
  const [selectedDriverToken, setSelectedDriverToken] = useState('');
  const [selectedDriverVariant, setSelectedDriverVariant] = useState('');
  const [selectedDriverMerchantId, setSelectedDriverMerchantId] = useState('');
  const [selectedDriverPersonId, setSelectedDriverPersonId] = useState('');
  const [driverAvailable, setDriverAvailable] = useState(false);
  const [stepResults, setStepResults] = useState<Record<string, StepResult>>({});

  // Location selections
  const [driverLocationIdx, setDriverLocationIdx] = useState(0);
  const [fromLocationIdx, setFromLocationIdx] = useState(0);
  const [toLocationIdx, setToLocationIdx] = useState(1);
  const [paymentPreset, setPaymentPreset] = useState('with-card-payment');
  const [presetSelections, setPresetSelections] = useState<Record<string, string>>({});

  // Payment methods
  const [paymentMethods, setPaymentMethods] = useState<{ cardId: string; lastFourDigits?: string; cardType?: string }[]>([]);
  const [selectedPaymentMethodId, setSelectedPaymentMethodId] = useState('');
  const [paymentMethodsLoading, setPaymentMethodsLoading] = useState(false);
  const [captureRideId, setCaptureRideId] = useState('');
  const [tipAmount, setTipAmount] = useState(50);
  const [rideEndMode, setRideEndMode] = useState<'actual' | 'downward-recompute' | 'upward-recompute'>('actual');
  const [skipTip, setSkipTip] = useState(false);
  const [skippedNodes, setSkippedNodes] = useState<Record<string, boolean>>({});

  const abortRef = useRef(false);
  const ctxRef = useRef<Record<string, any>>({});
  const captureRideIdRef = useRef(captureRideId);
  captureRideIdRef.current = captureRideId; // always up to date

  const catalog = useMemo(() => buildApiCatalog(selectedCity), [selectedCity]);
  const allSteps = useMemo(() => {
    if (activeFlowId === 'dues-flow') return buildDuesFlowSteps();
    return buildRideFlowSteps();
  }, [activeFlowId]);
  const activeFlow = FLOWS.find(f => f.id === activeFlowId) || FLOWS[0];

  const log = useCallback((level: LogEntry['level'], message: string, extra?: { request?: LogEntry['request']; response?: LogEntry['response'] }) => {
    const time = new Date().toLocaleTimeString();
    setLogs(prev => [...prev, { time, level, message, ...extra }]);
  }, []);

  useEffect(() => { setGlobalLog(log); }, [log]);

  // Fetch payment methods when card payment selected or dues flow active
  const needPaymentMethods = paymentPreset === 'with-card-payment' || activeFlowId === 'dues-flow';
  useEffect(() => {
    if (!needPaymentMethods || !config.token) {
      setPaymentMethods([]);
      setSelectedPaymentMethodId('');
      return;
    }
    setPaymentMethodsLoading(true);
    axios.get('http://localhost:7082/proxy/rider/payment/methods', {
      headers: { token: config.token, 'Content-Type': 'application/json' },
      timeout: 5000,
    }).then(resp => {
      const list = resp.data?.list || [];
      setPaymentMethods(list);
      if (list.length > 0) {
        const defaultId = resp.data?.defaultPaymentMethodId || list[0].cardId;
        setSelectedPaymentMethodId(defaultId);
      } else {
        setSelectedPaymentMethodId('');
        // Auto-switch to cash if no cards available and we're in card mode
        if (paymentPreset === 'with-card-payment') {
          setPaymentPreset('cash-payment');
        }
      }
    }).catch(() => {
      setPaymentMethods([]);
      setSelectedPaymentMethodId('');
    }).finally(() => setPaymentMethodsLoading(false));
  }, [needPaymentMethods, config.token]);

  // Auto-fill captureRideId from booking details response (rider-side rideId)
  useEffect(() => {
    const bookingResult = stepResults['get-booking'];
    if (bookingResult?.status === 'pass' && bookingResult.response) {
      const ride = bookingResult.response.rideList?.[0] || bookingResult.response.ride;
      if (ride?.id && !captureRideId) {
        setCaptureRideId(ride.id);
      }
    }
  }, [stepResults, captureRideId]);

  // Reset location indices when city changes
  useEffect(() => {
    setFromLocationIdx(0);
    setToLocationIdx(Math.min(1, getLocationsForCity(selectedCity).length - 1));
    setDriverLocationIdx(0);
  }, [selectedCity]);

  // Sync preset selections with location choices
  useEffect(() => {
    const locs = getLocationsForCity(selectedCity);
    const from = locs[fromLocationIdx];
    const to = locs[toLocationIdx] || locs[0];
    if (from && to) {
      // Find or create a matching preset id
      setPresetSelections(prev => ({
        ...prev,
        'ride-search': `oneway-${selectedCity.toLowerCase()}-${fromLocationIdx}`,
        'estimate-select': paymentPreset,
      }));
    }
  }, [selectedCity, fromLocationIdx, toLocationIdx, paymentPreset]);

  const initCtx = useCallback(() => {
    const locs = getLocationsForCity(selectedCity);
    const fromLoc = locs[fromLocationIdx];
    const driverLoc = locs[driverLocationIdx]?.gps || fromLoc?.gps || locs[0]?.gps;
    const toLoc = locs[toLocationIdx] || locs[Math.min(1, locs.length - 1)];

    // Merge base config into existing context — preserves values saved by previous steps
    Object.assign(ctxRef.current, {
      driverToken: selectedDriverToken || '',
      driverPersonId: selectedDriverPersonId || '',
      driverVehicleVariant: selectedDriverVariant || 'SUV',
      driverMerchantId: selectedDriverMerchantId || '',
      driverLocation: driverLoc,
      searchOrigin: fromLoc?.gps,
      searchDestination: toLoc?.gps,
      fromAddress: fromLoc?.address,
      toAddress: toLoc?.address,
      paymentMethodId: paymentPreset === 'with-card-payment' ? selectedPaymentMethodId : null,
      captureRideId: captureRideId || undefined,
      tipAmount,
      skipTip,
      rideEndMode,
    });
  }, [selectedCity, selectedDriverToken, selectedDriverVariant, selectedDriverMerchantId, driverLocationIdx, fromLocationIdx, toLocationIdx, paymentPreset, selectedPaymentMethodId]);

  const executeStep = useCallback(async (step: Step): Promise<boolean> => {
    // Dynamic skip check
    const shouldSkip = typeof step.skip === 'function' ? step.skip(ctxRef.current) : step.skip;
    if (shouldSkip) {
      setStepResults(prev => ({ ...prev, [step.id]: { stepId: step.id, status: 'skip', durationMs: 0 } }));
      return true;
    }

    setStepResults(prev => ({ ...prev, [step.id]: { stepId: step.id, status: 'running', durationMs: 0 } }));

    const catalogApi = step.useCatalog ? catalog[step.useCatalog] : undefined;
    const presetId = presetSelections[step.id] || catalogApi?.mockDataPresets?.[0]?.id;
    const displayPath = catalogApi
      ? (typeof catalogApi.path === 'function' ? catalogApi.path(ctxRef.current) : catalogApi.path)
      : (typeof step.path === 'function' ? step.path(ctxRef.current) : step.path);

    // Build request body for logging
    let reqBody: any;
    if (catalogApi?.bodyBuilder && presetId) {
      const preset = catalogApi.mockDataPresets.find((p: any) => p.id === presetId);
      if (preset) reqBody = catalogApi.bodyBuilder(preset, ctxRef.current);
    } else if (step.body) {
      reqBody = typeof step.body === 'function' ? step.body(ctxRef.current) : step.body;
    }

    log('req', `${step.method} ${displayPath}`, {
      request: { method: step.method, url: displayPath, body: reqBody },
    });

    // Single attempt logic
    const attemptOnce = async () => {
      const result = await callStep(step, config, ctxRef.current, catalogApi, presetId);
      if (step.save && result.ok) step.save(result.data, ctxRef.current);

      let error: string | null = null;
      if (!result.ok) {
        const d = result.data;
        const apiMsg = d?.errorMessage || d?.error?.message || d?.errorCode || d?.error?.code;
        error = apiMsg ? `${apiMsg} (HTTP ${result.status})` : `HTTP ${result.status}`;
      } else if (catalogApi?.assert) {
        error = catalogApi.assert(result.data);
      } else if (step.assert) {
        error = step.assert(result.data);
      }

      const summaryText = (!error && step.summary) ? step.summary(result.data, ctxRef.current) : undefined;
      return { result, error, summaryText };
    };

    try {
      // Polling logic
      if (step.poll) {
        const startTime = Date.now();
        let attempt = 0;
        while (true) {
          attempt++;
          const { result, error, summaryText } = await attemptOnce();
          const elapsed = Date.now() - startTime;

          if (!error) {
            const logExtra = { request: { method: step.method, url: displayPath, body: reqBody }, response: { status: result.status, body: result.data } };
            const msg = summaryText ? `[OK] ${step.name} (${elapsed}ms) — ${summaryText}` : `[OK] ${step.name} (${elapsed}ms)`;
            setStepResults(prev => ({ ...prev, [step.id]: { stepId: step.id, status: 'pass', durationMs: elapsed, response: result.data, statusCode: result.status, summary: summaryText } }));
            log('success', msg, logExtra);
            return true;
          }

          if (elapsed >= step.poll!.timeoutMs || abortRef.current) {
            const logExtra = { request: { method: step.method, url: displayPath, body: reqBody }, response: { status: result.status, body: result.data } };
            setStepResults(prev => ({ ...prev, [step.id]: { stepId: step.id, status: 'fail', durationMs: elapsed, response: result.data, error: `${error} (after ${attempt} attempts)`, statusCode: result.status } }));
            log('error', `[FAIL] ${step.name}: ${error} (${attempt} attempts, ${elapsed}ms)`, logExtra);
            return false;
          }

          log('info', `[POLL] ${step.name}: ${error} — retrying (${attempt})...`);
          setStepResults(prev => ({ ...prev, [step.id]: { stepId: step.id, status: 'running', durationMs: elapsed, summary: `polling... (${attempt})` } }));
          await new Promise(r => setTimeout(r, step.poll!.intervalMs));
        }
      }

      // Non-polling: single attempt
      const { result, error, summaryText } = await attemptOnce();
      const logExtra = { request: { method: step.method, url: displayPath, body: reqBody }, response: { status: result.status, body: result.data } };

      if (error) {
        setStepResults(prev => ({ ...prev, [step.id]: { stepId: step.id, status: 'fail', durationMs: result.elapsed, response: result.data, error: error || undefined, statusCode: result.status } }));
        log('error', `[FAIL] ${step.name}: ${error} (${result.elapsed}ms)`, logExtra);
        return false;
      } else {
        const msg = summaryText ? `[OK] ${step.name} (${result.elapsed}ms) — ${summaryText}` : `[OK] ${step.name} (${result.elapsed}ms)`;
        setStepResults(prev => ({ ...prev, [step.id]: { stepId: step.id, status: 'pass', durationMs: result.elapsed, response: result.data, statusCode: result.status, summary: summaryText } }));
        log('success', msg, logExtra);
        return true;
      }
    } catch (err: any) {
      setStepResults(prev => ({ ...prev, [step.id]: { stepId: step.id, status: 'fail', durationMs: 0, error: err.message } }));
      log('error', `[ERR] ${step.name}: ${err.message}`);
      return false;
    }
  }, [catalog, config, log, presetSelections]);

  const runNodeSteps = useCallback(async (nodeId: string) => {
    const steps = allSteps[nodeId];
    if (!steps) return;
    setRunningNodeId(nodeId);
    setIsRunning(true);
    initCtx();
    // Always sync latest captureRideId from input
    ctxRef.current.captureRideId = captureRideIdRef.current || undefined;
    log('info', `-- Running: ${nodeId} --`);

    for (const step of steps) {
      if (abortRef.current) break;
      const ok = await executeStep(step);
      if (!ok) break;
      if (ctxRef.current.bookingTerminal) {
        log('warn', `Booking is ${ctxRef.current.bookingStatus} — ${ctxRef.current.cancellationReason || 'terminal state'}. Stopping.`);
        break;
      }
      await new Promise(r => setTimeout(r, 300));
    }
    setIsRunning(false);
    setRunningNodeId(null);
  }, [allSteps, executeStep, initCtx, log]);

  const makeDriverAvailable = useCallback(async () => {
    initCtx();
    // Set driverLocation from selected index
    const locs = getLocationsForCity(selectedCity);
    const loc = locs[driverLocationIdx]?.gps || locs[0]?.gps;
    ctxRef.current.searchOrigin = loc;
    ctxRef.current.driverLocation = loc;

    setIsRunning(true);
    setRunningNodeId('driver-setup');
    log('info', '-- Making driver available --');

    for (const step of allSteps['driver-setup']) {
      const ok = await executeStep(step);
      if (!ok) break;
      await new Promise(r => setTimeout(r, 300));
    }
    setDriverAvailable(true);
    setIsRunning(false);
    setRunningNodeId(null);
  }, [allSteps, executeStep, initCtx, log, selectedCity, driverLocationIdx]);

  const runAll = useCallback(async () => {
    if (isRunning) return;
    abortRef.current = false;
    // Full reset for Run All — start fresh
    ctxRef.current = {};
    initCtx();

    // Ensure driver is available first (only for flows that need it)
    if (activeFlow.hasDriverSetup && !driverAvailable) {
      const locs = getLocationsForCity(selectedCity);
      const loc = locs[driverLocationIdx]?.gps || locs[0]?.gps;
      ctxRef.current.searchOrigin = loc;
      ctxRef.current.driverLocation = loc;

      setIsRunning(true);
      setRunningNodeId('driver-setup');
      log('info', '-- Making driver available --');
      for (const step of allSteps['driver-setup']) {
        if (abortRef.current) { setIsRunning(false); setRunningNodeId(null); return; }
        const ok = await executeStep(step);
        if (!ok) { setIsRunning(false); setRunningNodeId(null); return; }
        await new Promise(r => setTimeout(r, 300));
      }
      setDriverAvailable(true);
    } else {
      setIsRunning(true);
    }

    // Run each node in order
    // Replace 'fulfillment' in node order with the selected outcome, add tip after fulfillment
    const nodeOrder = activeFlow.nodeOrder.flatMap(n => {
      if (n === 'fulfillment') {
        if (selectedOutcome === 'fulfillment') return ['fulfillment', 'add-tip'];
        return [selectedOutcome];
      }
      return [n];
    });
    for (const nodeId of nodeOrder) {
      if (abortRef.current) break;
      if (skippedNodes[nodeId]) {
        log('info', `-- Skipping: ${nodeId} --`);
        continue;
      }
      setRunningNodeId(nodeId);
      ctxRef.current.captureRideId = captureRideIdRef.current || undefined;
      log('info', `-- Running: ${nodeId} --`);
      const steps = allSteps[nodeId];
      let failed = false;
      for (const step of steps) {
        if (abortRef.current) { failed = true; break; }
        const ok = await executeStep(step);
        if (!ok) { failed = true; break; }
        if (ctxRef.current.bookingTerminal) {
          log('warn', `Booking is ${ctxRef.current.bookingStatus} — ${ctxRef.current.cancellationReason || 'terminal state'}. Stopping flow.`);
          failed = true;
          break;
        }
        await new Promise(r => setTimeout(r, 300));
      }
      if (failed) break;
    }

    setIsRunning(false);
    setRunningNodeId(null);
    stopLocationPinger();
    log('info', '-- Flow complete --');
  }, [isRunning, initCtx, driverAvailable, allSteps, executeStep, log, selectedCity, driverLocationIdx]);

  const stop = useCallback(() => {
    abortRef.current = true;
    stopLocationPinger();
    setIsRunning(false);
    setRunningNodeId(null);
  }, []);

  const clearLogs = useCallback(() => setLogs([]), []);

  // --- Resizable log panel ---
  const [logWidth, setLogWidth] = useState(420);
  const draggingRef = useRef(false);

  const onResizeStart = useCallback((e: React.MouseEvent) => {
    e.preventDefault();
    draggingRef.current = true;
    const startX = e.clientX;
    const startW = logWidth;
    const onMove = (ev: MouseEvent) => {
      if (!draggingRef.current) return;
      const delta = startX - ev.clientX;
      setLogWidth(Math.max(250, Math.min(900, startW + delta)));
    };
    const onUp = () => {
      draggingRef.current = false;
      document.removeEventListener('mousemove', onMove);
      document.removeEventListener('mouseup', onUp);
      document.body.style.cursor = '';
      document.body.style.userSelect = '';
    };
    document.addEventListener('mousemove', onMove);
    document.addEventListener('mouseup', onUp);
    document.body.style.cursor = 'col-resize';
    document.body.style.userSelect = 'none';
  }, [logWidth]);

  return (
    <div className="app">
      <div className="main">
        <ConfigBar config={config} onChange={setConfig} onRun={runAll} onStop={stop} isRunning={isRunning}
          onCityChange={setSelectedCity} onDriverChange={(token, variant, merchantId, personId) => { setSelectedDriverToken(token); setSelectedDriverVariant(variant || ''); setSelectedDriverMerchantId(merchantId || ''); setSelectedDriverPersonId(personId || ''); }} />
        <div className="content-wrapper">
          <div className="content">
            <RideFlowTree
              flows={FLOWS.map(f => ({ id: f.id, name: f.name, description: f.description }))}
              activeFlowId={activeFlowId}
              onFlowChange={setActiveFlowId}
              city={selectedCity}
              results={stepResults}
              isRunning={isRunning}
              runningNodeId={runningNodeId}
              driverLocationIdx={driverLocationIdx}
              onDriverLocationChange={setDriverLocationIdx}
              fromLocationIdx={fromLocationIdx}
              toLocationIdx={toLocationIdx}
              onFromLocationChange={setFromLocationIdx}
              onToLocationChange={setToLocationIdx}
              paymentPreset={paymentPreset}
              onPaymentPresetChange={setPaymentPreset}
              paymentMethods={paymentMethods}
              selectedPaymentMethodId={selectedPaymentMethodId}
              onPaymentMethodChange={setSelectedPaymentMethodId}
              paymentMethodsLoading={paymentMethodsLoading}
              captureRideId={captureRideId}
              onCaptureRideIdChange={setCaptureRideId}
              tipAmount={tipAmount}
              onTipAmountChange={setTipAmount}
              skipTip={skipTip}
              onSkipTipChange={setSkipTip}
              rideEndMode={rideEndMode}
              onRideEndModeChange={(m) => setRideEndMode(m as 'actual' | 'downward-recompute')}
              onMakeDriverAvailable={makeDriverAvailable}
              onRunNode={runNodeSteps}
              onRunAll={runAll}
              onStop={stop}
              driverAvailable={driverAvailable}
              selectedOutcome={selectedOutcome}
              onOutcomeChange={setSelectedOutcome}
              skippedNodes={skippedNodes}
              onToggleSkipNode={(id) => setSkippedNodes(prev => ({ ...prev, [id]: !prev[id] }))}
            />
          </div>
          <div className="log-resize-handle" onMouseDown={onResizeStart} />
          <div className="content-logs" style={{ width: logWidth }}>
            <LogPanel logs={logs} onClear={clearLogs} />
          </div>
        </div>
      </div>
    </div>
  );
}

export default App;
