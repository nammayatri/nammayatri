import axios, { AxiosResponse } from 'axios';
import { Config, Step } from '../types';
import { ApiDef } from '../api-catalog/types';
import { ParsedStep } from './postman-parser';
import { resolveVariables, executeTestScript, executePrereqScript, VariableStores, PostmanRuntimeResult } from './postman-runtime';
import { startLogCapture, stopLogCapture } from './context';

const PROXY_BASE = 'http://localhost:7082';

export interface ApiResult {
  ok: boolean;
  status: number;
  data: any;
  elapsed: number;
}

// --- Driver location pinger: keeps LTS geo bucket alive ---
let locationPingerInterval: ReturnType<typeof setInterval> | null = null;
let pingerLogFn: ((level: 'info' | 'req' | 'error', msg: string) => void) | null = null;

// Global log setter — called from App.tsx so pinger can write to dashboard log panel
let globalLogFn: ((level: 'info' | 'req' | 'error', msg: string) => void) | null = null;
export function setGlobalLog(fn: (level: 'info' | 'req' | 'error', msg: string) => void) { globalLogFn = fn; }

export function startLocationPinger(ctx: Record<string, any>, log?: (level: 'info' | 'req' | 'error', msg: string) => void) {
  stopLocationPinger();
  pingerLogFn = log || globalLogFn || null;

  // Resolve merchant_id: from ctx, or fetch from context API on first ping
  let resolvedMerchantId = ctx.driverMerchantId || '';
  const ensureMerchantId = async () => {
    if (resolvedMerchantId || !ctx.driverToken) return;
    try {
      const resp = await axios.get(`${PROXY_BASE}/api/drivers`, { timeout: 3000 });
      const drivers = resp.data;
      if (Array.isArray(drivers)) {
        const match = drivers.find((d: any) => d.token === ctx.driverToken);
        if (match?.merchant_id) {
          resolvedMerchantId = match.merchant_id;
          ctx.driverMerchantId = resolvedMerchantId;
          pingerLogFn?.('info', `[Pinger] Resolved merchant_id: ${resolvedMerchantId}`);
        }
      }
    } catch { /* ignore */ }
  };

  const ping = async () => {
    await ensureMerchantId();
    const origin = ctx.searchOrigin || ctx.driverLocation || { lat: 10.0739, lon: 76.2733 };
    const token = ctx.driverPersonId || ctx.driverToken;
    const headers: Record<string, string> = {
      'Content-Type': 'application/json',
      'vt': ctx.driverVehicleVariant || 'SUV',
      'dm': 'ONLINE',
    };
    if (token) headers['token'] = token;
    if (resolvedMerchantId) headers['mid'] = resolvedMerchantId;
    const body = [{ pt: { lat: origin.lat, lon: origin.lon }, ts: new Date().toISOString() }];
    const url = `${PROXY_BASE}/proxy/lts/driver/location`;
    const start = performance.now();
    axios.post(url, body, { headers, timeout: 5000 })
      .then((resp) => {
        const ms = Math.round(performance.now() - start);
        pingerLogFn?.('req', `[Pinger] POST /driver/location → ${resp.status} (${ms}ms) mid=${resolvedMerchantId || 'NONE'}`);
      })
      .catch((err) => {
        const ms = Math.round(performance.now() - start);
        const status = err.response?.status || 0;
        pingerLogFn?.('error', `[Pinger] POST /driver/location → ${status} FAILED (${ms}ms) ${err.message}`);
      });
  };
  pingerLogFn?.('info', '[Pinger] Started — pinging driver location every 10s');
  ping();
  locationPingerInterval = setInterval(ping, 10_000);
}

export function stopLocationPinger() {
  if (locationPingerInterval) {
    clearInterval(locationPingerInterval);
    locationPingerInterval = null;
    pingerLogFn?.('info', '[Pinger] Stopped');
    pingerLogFn = null;
  }
}

/**
 * Call a step, optionally using an API catalog entry for path/body resolution.
 * If catalogApi and selectedPreset are provided, they override the step's path and body.
 */
export async function callStep(
  step: Step,
  config: Config,
  ctx: Record<string, any>,
  catalogApi?: ApiDef,
  selectedPresetId?: string,
): Promise<ApiResult> {
  // Resolve path — from catalog (can be dynamic function) or step
  let path: string;
  if (catalogApi && typeof catalogApi.path === 'function') {
    path = catalogApi.path(ctx);
  } else if (catalogApi && typeof catalogApi.path === 'string') {
    path = catalogApi.path;
  } else if (typeof step.path === 'function') {
    path = step.path(ctx);
  } else {
    path = step.path;
  }

  const proxyPrefix = step.service === 'rider' ? '/proxy/rider'
    : step.service === 'lts' ? '/proxy/lts'
    : step.service === 'provider-dashboard' ? '/proxy/provider-dashboard'
    : step.service === 'mock-idfy' ? '/proxy/mock-idfy'
    : step.service === 'internal' ? ''
    : '/proxy/driver';
  const url = `${PROXY_BASE}${proxyPrefix}${path}`;

  const headers: Record<string, string> = { 'Content-Type': 'application/json' };
  if (step.auth) {
    // Use driver person_id for LTS, fleet auth token for fleet, driver token for driver API, rider token otherwise
    const token = step.service === 'lts' ? (ctx.driverPersonId || ctx.driverToken || config.token)
      : step.service === 'provider-dashboard' ? (ctx.fleetAuthToken || config.token)
      : (step.service === 'driver') ? (ctx.driverToken || config.token) : config.token;
    if (token) headers['token'] = token;
  }

  // Apply extra headers from step
  if (step.extraHeaders) {
    const extra = typeof step.extraHeaders === 'function' ? step.extraHeaders(ctx) : step.extraHeaders;
    Object.assign(headers, extra);
  }

  // Build body — from catalog preset or step body
  let body: any = undefined;
  if (catalogApi?.bodyBuilder && selectedPresetId) {
    const preset = catalogApi.mockDataPresets.find(p => p.id === selectedPresetId);
    if (preset) {
      body = catalogApi.bodyBuilder(preset, ctx);
    }
  } else if (step.body) {
    body = typeof step.body === 'function' ? step.body(ctx) : step.body;
  }

  const start = performance.now();
  try {
    let resp: AxiosResponse;

    switch (step.method) {
      case 'GET':
        resp = await axios.get(url, { headers, timeout: 30000 });
        break;
      case 'POST':
        resp = await axios.post(url, body, { headers, timeout: 30000 });
        break;
      case 'PUT':
        resp = await axios.put(url, body, { headers, timeout: 30000 });
        break;
      case 'DELETE':
        resp = await axios.delete(url, { headers, timeout: 30000 });
        break;
      default:
        resp = await axios.get(url, { headers, timeout: 30000 });
    }

    const elapsed = Math.round(performance.now() - start);

    // Run catalog extractFromResponse
    if (catalogApi?.extractFromResponse) {
      catalogApi.extractFromResponse(resp.data, ctx);
    }

    return { ok: true, status: resp.status, data: resp.data, elapsed };
  } catch (err: any) {
    const elapsed = Math.round(performance.now() - start);
    if (err.response) {
      return { ok: false, status: err.response.status, data: err.response.data, elapsed };
    }
    return { ok: false, status: 0, data: { error: err.message }, elapsed };
  }
}

// ── Postman Collection Step Execution ──

/** Proxy map for legacy custom flows (adds /v2, /ui prefixes) */
export const LEGACY_SERVICE_PROXY_MAP: Record<string, string> = {
  rider: '/proxy/rider',
  driver: '/proxy/driver',
  lts: '/proxy/lts',
  'provider-dashboard': '/proxy/provider-dashboard',
  'mock-idfy': '/proxy/mock-idfy',
};

/** Proxy map for Postman collection steps (path prefix already in pathTemplate from env URL) */
const SERVICE_PROXY_MAP: Record<string, string> = {
  rider: '/proxy/rider-raw',
  driver: '/proxy/driver-raw',
  lts: '/proxy/lts-raw',
  'provider-dashboard': '/proxy/provider-dashboard',
  'rider-dashboard': '/proxy/rider-dashboard',
  'mock-idfy': '/proxy/mock-idfy',
  'mock-server': '/proxy/mock-server',
};

export interface PostmanStepResult extends ApiResult {
  assertions: PostmanRuntimeResult['assertions'];
  consoleLogs: string[];
  scriptError?: string;
  serviceLogs: Record<string, string>;
  /** Resolved URL (with all {{var}} substituted) */
  resolvedUrl: string;
  /** Resolved body (with all {{var}} substituted) */
  resolvedBody?: any;
}

/**
 * Execute a Postman collection step with variable substitution, script execution, and log capture.
 */
export async function callPostmanStep(
  step: ParsedStep,
  stores: VariableStores,
): Promise<PostmanStepResult> {
  // 1. Execute prerequest script (variable init only, delays handled by test script)
  if (step.prereqScript) {
    executePrereqScript(step.prereqScript, stores);
  }

  // 2. Start tail -f on all service logs
  const logToken = await startLogCapture();

  // 4. Resolve URL
  const resolvedPath = resolveVariables(step.pathTemplate, stores);
  const proxyPrefix = SERVICE_PROXY_MAP[step.service] ?? '';
  const url = `${PROXY_BASE}${proxyPrefix}${resolvedPath}`;

  // 5. Resolve headers
  const headers: Record<string, string> = {};
  for (const [k, v] of Object.entries(step.headers)) {
    headers[k] = resolveVariables(v, stores);
  }

  // 6. Resolve body
  let body: any = undefined;
  if (step.bodyTemplate) {
    const resolvedBody = resolveVariables(step.bodyTemplate, stores);
    try { body = JSON.parse(resolvedBody); } catch { body = resolvedBody; }
  }

  // 7. Make HTTP call
  const start = performance.now();
  let ok = false;
  let status = 0;
  let data: any = null;

  try {
    let resp: AxiosResponse;
    const config = { headers, timeout: 30000, validateStatus: () => true };
    if (step.method === 'GET') {
      resp = await axios.get(url, config);
    } else if (step.method === 'POST') {
      resp = await axios.post(url, body, config);
    } else if (step.method === 'PUT') {
      resp = await axios.put(url, body, config);
    } else {
      resp = await axios.delete(url, { ...config, data: body });
    }
    status = resp.status;
    data = resp.data;
    ok = status >= 200 && status < 400;
  } catch (err: any) {
    status = err.response?.status ?? 0;
    data = err.response?.data ?? { error: err.message };
  }

  const elapsed = Math.round(performance.now() - start);

  // 8. Execute test script FIRST (may include delays for async callbacks like FRFS on_search)
  let assertions: PostmanRuntimeResult['assertions'] = [];
  let consoleLogs: string[] = [];
  let scriptError: string | undefined;

  if (step.testScript) {
    // Detect busy-wait loop delay in script: while (Date.now() - start < N) {}
    const busyWaitMatch = step.testScript.match(/Date\.now\(\)\s*-\s*\w+\s*<\s*(\d+)/);
    if (busyWaitMatch) {
      // Replace busy-wait with real async delay (don't block browser)
      const delayMs = parseInt(busyWaitMatch[1], 10);
      await new Promise(r => setTimeout(r, delayMs));
    }

    const result = executeTestScript(step.testScript, data, status, stores);
    assertions = result.assertions;
    consoleLogs = result.consoleLogs;
    scriptError = result.error;
  }

  // 9. Stop tail -f and collect captured logs AFTER test script + delay (captures all async service activity)
  const serviceLogs = logToken ? await stopLogCapture(logToken) : {};

  return { ok, status, data, elapsed, assertions, consoleLogs, scriptError, serviceLogs, resolvedUrl: resolvedPath, resolvedBody: body };
}
