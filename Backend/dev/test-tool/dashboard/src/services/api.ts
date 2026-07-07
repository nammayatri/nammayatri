import axios, { AxiosResponse } from 'axios';
import { Config, Step } from '../types';
import { ApiDef } from '../api-catalog/types';
import { ParsedStep } from './postman-parser';
import { resolveVariables, executeTestScript, executePrereqScript, VariableStores, PostmanRuntimeResult } from './postman-runtime';
import { PROXY_BASE } from '../config';

export interface ApiResult {
  ok: boolean;
  status: number;
  data: any;
  elapsed: number;
}

// ── Coverage Auto-Recording ──

let _coverageRunId: string = `run-${Date.now()}`;
let _coverageBatch: any[] = [];
let _coverageFlushTimer: ReturnType<typeof setTimeout> | null = null;

export function startNewCoverageRun(id?: string) {
  _coverageRunId = id || `run-${Date.now()}`;
  _coverageBatch = [];
}

function _extractFingerprint(body: any): Record<string, any> {
  if (!body || typeof body !== 'object') return {};
  const fp: Record<string, any> = {};

  const interesting = (val: any): boolean => {
    if (val === null || val === undefined) return true;
    if (typeof val === 'boolean') return true;
    if (typeof val === 'string' && val === val.toUpperCase() && val.length > 1 && !val.includes(' ')) return true; // enum-like
    return false;
  };

  for (const [key, val] of Object.entries(body)) {
    if (interesting(val)) {
      fp[key] = val;
    } else if (typeof val === 'object' && val !== null && !Array.isArray(val)) {
      // One level deep for nested objects (e.g. paymentInstrument.instrumentType)
      for (const [subKey, subVal] of Object.entries(val)) {
        if (interesting(subVal)) {
          fp[`${key}.${subKey}`] = subVal;
        }
      }
    }
  }
  return fp;
}

function _normalizePath(path: string): string {
  // Replace UUIDs and IDs with placeholders for grouping
  return path.replace(/[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}/gi, '{id}')
             .replace(/\/[0-9a-f]{24,}/gi, '/{id}');
}

function _recordCoverageHit(service: string, method: string, path: string, body?: any) {
  const hit = {
    runId: _coverageRunId,
    service,
    method,
    path: _normalizePath(path),
    fingerprint: _extractFingerprint(body),
    timestamp: Date.now() / 1000,
  };
  _coverageBatch.push(hit);

  // Flush in batches (debounced 2s)
  if (_coverageFlushTimer) clearTimeout(_coverageFlushTimer);
  _coverageFlushTimer = setTimeout(_flushCoverageBatch, 2000);
}

function _flushCoverageBatch() {
  if (_coverageBatch.length === 0) return;
  const batch = [..._coverageBatch];
  _coverageBatch = [];
  // Fire and forget
  axios.post(`${PROXY_BASE}/api/coverage/record`, batch, { timeout: 5000 }).catch(() => {});
}

// Flush on page unload
if (typeof window !== 'undefined') {
  window.addEventListener('beforeunload', _flushCoverageBatch);
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
    const token = ctx.driverToken || ctx.driverPersonId;
    const url = `${PROXY_BASE}/proxy/lts/driver/location`;
    const body = [{ pt: { lat: origin.lat, lon: origin.lon }, ts: new Date().toISOString() }];

    // Ping ONCE with the driver's actual vehicle variant. LTS allows only one
    // in-flight location update per driver token — firing several variants
    // concurrently makes all but one return HTTP 429 (and the survivor is
    // random), so the driver lands in an unpredictable geo-bucket. A single
    // ping mirrors production and is enough: LTS nearBy returns the driver to
    // the allocator regardless of which variant bucket it sits in, and the test
    // flow selects the estimate matching this same variant. Falls back to SEDAN
    // only if the variant is unknown.
    const vt: string = ctx.driverVehicleVariant || 'SEDAN';
    const headers: Record<string, string> = {
      'Content-Type': 'application/json',
      'vt': vt,
      'dm': 'ONLINE',
    };
    if (token) headers['token'] = token;
    if (resolvedMerchantId) headers['mid'] = resolvedMerchantId;
    const start = performance.now();
    try {
      const resp = await axios.post(url, body, { headers, timeout: 5000 });
      const ms = Math.round(performance.now() - start);
      pingerLogFn?.('req', `[Pinger] POST /driver/location (${vt}) → ${resp.status} (${ms}ms) mid=${resolvedMerchantId || 'NONE'}`);
    } catch (err: any) {
      const ms = Math.round(performance.now() - start);
      const status = err.response?.status || 0;
      pingerLogFn?.('error', `[Pinger] POST /driver/location (${vt}) → ${status} FAILED (${ms}ms) ${err.message}`);
    }
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
    // LTS validates the registration token via driver-app /internal/auth (verifyPerson),
    // so it needs the driver registration token — not the person_id.
    const token = step.service === 'lts' ? (ctx.driverToken || ctx.driverPersonId || config.token)
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

    // Auto-record coverage
    _recordCoverageHit(step.service, step.method, path, body);

    return { ok: true, status: resp.status, data: resp.data, elapsed };
  } catch (err: any) {
    const elapsed = Math.round(performance.now() - start);
    // Still record the hit (even failures are coverage)
    _recordCoverageHit(step.service, step.method, path, body);
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
  'juspay-payment': '/proxy/juspay-payment',
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
  /** Resolved request headers (with all {{var}} substituted) */
  resolvedHeaders?: Record<string, string>;
  /** Response headers (lower-cased keys, as returned by axios) */
  responseHeaders?: Record<string, string>;
  /** Upstream latency in ms — measured on context-api side (excludes browser/nginx/proxy overhead) */
  upstreamMs: number;
  /** True if the step's prerequest script invoked pm.execution.skipRequest() */
  skipped?: boolean;
}

/**
 * Execute a Postman collection step with variable substitution, script execution, and log capture.
 *
 * @param attachments  Files attached to this step's `formdata` file fields,
 *                     keyed by the formdata field's `key`. When a step has
 *                     `formdataFields` containing a `type: 'file'` entry and
 *                     no attachment is provided for that key, the step
 *                     self-skips (analogous to `pm.execution.skipRequest`).
 */
export async function callPostmanStep(
  step: ParsedStep,
  stores: VariableStores,
  attachments?: Record<string, File>,
): Promise<PostmanStepResult> {
  // 1. Execute prerequest script (variable init only, delays handled by test script)
  if (step.prereqScript) {
    const prereq = await executePrereqScript(step.prereqScript, stores);
    if (prereq.skipped) {
      return {
        ok: true, status: 0, data: null, elapsed: 0, upstreamMs: 0,
        assertions: [], consoleLogs: prereq.consoleLogs, serviceLogs: {},
        resolvedUrl: step.pathTemplate, resolvedBody: undefined, resolvedHeaders: {}, responseHeaders: {},
        skipped: true,
      };
    }
  }

  // Log + mock-hits capture is the *caller's* responsibility now — see
  // startStepCapture() in CollectionRunner. It opens both streams before this
  // call, signals done() after, and stitches serviceLogs/mockHits onto the
  // step state.

  // 4. Resolve URL
  const resolvedPath = resolveVariables(step.pathTemplate, stores);
  const proxyPrefix = SERVICE_PROXY_MAP[step.service] ?? '';
  const url = `${PROXY_BASE}${proxyPrefix}${resolvedPath}`;

  // 5. Resolve headers
  const headers: Record<string, string> = {};
  for (const [k, v] of Object.entries(step.headers)) {
    headers[k] = resolveVariables(v, stores);
  }

  const _svcEnvVar: Record<string, string> = {
    driver: 'baseURL_namma_P',
    rider: 'baseUrl_app',
    lts: 'baseUrl_lts',
    'provider-dashboard': 'dashboard_base_url',
    'rider-dashboard': 'bap_dashboard_url',
  };
  const _baseUrlVar = _svcEnvVar[step.service];
  const _baseUrlVal = _baseUrlVar ? (stores.environment[_baseUrlVar] ?? stores.collection[_baseUrlVar] ?? '') : '';
  let _proxyTargetHost = '';
  if (_baseUrlVal) {
    try {
      const _pu = new URL(_baseUrlVal);
      _proxyTargetHost = `${_pu.protocol}//${_pu.host}`;
      headers['X-Proxy-Target'] = _proxyTargetHost;
    } catch { /* invalid URL, skip */ }
  }

  // 6. Resolve body
  let body: any = undefined;
  let isMultipart = false;
  if (step.formdataFields && step.formdataFields.length > 0) {
    // Any file field without an attachment => user did not opt in for this step;
    // treat as a skip so the rest of the collection still runs.
    const missingFile = step.formdataFields.find(
      f => f.type === 'file' && !(attachments && attachments[f.key])
    );
    if (missingFile) {
      return {
        ok: true, status: 0, data: null, elapsed: 0, upstreamMs: 0,
        assertions: [],
        consoleLogs: [
          `[skip] "${step.name}" — no file attached for formdata field "${missingFile.key}". Attach a file in the step's file picker to run this step.`,
        ],
        serviceLogs: {},
        resolvedUrl: step.pathTemplate,
        resolvedBody: undefined,
        resolvedHeaders: {},
        responseHeaders: {},
        skipped: true,
      };
    }
    const fd = new FormData();
    for (const f of step.formdataFields) {
      if (f.type === 'file') {
        const file = attachments![f.key];
        fd.append(f.key, file, file.name);
      } else {
        fd.append(f.key, resolveVariables(f.value ?? '', stores));
      }
    }
    body = fd;
    isMultipart = true;
    // Let the browser set Content-Type (with correct boundary). If the caller
    // already put a Content-Type header on the step, drop it so axios doesn't
    // override the boundary.
    delete headers['Content-Type'];
    delete headers['content-type'];
  } else if (step.bodyTemplate) {
    const resolvedBody = resolveVariables(step.bodyTemplate, stores);
    try { body = JSON.parse(resolvedBody); } catch { body = resolvedBody; }
  }

  // 7. Make HTTP call
  const start = performance.now();
  let ok = false;
  let status = 0;
  let data: any = null;
  let responseHeaders: Record<string, string> = {};

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
    responseHeaders = normalizeHeaders(resp.headers);
    ok = status >= 200 && status < 400;
  } catch (err: any) {
    status = err.response?.status ?? 0;
    data = err.response?.data ?? { error: err.message };
    responseHeaders = normalizeHeaders(err.response?.headers);
  }
  void isMultipart;

  const elapsed = Math.round(performance.now() - start);
  // X-Upstream-Latency-Ms = time measured on context-api side for the upstream
  // HTTP call only (excludes browser, nginx, proxy overhead)
  const upstreamMs = parseInt(responseHeaders['x-upstream-latency-ms'] || '0', 10) || elapsed;

  // Auto-record coverage for Postman steps
  _recordCoverageHit(step.service, step.method, resolvedPath, body);

  // 8. Execute test script FIRST (may include delays for async callbacks like FRFS on_search)
  let assertions: PostmanRuntimeResult['assertions'] = [];
  let consoleLogs: string[] = [];
  let scriptError: string | undefined;

  if (step.testScript) {
    // Test script can use pm.sendRequest + real setTimeout from inside (postman-runtime.ts drains all queued
    // async callbacks before resolving), so polling/sleep-then-check patterns just work — no special-casing here.
    const result = await executeTestScript(step.testScript, data, status, stores);
    assertions = result.assertions;
    consoleLogs = result.consoleLogs;
    scriptError = result.error;
  }

  // resolvedUrl = full URL the proxy forwarded to (shows in log panel for debugging)
  const resolvedUrl = _proxyTargetHost ? `${_proxyTargetHost}${resolvedPath}` : resolvedPath;
  return { ok, status, data, elapsed, upstreamMs, assertions, consoleLogs, scriptError, serviceLogs: {}, resolvedUrl, resolvedBody: body, resolvedHeaders: headers, responseHeaders };
}

function normalizeHeaders(h: any): Record<string, string> {
  if (!h) return {};
  const out: Record<string, string> = {};
  // Axios v1 returns AxiosHeaders (with forEach). v0 returns a plain object.
  const visit = (k: string, v: any) => {
    if (v == null) return;
    out[String(k).toLowerCase()] = Array.isArray(v) ? v.join(', ') : String(v);
  };
  if (typeof h.forEach === 'function') {
    h.forEach((v: any, k: string) => visit(k, v));
  } else {
    Object.keys(h).forEach((k) => visit(k, (h as any)[k]));
  }
  return out;
}
