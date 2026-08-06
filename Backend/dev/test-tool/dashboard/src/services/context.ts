import axios from 'axios';
import { PROXY_BASE as CONTEXT_API, getServiceUrl, getServicePort } from '../config';

export interface RiderInfo {
  person_id: string;
  first_name: string;
  merchant: string;
  city: string;
  token: string;
  verified: boolean;
}

export interface DriverInfo {
  person_id: string;
  first_name: string;
  merchant: string;
  merchant_id: string;
  city: string;
  currency: string;
  token: string;
  verified: boolean;
  vehicle_variant?: string;
}

export interface MerchantInfo {
  id: string;
  short_id: string;
  name: string;
  city_id: string;
  city: string;
  country: string;
  online_payment?: boolean;
  currency?: string;
}

export interface VariantInfo {
  id: string;
  service_tier_type: string;
  name: string;
  seating_capacity: number;
  is_enabled: boolean;
  merchant: string;
  city: string;
  currency: string;
}

export interface AdminCredentials {
  [merchantShortId: string]: { email: string; password: string };
}

export interface TestContext {
  merchants: { rider_merchants: MerchantInfo[]; driver_merchants: MerchantInfo[] };
  riders: RiderInfo[];
  drivers: DriverInfo[];
  variants: VariantInfo[];
  admin_credentials: AdminCredentials;
}

function safeArray<T>(val: any): T[] {
  return Array.isArray(val) ? val : [];
}

export async function fetchTestContext(): Promise<TestContext | null> {
  try {
    const resp = await axios.get(`${CONTEXT_API}/api/context`, { timeout: 5000 });
    const d = resp.data;
    return {
      merchants: {
        rider_merchants: safeArray(d?.merchants?.rider_merchants),
        driver_merchants: safeArray(d?.merchants?.driver_merchants),
      },
      riders: safeArray(d?.riders),
      drivers: safeArray(d?.drivers),
      variants: safeArray(d?.variants),
      admin_credentials: d?.admin_credentials || {},
    };
  } catch {
    return null;
  }
}

export async function fetchRiders(): Promise<RiderInfo[]> {
  try {
    const resp = await axios.get(`${CONTEXT_API}/api/riders`, { timeout: 5000 });
    return safeArray(resp.data);
  } catch {
    return [];
  }
}

export async function fetchDrivers(): Promise<DriverInfo[]> {
  try {
    const resp = await axios.get(`${CONTEXT_API}/api/drivers`, { timeout: 5000 });
    return safeArray(resp.data);
  } catch {
    return [];
  }
}

// ── Collection Scanner ──

export interface CollectionEnvironment {
  filename: string;
  envType: string;
  envName: string;
  name: string;
  city: string;
  state: string;
  merchant: string;
  bapShortId: string;
  origin: { lat: number; lon: number };
  destination: { lat: number; lon: number };
  compatibleEnvs: string[];
  variables: Record<string, string>;
}

export interface CollectionSuite {
  filename: string;
  name: string;
  description: string;
  itemCount: number;
}

export interface CollectionGroup {
  directory: string;
  envTypes: string[];
  environments: CollectionEnvironment[];
  compatibleEnvs: string[];
  suites: CollectionSuite[];
}

export interface ConfigSyncStatus {
  running: boolean;
  from: string | null;
  started_at: number | null;
  finished_at: number | null;
  exit_code: number | null;
  error: string | null;
  log: string[];
  last_synced: { from: string; finished_at: number } | null;
}

export async function fetchConfigSyncStatus(): Promise<ConfigSyncStatus | null> {
  try {
    const resp = await axios.get(`${CONTEXT_API}/api/config-sync/status`, { timeout: 5000 });
    return resp.data as ConfigSyncStatus;
  } catch {
    return null;
  }
}

export interface MockHit {
  id: number;
  timestamp: number;
  duration_ms: number;
  service: string | null;
  method: string;
  path: string;
  query: string;
  url: string;
  request_headers: Record<string, string>;
  request_body: any;
  status: number;
  response_headers: Record<string, string>;
  response_body: any;
  run_id: string | null;
  admin: boolean;
}

// Runtime port discovery: when REACT_APP_MOCK_SERVER_BASE is unset, fall
// back to getServiceUrl('mock-server') which reads the resolved port from
// /api/ports (populated at App boot via refreshPortsTable).
function _resolveMockServerBase(): string {
  const buildTime = process.env.REACT_APP_MOCK_SERVER_BASE;
  return (buildTime || getServiceUrl('mock-server')).replace(/\/+$/, '');
}
export function mockServerBase(): string { return _resolveMockServerBase(); }
const MOCK_SERVER_BASE = _resolveMockServerBase();
// Re-export MOCK_SERVER_BASE for code paths that read the const directly.
export { MOCK_SERVER_BASE };

export async function triggerConfigSync(fromEnv: string, forceFetch = false): Promise<{ started: boolean; error?: string }> {
  try {
    const resp = await axios.post(`${CONTEXT_API}/api/config-sync/import`, { from: fromEnv, forceFetch }, { timeout: 10000 });
    return { started: !!resp.data?.started };
  } catch (e: any) {
    return { started: false, error: e?.response?.data?.error ?? String(e) };
  }
}

const TOLL_COLLECTION_DIRS = new Set(['TollRideFlow', 'TollConfigFlow']);

export function isTollCollectionDir(directory: string): boolean {
  return TOLL_COLLECTION_DIRS.has(directory);
}

async function waitForConfigSyncDone(timeoutMs = 600000): Promise<{ ok: boolean; error?: string }> {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    const st = await fetchConfigSyncStatus();
    if (!st) {
      await new Promise(r => setTimeout(r, 2000));
      continue;
    }
    if (!st.running) {
      if (st.exit_code === 0 && !st.error) return { ok: true };
      return { ok: false, error: st.error ?? `config-sync exit ${st.exit_code ?? 'unknown'}` };
    }
    await new Promise(r => setTimeout(r, 2000));
  }
  return { ok: false, error: 'config-sync timed out' };
}

/** test-context-api without the toll seed routes (405 / 404). */
function isMissingSeedEndpoint(status?: number): boolean {
  return status === 405 || status === 404;
}

/** Re-apply Backend/dev/local-testing-data/*.sql (includes toll-dashboard-access.sql). */
export async function reapplyLocalTestingData(): Promise<{ ok: boolean; error?: string }> {
  let lastError = 'seed endpoint not available';
  for (const method of ['post', 'get'] as const) {
    try {
      const resp = await axios[method](`${CONTEXT_API}/api/local-testing-data/reapply`, {}, { timeout: 120000 });
      if (resp.data?.ok) return { ok: true };
      lastError = resp.data?.error ?? 'reapply failed';
    } catch (e: any) {
      if (isMissingSeedEndpoint(e?.response?.status)) continue;
      return { ok: false, error: e?.response?.data?.error ?? String(e) };
    }
  }
  return { ok: false, error: lastError };
}

export async function seedTollDashboardAccess(
  syncEnv = 'master',
  options: { skipConfigSync?: boolean } = {},
): Promise<{ ok: boolean; error?: string; skipped?: boolean }> {
  let lastError = 'seed endpoint not available';
  for (const method of ['post', 'get'] as const) {
    try {
      const resp = await axios[method](`${CONTEXT_API}/api/integration-tests/seed-toll-dashboard`, {}, { timeout: 15000 });
      const data = resp.data as { ok?: boolean; files?: { name: string; ok: boolean; error?: string }[] };
      if (data?.ok) return { ok: true };
      const failed = (data?.files ?? []).filter(f => !f.ok).map(f => `${f.name}: ${f.error ?? 'failed'}`).join('; ');
      return { ok: false, error: failed || 'seed failed' };
    } catch (e: any) {
      if (isMissingSeedEndpoint(e?.response?.status)) {
        lastError = e?.response?.data?.error ?? lastError;
        continue;
      }
      return { ok: false, error: e?.response?.data?.error ?? String(e) };
    }
  }

  const reapply = await reapplyLocalTestingData();
  if (reapply.ok) return { ok: true };

  // Config-sync already ran in this session — local-testing-data should include toll access.
  if (options.skipConfigSync) {
    return { ok: false, error: reapply.error ?? lastError, skipped: true };
  }

  const trig = await triggerConfigSync(syncEnv, false);
  if (!trig.started) {
    return {
      ok: false,
      error: trig.error ?? reapply.error ?? lastError,
      skipped: true,
    };
  }
  return waitForConfigSyncDone();
}

export async function fetchCollections(): Promise<CollectionGroup[]> {
  try {
    const resp = await axios.get(`${CONTEXT_API}/api/collections`, { timeout: 5000 });
    return resolveEnvTemplatesInTree(safeArray(resp.data));
  } catch {
    return [];
  }
}

export async function fetchCollection(directory: string, filename: string): Promise<any | null> {
  try {
    const resp = await axios.get(`${CONTEXT_API}/api/collection/${directory}/${filename}`, { timeout: 10000 });
    return resolveEnvTemplatesInTree(resp.data);
  } catch {
    return null;
  }
}

// ── ${VAR:default} resolution for Postman env files & collections ──────────
// The Local_*.postman_environment.json files (and any future templated
// collection bodies) carry literals like "http://localhost:${RIDER_APP_PORT:8013}/v2".
// Resolve them against the runtime ports table fetched via /api/ports, so
// the same env files work whether the user is on default ports or under
// resolve-ports.sh remapping. Defaults (after `:`) preserve the existing
// values when env-var → port-key lookup fails.

const ENV_VAR_TO_PORT_KEY: Record<string, string> = {
  RIDER_APP_PORT: 'rider-app',
  DRIVER_APP_PORT: 'dynamic-offer-driver-app',
  RIDER_DASHBOARD_PORT: 'rider-dashboard',
  PROVIDER_DASHBOARD_PORT: 'provider-dashboard',
  MOCK_SERVER_PORT: 'mock-server',
  LOCATION_TRACKING_SERVICE_PORT: 'location-tracking-service',
  TEST_CONTEXT_API_PORT: 'test-context-api',
  MOCK_FCM_PORT: 'mock-fcm',
  MOCK_SMS_PORT: 'mock-sms',
  MOCK_IDFY_PORT: 'mock-idfy',
  MOCK_GOOGLE_PORT: 'mock-google',
  MOCK_REGISTRY_PORT: 'mock-registry',
  BECKN_GATEWAY_PORT: 'beckn-gateway',
  UNIFIED_DASHBOARD_PORT: 'unified-dashboard',
};

const ENV_TEMPLATE_RE = /\$\{([A-Z_][A-Z0-9_]*)(?::([^}]*))?\}/g;

export function expandEnvTemplates(s: string): string {
  return s.replace(ENV_TEMPLATE_RE, (_full, name: string, dflt: string | undefined) => {
    const key = ENV_VAR_TO_PORT_KEY[name];
    if (key) {
      const p = getServicePort(key);
      if (p) return String(p);
    }
    return dflt ?? '';
  });
}

function resolveEnvTemplatesInTree<T>(node: T): T {
  if (Array.isArray(node)) return node.map(resolveEnvTemplatesInTree) as any;
  if (node && typeof node === 'object') {
    const out: any = {};
    for (const [k, v] of Object.entries(node)) out[k] = resolveEnvTemplatesInTree(v);
    return out;
  }
  if (typeof node === 'string') return expandEnvTemplates(node) as any;
  return node;
}

// ── Service Log Capture (tail -f based) ──

export async function startLogCapture(): Promise<string> {
  try {
    const resp = await axios.post(`${CONTEXT_API}/api/logs/start`, {}, { timeout: 3000 });
    return resp.data?.token ?? '';
  } catch {
    return '';
  }
}

export async function stopLogCapture(token: string): Promise<Record<string, string>> {
  try {
    const resp = await axios.post(`${CONTEXT_API}/api/logs/stop`, { token }, { timeout: 10000 });
    return resp.data?.logs ?? {};
  } catch {
    return {};
  }
}
