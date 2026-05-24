import axios from 'axios';
import { PROXY_BASE as CONTEXT_API } from '../config';

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

const MOCK_SERVER_BASE = (process.env.REACT_APP_MOCK_SERVER_BASE || 'http://localhost:8080').replace(/\/+$/, '');

export function mockServerBase(): string { return MOCK_SERVER_BASE; }

export async function triggerConfigSync(fromEnv: string, forceFetch = false): Promise<{ started: boolean; error?: string }> {
  try {
    const resp = await axios.post(`${CONTEXT_API}/api/config-sync/import`, { from: fromEnv, forceFetch }, { timeout: 10000 });
    return { started: !!resp.data?.started };
  } catch (e: any) {
    return { started: false, error: e?.response?.data?.error ?? String(e) };
  }
}

export async function fetchCollections(): Promise<CollectionGroup[]> {
  try {
    const resp = await axios.get(`${CONTEXT_API}/api/collections`, { timeout: 5000 });
    return safeArray(resp.data);
  } catch {
    return [];
  }
}

export async function fetchCollection(directory: string, filename: string): Promise<any | null> {
  try {
    const resp = await axios.get(`${CONTEXT_API}/api/collection/${directory}/${filename}`, { timeout: 10000 });
    return resp.data;
  } catch {
    return null;
  }
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
