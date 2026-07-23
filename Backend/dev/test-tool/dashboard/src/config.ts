// Test-context-api base URL.
//   - Build-time default from REACT_APP_PROXY_BASE (or http://localhost:7082).
//   - Runtime override stored in localStorage under "ny.contextApiBase" — set
//     by the Remote panel after pointing the dashboard at an SSH'd context-api.
const _readContextApiOverride = (): string | null => {
  try {
    return (typeof window !== 'undefined' && window.localStorage?.getItem('ny.contextApiBase')) || null;
  } catch {
    return null;
  }
};
const _proxyDefault = process.env.REACT_APP_PROXY_BASE ?? 'http://localhost:7082';
export const PROXY_BASE = _readContextApiOverride() || _proxyDefault;

export function setContextApiBase(url: string | null): void {
  try {
    if (url && url.trim()) {
      window.localStorage.setItem('ny.contextApiBase', url.trim());
    } else {
      window.localStorage.removeItem('ny.contextApiBase');
    }
    // Force a reload so all modules pick up the new PROXY_BASE binding.
    window.location.reload();
  } catch {
    /* ignore */
  }
}

export function getContextApiBaseDefault(): string { return _proxyDefault; }
export function getContextApiBaseOverride(): string | null { return _readContextApiOverride(); }

// ── Runtime port discovery ───────────────────────────────────────────────────
// The dashboard learns the per-user port mapping by calling context-api's
// GET /api/ports on first paint. The response is cached in localStorage
// (keyed by context-api base URL so multiple remote stacks don't collide)
// and consumed by getServicePort(). Until the fetch completes — or when
// the call fails (e.g. offline / dev preview) — getServicePort() falls
// back to the build-time defaults below, so nothing breaks.

const PORTS_CACHE_KEY = `ny.portsTable::${PROXY_BASE}`;

// Build-time defaults match the literal port numbers in Backend/nix/services/ports.nix.
const DEFAULT_PORTS: Record<string, number> = {
  'rider-app': 8013,
  'dynamic-offer-driver-app': 8016,
  'rider-dashboard': 8017,
  'provider-dashboard': 8018,
  'mock-server': 8080,
  'location-tracking-service': 8081,
  'beckn-gateway': 8015,
  'mock-registry': 8020,
  'unified-dashboard': 8022,
  'test-context-api': 7082,
  'metabase': 3001,
  'redis-commander': 8431,
  'victoria-metrics': 8428,
  'db-manager-frontend': 5183,
  'db-manager-backend': 3010,
};

let _portsTable: Record<string, number> = (() => {
  try {
    const raw = window.localStorage?.getItem(PORTS_CACHE_KEY);
    if (raw) return JSON.parse(raw);
  } catch { /* ignore */ }
  return { ...DEFAULT_PORTS };
})();

export function getServicePort(name: string): number {
  return _portsTable[name] ?? DEFAULT_PORTS[name] ?? 0;
}

export function getServiceUrl(name: string, pathSuffix = ''): string {
  return `http://localhost:${getServicePort(name)}${pathSuffix}`;
}

// Async refresh. Invoked once on app boot (see App.tsx). Subsequent renders
// of consumer components will see the resolved table.
export async function refreshPortsTable(): Promise<void> {
  try {
    const resp = await fetch(`${PROXY_BASE}/api/ports`, { cache: 'no-store' });
    if (!resp.ok) return;
    const body = await resp.json();
    if (body && body.ports && typeof body.ports === 'object') {
      _portsTable = { ...DEFAULT_PORTS, ...body.ports };
      try { window.localStorage?.setItem(PORTS_CACHE_KEY, JSON.stringify(_portsTable)); } catch { /* ignore */ }
    }
  } catch {
    /* ignore — fall back to defaults / cached values */
  }
}

export const LOCAL_API_BASE = process.env.REACT_APP_LOCAL_API_BASE || 'http://localhost:7083';
// MOCK_SERVER_URL / RIDER_URL / DRIVER_URL / PROVIDER_DASHBOARD_URL are kept
// as live getters so they pick up runtime-resolved ports after refreshPortsTable.
export const MOCK_SERVER_URL = process.env.REACT_APP_MOCK_SERVER_URL || getServiceUrl('mock-server');
export const RIDER_URL = process.env.REACT_APP_RIDER_URL || getServiceUrl('rider-app');
export const DRIVER_URL = process.env.REACT_APP_DRIVER_URL || getServiceUrl('dynamic-offer-driver-app');
export const PROVIDER_DASHBOARD_URL = process.env.REACT_APP_PROVIDER_DASHBOARD_URL || getServiceUrl('provider-dashboard');
// Default URL when no per-env entry exists.
export const CONFIG_SYNC_BASE = process.env.REACT_APP_CONFIG_SYNC_BASE ?? 'http://localhost:8090';

// Per-source-env URL map. Each env's cluster runs its own config-sync pod
// (needs direct DB access to that env). In a deployed dashboard the
// recommended values are RELATIVE paths so the browser stays same-origin
// and the dashboard pod's nginx proxies to the in-cluster config-sync:
//
//   REACT_APP_CONFIG_SYNC_URLS='{"master":"/config-sync/master",
//                                "prod":"/config-sync/prod"}'
//
// (See the ny-test-nginx-conf ConfigMap in the deployment for the matching
// location blocks.)
// Absolute URLs still work for local dev or one-off pointing at a remote
// host. Anything missing falls back to CONFIG_SYNC_BASE.
const _parseUrlMap = (): Record<string, string> => {
  const raw = process.env.REACT_APP_CONFIG_SYNC_URLS;
  if (!raw) return {};
  try {
    const parsed = JSON.parse(raw);
    if (parsed && typeof parsed === 'object' && !Array.isArray(parsed)) return parsed;
  } catch { /* fall through */ }
  return {};
};
export const CONFIG_SYNC_URLS: Record<string, string> = _parseUrlMap();

export function configSyncBaseFor(env: string | null | undefined): string {
  if (env && CONFIG_SYNC_URLS[env]) return CONFIG_SYNC_URLS[env];
  return CONFIG_SYNC_BASE;
}
