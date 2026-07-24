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

// Point the dashboard at whichever stack this checkout targets, without anyone
// clicking anything: local-api resolves {host, contextApiPort} from the stack's
// data/devbox-ports.json (over SSH for a devbox). PROXY_BASE is a module-level
// binding read synchronously all over the app, so a change means persisting the
// new base and reloading once — the guard below keeps that to a single reload.
const CTX_SYNC_GUARD = 'ny.contextApiBase.syncedTo';

export async function syncContextApiBase(): Promise<void> {
  let resolved: string | null = null;
  try {
    const resp = await fetch(`${LOCAL_API_BASE}/api/devbox/ports`, { cache: 'no-store' });
    if (!resp.ok) return;
    const body = await resp.json();
    const host = body?.host || 'localhost';
    const port = body?.contextApiPort;
    if (!port) return; // stack not up — keep whatever we have
    resolved = `http://${host}:${port}`;
  } catch {
    return; // no local-api (deployed dashboard) — keep the build-time default
  }
  if (resolved === PROXY_BASE) return;
  try {
    // Don't fight a reload loop if something keeps flapping.
    if (window.sessionStorage?.getItem(CTX_SYNC_GUARD) === resolved) return;
    window.sessionStorage?.setItem(CTX_SYNC_GUARD, resolved);
  } catch { /* ignore */ }
  setContextApiBase(resolved); // persists + reloads
}

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

// ── Stack host ──
// Same source as the Service Ports modal (GET /api/devbox/ports): the machine
// the stack actually runs on. "localhost" for a local run, the dev-box IP
// otherwise — a link built with localhost is dead in the dev-box case.
const HOST_CACHE_KEY = `ny.stackHost::${PROXY_BASE}`;

let _stackHost = 'localhost';
let _caddyPort: number | null = null;
(() => {
  try {
    const raw = window.localStorage?.getItem(HOST_CACHE_KEY);
    if (raw) {
      const cached = JSON.parse(raw);
      if (cached?.host) _stackHost = cached.host;
      if (typeof cached?.caddyPort === 'number') _caddyPort = cached.caddyPort;
    }
  } catch { /* ignore */ }
})();

export function getStackHost(): string {
  return _stackHost;
}

/** Direct http://<stack-host>:<port> — for services that bind 0.0.0.0. */
export function getStackServiceUrl(name: string, pathSuffix = ''): string {
  return `http://${_stackHost}:${getServicePort(name)}${pathSuffix}`;
}

/**
 * http://<stack-host>:<caddyPort>/<name>/ — required by services that bind
 * 127.0.0.1 or resolve their own API against window.location.origin
 * (db-manager-frontend does both). Null when no caddy port is known.
 */
export function getCaddyServiceUrl(name: string): string | null {
  if (_caddyPort == null) return null;
  return `http://${_stackHost}:${_caddyPort}/${name}/`;
}

// Async refresh. Invoked once on app boot (see App.tsx). Subsequent renders
// of consumer components will see the resolved table.
// Preference order:
//   1. local-api GET /api/devbox/ports — the single accessor, which reads
//      .devbox-ports.json off the stack host (over SSH when it's a devbox).
//   2. context-api GET /api/ports — for a deployed dashboard with no local-api.
//   3. DEFAULT_PORTS / the localStorage cache.
export async function refreshPortsTable(): Promise<void> {
  const apply = (ports: Record<string, number>): boolean => {
    if (!ports || typeof ports !== 'object' || Object.keys(ports).length === 0) return false;
    _portsTable = { ...DEFAULT_PORTS, ...ports };
    try { window.localStorage?.setItem(PORTS_CACHE_KEY, JSON.stringify(_portsTable)); } catch { /* ignore */ }
    return true;
  };
  const applyHost = (host?: string, caddyPort?: number | null): void => {
    if (host) _stackHost = host;
    if (typeof caddyPort === 'number') _caddyPort = caddyPort;
    try {
      window.localStorage?.setItem(
        HOST_CACHE_KEY, JSON.stringify({ host: _stackHost, caddyPort: _caddyPort }),
      );
    } catch { /* ignore */ }
  };
  try {
    const resp = await fetch(`${LOCAL_API_BASE}/api/devbox/ports`, { cache: 'no-store' });
    if (resp.ok) {
      const data = await resp.json();
      applyHost(data?.host, data?.caddyPort);
      if (apply(data?.ports)) return;
    }
  } catch {
    /* local-api not running — fall through to context-api */
  }
  try {
    const resp = await fetch(`${PROXY_BASE}/api/ports`, { cache: 'no-store' });
    if (!resp.ok) return;
    apply((await resp.json())?.ports);
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
