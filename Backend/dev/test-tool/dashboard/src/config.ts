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
const _proxyDefault = process.env.REACT_APP_PROXY_BASE || 'http://localhost:7082';
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
export const LOCAL_API_BASE = process.env.REACT_APP_LOCAL_API_BASE || 'http://localhost:7083';
export const MOCK_SERVER_URL = process.env.REACT_APP_MOCK_SERVER_URL || 'http://localhost:8080';
export const RIDER_URL = process.env.REACT_APP_RIDER_URL || 'http://localhost:8013';
export const DRIVER_URL = process.env.REACT_APP_DRIVER_URL || 'http://localhost:8016';
export const PROVIDER_DASHBOARD_URL = process.env.REACT_APP_PROVIDER_DASHBOARD_URL || 'http://localhost:8018';
// Default URL when no per-env entry exists.
export const CONFIG_SYNC_BASE = process.env.REACT_APP_CONFIG_SYNC_BASE || 'http://localhost:8090';

// Per-source-env URL map. Same config-sync server can live in different
// k8s clusters depending on which `--from` env it talks to (each env's
// cluster runs its own copy so it has direct DB access). Override at build
// time via REACT_APP_CONFIG_SYNC_URLS (JSON object), e.g.:
//
//   REACT_APP_CONFIG_SYNC_URLS='{"master":"https://config-sync.master.internal",
//                                "prod":"https://config-sync.prod.internal"}'
//
// Anything missing falls back to CONFIG_SYNC_BASE.
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
