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
