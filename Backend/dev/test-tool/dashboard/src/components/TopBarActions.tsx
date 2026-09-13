import React, { useEffect, useRef, useState } from 'react';
import './TopBarActions.css';
import { showAlert, showConfirm } from './Dialogs';
import { RefPicker } from './RefPicker';
import { loadUiState, saveUiState } from './uiState';
import { PROXY_BASE as CONTEXT_API, LOCAL_API_BASE, getCaddyServiceUrl } from '../config';
import { Terminal as IntegratedTerminal } from './Terminal';
import { ServicePortsModal } from './ServicePortsModal';
import { FinanceViewer } from './FinanceViewer';

interface SyncStatus {
  running: boolean;
  from?: string | null;
  started_at?: number | null;
  finished_at?: number | null;
  exit_code?: number | null;
  error?: string | null;
  log: string[];
}

interface ControlCenterStatus {
  running: boolean;
  ready: boolean;
  url: string;
  started_at?: number | null;
  finished_at?: number | null;
  exit_code?: number | null;
  error?: string | null;
  log: string[];
  pid?: number | null;
}

/** One entry from a direction's metadata.json on S3, written by DB Manager on
 *  every successful publish. `status` is a human judgement someone recorded in
 *  DB Manager's Versions tab after actually importing that bundle — a version
 *  existing says nothing about whether it works. */
interface PublishedVersion {
  version: number;
  metadata: string;
  created_at?: string | null;
  uploaded_by?: string | null;
  status?: 'stable' | 'not_stable' | 'not_verified';
  verified_by?: string | null;
  verified_at?: string | null;
}

// Prefixed onto the <option> text: browsers won't render per-option colour
// reliably, so the signal has to survive as plain characters.
const VERSION_STATUS_MARK: Record<string, string> = {
  stable: '✅ ',      // ✅
  not_stable: '⛔ ',  // ⛔
  not_verified: '',       // no marker — absence of a claim, not a negative one
};

const versionLabel = (v: PublishedVersion): string =>
  `${VERSION_STATUS_MARK[v.status || 'not_verified'] ?? ''}v${v.version}` +
  `${v.metadata ? ` — ${v.metadata}` : ''}`;

/** Top-bar actions: live next to DB Manager / Metabase.
 *  - Flush Redis: wipes all Redis keys.
 *  - Sync Data:  runs config-sync from a chosen env (default: prod) → local.
 *  - While running, the sync button flips to "Check Status" which opens a
 *    modal that polls /api/config-sync/status every 2s and streams logs. */
export const TopBarActions: React.FC = () => {
  const [flushState, setFlushState] = useState<'idle' | 'flushing' | 'done' | 'error'>('idle');
  const [syncEnvs, setSyncEnvs] = useState<string[]>([]);
  const [syncFrom, setSyncFrom] = useState<string>('prod');
  const [syncRunning, setSyncRunning] = useState(false);
  const [syncStatus, setSyncStatus] = useState<'idle' | 'running' | 'done' | 'error'>('idle');
  const [forceFetch, setForceFetch] = useState(false);

  // Published bundles for the selected source env, read from that direction's
  // metadata.json on S3 (which DB Manager writes on every successful publish).
  // Empty selection = don't pass a version, so config_transfer.py falls back to
  // its built-in DEFAULT_FETCH_VERSIONS — i.e. exactly the old behaviour. That
  // matters when S3 is unreachable (VPN off): Sync Data still works, it just
  // can't offer a choice.
  const [versions, setVersions] = useState<PublishedVersion[]>([]);
  const [syncVersion, setSyncVersion] = useState('');
  const [versionsLoading, setVersionsLoading] = useState(false);
  const [versionsError, setVersionsError] = useState<string | null>(null);

  // Status modal
  const [statusOpen, setStatusOpen] = useState(false);
  const [statusData, setStatusData] = useState<SyncStatus | null>(null);
  const logBoxRef = useRef<HTMLPreElement | null>(null);

  // Control-center launcher state
  const [ccData, setCcData] = useState<ControlCenterStatus | null>(null);
  const [ccModalOpen, setCcModalOpen] = useState(false);
  const [ccRef, setCcRef] = useState<string>('');
  const [ccIsStarting, setCcIsStarting] = useState(false);
  const ccLogBoxRef = useRef<HTMLPreElement | null>(null);

  // Integrated terminal modal state.
  const [terminalOpen, setTerminalOpen] = useState(false);
  const [portsOpen, setPortsOpen] = useState(false);

  // Finance visualization modal state.
  const [financeOpen, setFinanceOpen] = useState(false);

  // Floating-panel collapse state. The bar used to live inline in the
  // top bar and crowded out the mode tabs at narrow widths; now it
  // collapses behind a single toggle in the top-right corner.
  const [panelOpen, setPanelOpen] = useState(false);
  const panelRef = useRef<HTMLDivElement | null>(null);

  // Close the panel when the user clicks outside it (but not when they
  // click inside one of the fullscreen modals — those are siblings, not
  // descendants of the panel).
  useEffect(() => {
    if (!panelOpen) return;
    const onDocClick = (ev: MouseEvent) => {
      const target = ev.target as Node | null;
      if (!target) return;
      if (panelRef.current?.contains(target)) return;
      // Don't close if the click landed inside one of our modals.
      const inModal = (target instanceof Element)
        && target.closest('.tb-modal-backdrop') !== null;
      if (inModal) return;
      setPanelOpen(false);
    };
    document.addEventListener('mousedown', onDocClick);
    return () => document.removeEventListener('mousedown', onDocClick);
  }, [panelOpen]);


  // Hydrate dropdown selections (syncFrom, ccRef) from the server-side
  // ui-state bag, then fall back to the envs endpoint's `default` only if
  // ui-state had nothing recorded for syncFrom. Order matters: we want the
  // user's last pick to win over the server's default. Run once on mount.
  useEffect(() => {
    let cancelled = false;
    let retryTimer: number | null = null;

    const fetchEnvs = async (isFirstRun: boolean) => {
      const [ui, envsRes] = await Promise.all([
        isFirstRun ? loadUiState() : Promise.resolve(null),
        fetch(`${CONTEXT_API}/api/config-sync/envs`).then(r => r.ok ? r.json() : { envs: [] }).catch(() => ({ envs: [] })),
      ]);
      if (cancelled) return;
      if (Array.isArray(envsRes.envs) && envsRes.envs.length > 0) {
        setSyncEnvs(envsRes.envs);
        if (isFirstRun && ui) {
          const fromUi = typeof ui.syncFrom === 'string' ? ui.syncFrom : null;
          if (fromUi) {
            setSyncFrom(fromUi);
          } else if (envsRes.default) {
            setSyncFrom(envsRes.default);
          }
          const ccUi = typeof ui.ccRef === 'string' ? ui.ccRef : null;
          if (ccUi) setCcRef(ccUi);
        }
      } else {
        // test-context-api not up yet — retry every 5s until it responds
        if (!cancelled) retryTimer = window.setTimeout(() => fetchEnvs(false), 5000);
      }
    };

    fetchEnvs(true);
    return () => {
      cancelled = true;
      if (retryTimer !== null) window.clearTimeout(retryTimer);
    };
  }, []);

  // Published versions follow the selected source env — each env is a different
  // S3 direction (<from>_to_local), so the list has to be re-fetched on change.
  // A failure here is deliberately non-fatal: the dropdown just stays on
  // "default" and the sync proceeds without an explicit version.
  useEffect(() => {
    if (!syncFrom) return;
    let cancelled = false;
    setVersionsLoading(true);
    setVersionsError(null);
    fetch(`${CONTEXT_API}/api/config-sync/versions?from=${encodeURIComponent(syncFrom)}`)
      .then(r => r.json())
      .then((d: { versions?: PublishedVersion[]; error?: string }) => {
        if (cancelled) return;
        const list = [...(d.versions || [])].sort((a, b) => b.version - a.version);
        setVersions(list);
        setVersionsError(d.error || null);
        // Default to the newest version someone has actually marked stable.
        // The list is newest-first, so the first 'stable' hit IS the latest one.
        //
        // When nothing is marked stable we deliberately leave this blank rather
        // than falling back to the newest bundle: 'not_verified' means nobody
        // has confirmed it imports cleanly, so auto-selecting one would put
        // exactly the unvetted case behind a default nobody looks at. Blank
        // keeps config_transfer.py's own pinned version, and the user can pick
        // consciously.
        //
        // Recomputed per env because the selection is direction-scoped — a
        // version number from one direction means nothing in another.
        const latestStable = list.find(v => v.status === 'stable');
        setSyncVersion(latestStable ? `v${latestStable.version}` : '');
      })
      .catch(e => {
        if (cancelled) return;
        setVersions([]);
        setVersionsError(e instanceof Error ? e.message : String(e));
      })
      .finally(() => { if (!cancelled) setVersionsLoading(false); });
    return () => { cancelled = true; };
  }, [syncFrom]);

  // On mount, fetch current status to detect a sync that was already running
  // (e.g. the auto-sync triggered on test-context-api startup).
  useEffect(() => {
    fetch(`${CONTEXT_API}/api/config-sync/status`)
      .then(r => r.json())
      .then((d: SyncStatus) => {
        setStatusData(d);
        if (d.running) { setSyncRunning(true); setSyncStatus('running'); }
      })
      .catch(() => { });
  }, []);

  // Poll while sync is running OR while the status modal is open.
  useEffect(() => {
    if (!syncRunning && !statusOpen) return;
    const t = setInterval(async () => {
      try {
        const r = await fetch(`${CONTEXT_API}/api/config-sync/status`);
        const d: SyncStatus = await r.json();
        setStatusData(d);
        if (!d.running && syncRunning) {
          setSyncRunning(false);
          const failed = !!d.error || (typeof d.exit_code === 'number' && d.exit_code !== 0);
          setSyncStatus(failed ? 'error' : 'done');
          setTimeout(() => setSyncStatus('idle'), 4000);
        }
      } catch { /* keep polling */ }
    }, 2000);
    return () => clearInterval(t);
  }, [syncRunning, statusOpen]);

  // Auto-scroll the log box to the bottom on update.
  useEffect(() => {
    if (statusOpen && logBoxRef.current) {
      logBoxRef.current.scrollTop = logBoxRef.current.scrollHeight;
    }
  }, [statusData?.log, statusOpen]);

  // Initial fetch of control-center status to recover state across page reloads.
  useEffect(() => {
    fetch(`${LOCAL_API_BASE}/api/control-center/status`)
      .then(r => r.json())
      .then((d: ControlCenterStatus) => setCcData(d))
      .catch(() => { });
  }, []);

  // Poll control-center status while it's running OR while its modal is open.
  useEffect(() => {
    const isPolling = (ccData?.running || ccModalOpen) && !ccData?.ready;
    if (!isPolling && !ccModalOpen) return;
    const t = setInterval(async () => {
      try {
        const r = await fetch(`${LOCAL_API_BASE}/api/control-center/status`);
        const d: ControlCenterStatus = await r.json();
        setCcData(d);
      } catch { /* keep polling */ }
    }, 2000);
    return () => clearInterval(t);
  }, [ccData?.running, ccData?.ready, ccModalOpen]);

  // Auto-scroll cc log box.
  useEffect(() => {
    if (ccModalOpen && ccLogBoxRef.current) {
      ccLogBoxRef.current.scrollTop = ccLogBoxRef.current.scrollHeight;
    }
  }, [ccData?.log, ccModalOpen]);


  // Top-bar "Sync from <env>" click — open the modal so the user can change
  // the source env and confirm there. Avoids surprise data wipes from a
  // single accidental click on the bar.
  const handleSyncBarClick = () => {
    setStatusOpen(true);
  };

  const triggerConfigSync = async () => {
    if (syncRunning) { setStatusOpen(true); return; }
    const ok = await showConfirm(
      `Sync data from "${syncFrom}"${syncVersion ? ` (${syncVersion})` : ''} → local?` +
      `${syncVersion ? `\n\nImporting published bundle ${syncVersion}; the local patched copy will be re-downloaded.` : ''}` +
      `${!syncVersion && forceFetch ? '\n\nForce-fetch is ON — local patched data will be discarded and re-downloaded from S3.' : ''}` +
      `\n\nRider, driver, and mock-registry services will be restarted.`,
      { title: 'Sync Data', confirmLabel: `Sync from ${syncFrom}`, variant: 'info' },
    );
    if (!ok) return;
    setSyncRunning(true);
    setSyncStatus('running');
    try {
      const r = await fetch(`${CONTEXT_API}/api/config-sync/import`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ from: syncFrom, forceFetch, version: syncVersion || undefined }),
      });
      if (!r.ok) {
        setSyncRunning(false);
        setSyncStatus('error');
        setTimeout(() => setSyncStatus('idle'), 4000);
      } else {
        setStatusOpen(true); // open the log modal right away
      }
    } catch {
      setSyncRunning(false);
      setSyncStatus('error');
      setTimeout(() => setSyncStatus('idle'), 4000);
    }
  };

  // Top-bar buttons just open their respective modals — every action
  // (Start / Stop / Re-Launch / Open / Retry) lives inside the modal so
  // the bar stays a status surface, not a control surface.
  const openCcModal = () => setCcModalOpen(true);

  const handleControlCenterStart = async () => {
    if (ccIsStarting) return;
    setCcIsStarting(true);
    try {
      const r = await fetch(`${LOCAL_API_BASE}/api/control-center/start`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ ref: ccRef.trim() || undefined }),
      });
      if (r.ok) {
        setCcData(prev => ({ ...(prev ?? { url: '', log: [], ready: false }), running: true, ready: false } as ControlCenterStatus));
      } else {
        const err = await r.json().catch(() => ({}));
        if (!err?.error?.includes('already running')) {
          showAlert(`${err?.error ?? `HTTP ${r.status}`}`, { title: 'Could not start Control Center', variant: 'danger' });
        }
      }
    } catch (e) {
      showAlert(String(e), { title: 'Could not start Control Center', variant: 'danger' });
    } finally {
      setCcIsStarting(false);
    }
  };

  const handleControlCenterOpen = () => {
    if (ccData?.url) window.open(ccData.url, '_blank', 'noopener,noreferrer');
  };

  const handleControlCenterStop = async () => {
    const ok = await showConfirm(
      'Stop the running control-center launcher?\n\nVite will be killed.',
      { title: 'Stop Control Center', confirmLabel: 'Stop', variant: 'danger' },
    );
    if (!ok) return;
    try {
      await fetch(`${LOCAL_API_BASE}/api/control-center/stop`, { method: 'POST' });
      setCcData(prev => prev ? { ...prev, running: false, ready: false } : prev);
    } catch (e) {
      showAlert(String(e), { title: 'Could not stop Control Center', variant: 'danger' });
    }
  };

  const flushRedis = async () => {
    if (flushState === 'flushing') return;
    const ok = await showConfirm(
      'Flush ALL Redis keys?\n\nThis cannot be undone — every key in both the standalone and cluster Redis instances will be deleted.',
      { title: 'Flush Redis', confirmLabel: 'Flush all keys', variant: 'danger' },
    );
    if (!ok) return;
    setFlushState('flushing');
    try {
      const res = await fetch(`${CONTEXT_API}/api/redis/flushall`, { method: 'POST' });
      const data = await res.json();
      setFlushState(data.result === 'ok' ? 'done' : 'error');
    } catch {
      setFlushState('error');
    }
    setTimeout(() => setFlushState('idle'), 3000);
  };

  const dbManagerUrl = getCaddyServiceUrl('db-manager-frontend');
  const metabaseUrl = getCaddyServiceUrl('metabase');

  const syncBtnLabel = syncRunning
    ? 'Check Status'
    : syncStatus === 'done' ? 'Synced'
      : syncStatus === 'error' ? 'Error'
        : 'Sync Data';

  const ccBtnLabel = ccData?.ready
    ? 'Open Control Center'
    : ccData?.running ? 'Check Status'
      : ccData?.error ? 'Retry Launch'
        : 'Launch Control Center';
  const ccElapsed = ccData?.started_at
    ? Math.floor(((ccData.finished_at ?? Date.now() / 1000) - ccData.started_at))
    : 0;

  const elapsed = statusData?.started_at
    ? Math.floor(((statusData.finished_at ?? Date.now() / 1000) - statusData.started_at))
    : 0;

  // Aggregate "something is happening" for the toggle dot.
  const anyRunning = syncRunning || (ccData?.running && !ccData.ready);
  const anyError = syncStatus === 'error' || flushState === 'error' || !!ccData?.error;
  const anyReady = !!ccData?.ready;
  const toggleStatus: 'running' | 'error' | 'ready' | 'idle' =
    anyRunning ? 'running' : anyError ? 'error' : anyReady ? 'ready' : 'idle';

  return (
    <>
      <div className="tb-floating" ref={panelRef}>
        <button
          className={`tb-floating-toggle is-${toggleStatus}${panelOpen ? ' is-open' : ''}`}
          onClick={() => setPanelOpen(o => !o)}
          title={
            anyRunning ? 'Tools — something is running' :
            anyError ? 'Tools — last action errored' :
            anyReady ? 'Tools — control center / app ready' :
            'Tools — DB / Redis / Sync / Launchers'
          }
          aria-expanded={panelOpen}
          aria-label="Tools menu"
        >
          <span className="tb-floating-toggle-icon">🛠</span>
          <span className="tb-floating-toggle-label">Tools</span>
          {toggleStatus !== 'idle' && (
            <span className={`tb-floating-toggle-dot is-${toggleStatus}`} />
          )}
          <span className="tb-floating-toggle-caret">{panelOpen ? '▴' : '▾'}</span>
        </button>

        {panelOpen && (
          <div className="tb-floating-panel" role="menu">
            {/* Group 1: Systems */}
            <div className="tb-floating-section" title="Systems">
              <div className="tb-floating-section-label">Systems</div>
              <div className="tb-floating-section-row">
                <a className="tb-btn" href={dbManagerUrl} target="_blank" rel="noopener noreferrer" title={`DB Manager — browse / edit the rider (atlas_app) and driver (atlas_driver_offer_bpp) schemas (${dbManagerUrl})`}>
                  🧭 DB Manager
                </a>
                <a className="tb-btn" href={metabaseUrl} target="_blank" rel="noopener noreferrer" title={`Metabase — BI / analytics over the stack's DB (${metabaseUrl})`}>
                  🗄 Metabase
                </a>
                <button
                  className={`tb-btn tb-flush${flushState === 'done' ? ' tb-ok' : flushState === 'error' ? ' tb-err' : ''}`}
                  onClick={flushRedis}
                  disabled={flushState === 'flushing'}
                  title="Flush all Redis keys"
                >
                  {flushState === 'flushing' ? '🗑 Flushing…' : flushState === 'done' ? '🗑 Flushed' : flushState === 'error' ? '🗑 Error' : '🗑 Flush Redis'}
                </button>
                <button
                  className="tb-btn"
                  onClick={() => setTerminalOpen(true)}
                  title="Open an integrated PTY-backed shell running on the test-context-api host"
                >
                  💻 Terminal
                </button>
                <button
                  className="tb-btn"
                  onClick={() => { setPanelOpen(false); setPortsOpen(true); }}
                  title="Every resolved service port for the stack in use: direct host:port URLs and the Caddy host:caddyPort/<service> routes"
                >
                  🔌 Service Ports
                </button>
              </div>
            </div>

            {/* Group 2: Data Pipeline — always visible. Hidden-until-ready made
                the button look missing while test-context-api (7082) was down. */}
            <div className="tb-floating-section" title="Data Pipeline">
              <div className="tb-floating-section-label">Data Pipeline</div>
              <div className="tb-floating-section-row">
                <button
                  className={`tb-btn tb-sync tb-sync-span${syncEnvs.length === 0 ? ' tb-waiting' : ''}${syncStatus === 'done' ? ' tb-ok' : syncStatus === 'error' ? ' tb-err' : ''}${syncRunning ? ' tb-running' : ''}`}
                  onClick={handleSyncBarClick}
                  disabled={syncEnvs.length === 0}
                  title={syncEnvs.length === 0
                    ? `Waiting for test-context-api at ${CONTEXT_API} (GET /api/config-sync/envs). Retrying every 5s.`
                    : syncRunning
                      ? `Sync from ${syncFrom} in progress — click to view logs / change source`
                      : `Open Sync Data modal (current source: ${syncFrom}). Imports data from the S3 bundle, applies local-testing-data + feature-migrations, then restarts services.`}
                >
                  ⚡ {syncEnvs.length === 0
                    ? 'Sync Data · waiting'
                    : syncRunning ? `Sync · ${syncFrom} · ${elapsed}s` : syncStatus === 'done' ? 'Synced' : syncStatus === 'error' ? 'Sync Error' : `Sync Data`}
                </button>
              </div>
              {syncEnvs.length === 0 && (
                <div className="tb-floating-hint">
                  Waiting for <code>test-context-api</code> at <code>{CONTEXT_API}</code>. Retrying every 5s.
                </div>
              )}
            </div>

            {/* Group 3: Visualizations */}
            <div className="tb-floating-section" title="Visualizations">
              <div className="tb-floating-section-label">Visualizations</div>
              <div className="tb-floating-section-row">
                <button
                  className="tb-btn"
                  onClick={() => setFinanceOpen(true)}
                  title="Open Finance Visualization dashboard"
                >
                  📊 Finance
                </button>
              </div>
            </div>
          </div>
        )}
      </div>

      {ccModalOpen && (
        <div className="tb-modal-backdrop" onClick={() => setCcModalOpen(false)}>
          <div className="tb-modal" onClick={e => e.stopPropagation()}>
            <div className="tb-modal-header">
              <span className="tb-modal-title">
                Control Center
                <span className={`tb-pill ${ccData?.ready ? 'tb-pill-ok' : ccData?.error ? 'tb-pill-err' : ccData?.running ? 'tb-pill-run' : ''}`}>
                  {ccData?.ready ? `ready · ${ccElapsed}s`
                    : ccData?.running ? `starting · ${ccElapsed}s`
                      : ccData?.error ? 'error'
                        : ccData?.exit_code != null ? `exit ${ccData.exit_code}`
                          : 'idle'}
                </span>
              </span>
              <button className="tb-modal-close" onClick={() => setCcModalOpen(false)} title="Close">✕</button>
            </div>
            {/* Action bar: same pattern as the native-app form. RefPicker
                lets the user override the branch / commit before launch.
                Buttons swap based on running/ready/error state — exactly
                one primary action visible at a time. */}
            <div className="tb-modal-form">
              <div className="tb-modal-form-field">
                <RefPicker
                  repo="nammayatri/control-center"
                  value={ccRef}
                  onChange={(v) => { setCcRef(v); saveUiState({ ccRef: v || null }); }}
                  disabled={ccData?.running}
                />
              </div>
              <div className="tb-modal-form-actions">
                {!ccData?.running && (
                  <button
                    className={`tb-btn tb-modal-form-primary${ccIsStarting ? ' tb-busy' : ''}`}
                    onClick={handleControlCenterStart}
                    disabled={ccIsStarting}
                    title="Clone-or-pull nammayatri/control-center, optionally checkout the chosen ref, npm install, run vite dev (VITE_BAP_URL=http://localhost:8017, VITE_BPP_URL=http://localhost:8018)"
                  >
                    {ccIsStarting ? (
                      <>
                        <span className="tb-spinner" aria-hidden="true" />
                        {' '}Starting…
                      </>
                    ) : (
                      <>▶ {ccData?.ready ? 'Re-Launch' : ccData?.error ? 'Retry' : 'Start'}</>
                    )}
                  </button>
                )}
                {ccData?.running && (
                  <button
                    className="tb-btn tb-err"
                    onClick={handleControlCenterStop}
                    title="Kill the control-center launcher (vite + npm)"
                  >
                    ⏹ Stop
                  </button>
                )}
                {ccData?.ready && (
                  <button
                    className="tb-btn tb-ok"
                    onClick={handleControlCenterOpen}
                    title={`Open ${ccData.url} in a new tab`}
                  >
                    ↗ Open
                  </button>
                )}
              </div>
            </div>
            {ccData?.error && (
              <div className="tb-modal-error">⚠ {ccData.error}</div>
            )}
            <pre ref={ccLogBoxRef} className="tb-modal-log">
              {ccData?.log?.length ? ccData.log.join('\n') : '(no log lines yet — click Start)'}
            </pre>
            <div className="tb-modal-footer">
              <span className="tb-modal-meta">
                {ccData?.log?.length ?? 0} lines · polling every 2s
                {ccData?.ready && ccData.url ? ` · serving ${ccData.url}` : ''}
              </span>
              <button
                className="tb-btn"
                onClick={() => {
                  if (!ccData?.log) return;
                  navigator.clipboard?.writeText(ccData.log.join('\n')).catch(() => { });
                }}
                title="Copy logs to clipboard"
              >
                📋 Copy
              </button>
            </div>
          </div>
        </div>
      )}

      {statusOpen && (
        <div className="tb-modal-backdrop" onClick={() => setStatusOpen(false)}>
          <div className="tb-modal" onClick={e => e.stopPropagation()}>
            <div className="tb-modal-header">
              <span className="tb-modal-title">
                Config Sync — {statusData?.from ?? syncFrom}
                <span className={`tb-pill ${statusData?.running ? 'tb-pill-run' : statusData?.error || statusData?.exit_code ? 'tb-pill-err' : 'tb-pill-ok'}`}>
                  {statusData?.running ? `running · ${elapsed}s`
                    : statusData?.error ? 'error'
                      : statusData?.exit_code ? `exit ${statusData.exit_code}`
                        : statusData ? `done · ${elapsed}s` : 'idle'}
                </span>
              </span>
              <button className="tb-modal-close" onClick={() => setStatusOpen(false)} title="Close">✕</button>
            </div>
            {syncEnvs.length > 0 && (
              <div className="tb-modal-form">
                <div className="tb-modal-form-field">
                  <label htmlFor="sync-from">Source environment</label>
                  <select
                    id="sync-from"
                    value={syncFrom}
                    onChange={e => { setSyncFrom(e.target.value); saveUiState({ syncFrom: e.target.value }); }}
                    disabled={syncRunning}
                    title="S3 bundle to import data from"
                  >
                    {syncEnvs.map(env => (<option key={env} value={env}>{env}</option>))}
                  </select>
                </div>
                <div className="tb-modal-form-field">
                  <label htmlFor="sync-version">Version</label>
                  <select
                    id="sync-version"
                    value={syncVersion}
                    onChange={e => setSyncVersion(e.target.value)}
                    disabled={syncRunning || versionsLoading}
                    title="Published bundle to import. Blank uses whichever version config_transfer.py defaults to."
                  >
                    <option value="">
                      {versionsLoading
                        ? 'loading…'
                        : versions.length
                          ? 'default (config_transfer.py pinned version)'
                          : 'no published versions'}
                    </option>
                    {versions.map(v => (
                      <option key={v.version} value={`v${v.version}`}>
                        {versionLabel(v)}
                      </option>
                    ))}
                  </select>
                  {(() => {
                    // Detail line for whatever is currently selected. The
                    // dropdown can only carry a marker character, so the
                    // "who vouched for this, and when" lives here.
                    const sel = versions.find(v => `v${v.version}` === syncVersion);
                    if (!sel) return null;
                    const status = sel.status || 'not_verified';
                    const when = sel.verified_at
                      ? new Date(sel.verified_at).toLocaleDateString()
                      : null;
                    const text =
                      status === 'stable'
                        ? `Marked stable${sel.verified_by ? ` by ${sel.verified_by}` : ''}${when ? ` · ${when}` : ''}`
                        : status === 'not_stable'
                          ? `Marked NOT stable${sel.verified_by ? ` by ${sel.verified_by}` : ''}${when ? ` · ${when}` : ''} — avoid unless you know why`
                          : 'Not verified yet — nobody has confirmed this bundle imports cleanly';
                    return (
                      <div className="tb-modal-form-hint">
                        {text}
                        {sel.uploaded_by ? ` · published by ${sel.uploaded_by}` : ''}
                      </div>
                    );
                  })()}
                  {versionsError && (
                    <div className="tb-modal-form-hint">⚠ {versionsError}</div>
                  )}
                </div>
                <div className="tb-modal-form-field">
                  {/* Same label-on-top shape as the two selects, so all three
                      fields line up: heading row, control row, hint row. */}
                  <label htmlFor="sync-force-fetch">Force fetch</label>
                  <label className="tb-modal-checkbox" htmlFor="sync-force-fetch">
                    <input
                      id="sync-force-fetch"
                      type="checkbox"
                      checked={forceFetch}
                      onChange={e => setForceFetch(e.target.checked)}
                      disabled={syncRunning}
                    />
                    <span>Re-download even if cached</span>
                  </label>
                  <div className="tb-modal-form-hint">
                    Ignored when a version is picked — that always re-downloads.
                  </div>
                </div>
                <div className="tb-modal-form-actions">
                  <button
                    className="tb-btn tb-modal-form-primary"
                    onClick={triggerConfigSync}
                    disabled={syncRunning}
                    title={syncRunning ? 'A sync is in progress' : `Import data from ${syncFrom} → local`}
                  >
                    {syncRunning ? `Syncing · ${elapsed}s` : 'Sync Data'}
                  </button>
                </div>
              </div>
            )}
            {statusData?.error && (
              <div className="tb-modal-error">⚠ {statusData.error}</div>
            )}
            <pre ref={logBoxRef} className="tb-modal-log">
              {statusData?.log?.length ? statusData.log.join('\n') : '(no log lines yet — pick a source above and click Sync Data)'}
            </pre>
            <div className="tb-modal-footer">
              <span className="tb-modal-meta">
                {statusData?.log?.length ?? 0} lines · polling every 2s
              </span>
              <button
                className="tb-btn"
                onClick={() => {
                  if (!statusData?.log) return;
                  navigator.clipboard?.writeText(statusData.log.join('\n')).catch(() => { });
                }}
                title="Copy logs to clipboard"
              >
                📋 Copy
              </button>
            </div>
          </div>
        </div>
      )}
      {financeOpen && (
        <div className="tb-modal-backdrop" onClick={() => setFinanceOpen(false)}>
          <div className="tb-modal tb-modal-terminal" onClick={e => e.stopPropagation()}>
            <div className="tb-modal-header">
              <span className="tb-modal-title">📊 Finance Visualization</span>
              <button className="tb-modal-close" onClick={() => setFinanceOpen(false)} title="Close">✕</button>
            </div>
            <div className="tb-modal-body" style={{ overflow: 'auto' }}>
              <FinanceViewer />
            </div>
          </div>
        </div>
      )}
      {portsOpen && <ServicePortsModal onClose={() => setPortsOpen(false)} />}

      {terminalOpen && (
        <div className="tb-modal-backdrop" onClick={() => setTerminalOpen(false)}>
          <div className="tb-modal tb-modal-terminal" onClick={e => e.stopPropagation()}>
            <div className="tb-modal-header">
              <span className="tb-modal-title">
                💻 Terminal
                <span className="tb-pill tb-pill-run">PTY · bash</span>
              </span>
              <button className="tb-modal-close" onClick={() => setTerminalOpen(false)} title="Close terminal (kills the shell)">✕</button>
            </div>
            <div className="tb-modal-body">
              <IntegratedTerminal onClose={() => setTerminalOpen(false)} />
            </div>
          </div>
        </div>
      )}
    </>
  );
};
