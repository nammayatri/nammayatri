import React, { useEffect, useRef, useState } from 'react';
import './TopBarActions.css';
import { showAlert, showConfirm } from './Dialogs';
import { NativeAppLauncher } from './NativeAppLauncher';
import { RefPicker } from './RefPicker';
import { loadUiState, saveUiState } from './uiState';

const PROXY_BASE = 'http://localhost:7082';

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

/** Top-bar actions: live next to DB Explorer / Redis Explorer.
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
    (async () => {
      const [ui, envsRes] = await Promise.all([
        loadUiState(),
        fetch(`${PROXY_BASE}/api/config-sync/envs`).then(r => r.ok ? r.json() : { envs: [] }).catch(() => ({ envs: [] })),
      ]);
      if (cancelled) return;
      if (Array.isArray(envsRes.envs)) setSyncEnvs(envsRes.envs);
      const fromUi = typeof ui.syncFrom === 'string' ? ui.syncFrom : null;
      if (fromUi) {
        setSyncFrom(fromUi);
      } else if (envsRes.default) {
        setSyncFrom(envsRes.default);
      }
      const ccUi = typeof ui.ccRef === 'string' ? ui.ccRef : null;
      if (ccUi) setCcRef(ccUi);
    })();
    return () => { cancelled = true; };
  }, []);

  // On mount, fetch current status to detect a sync that was already running
  // (e.g. the auto-sync triggered on test-context-api startup).
  useEffect(() => {
    fetch(`${PROXY_BASE}/api/config-sync/status`)
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
        const r = await fetch(`${PROXY_BASE}/api/config-sync/status`);
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
    fetch(`${PROXY_BASE}/api/control-center/status`)
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
        const r = await fetch(`${PROXY_BASE}/api/control-center/status`);
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
      `Sync data from "${syncFrom}" → local?\n\nRider, driver, and mock-registry services will be restarted.`,
      { title: 'Sync Data', confirmLabel: `Sync from ${syncFrom}`, variant: 'info' },
    );
    if (!ok) return;
    setSyncRunning(true);
    setSyncStatus('running');
    try {
      const r = await fetch(`${PROXY_BASE}/api/config-sync/import`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ from: syncFrom }),
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
      const r = await fetch(`${PROXY_BASE}/api/control-center/start`, {
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
      await fetch(`${PROXY_BASE}/api/control-center/stop`, { method: 'POST' });
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
      const res = await fetch(`${PROXY_BASE}/api/redis/flushall`, { method: 'POST' });
      const data = await res.json();
      setFlushState(data.result === 'ok' ? 'done' : 'error');
    } catch {
      setFlushState('error');
    }
    setTimeout(() => setFlushState('idle'), 3000);
  };

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
                <a className="tb-btn" href="http://localhost:3001" target="_blank" rel="noopener noreferrer" title="Metabase — BI / DB explorer">
                  🗄 DB Explorer
                </a>
                <a className="tb-btn" href="http://localhost:8431" target="_blank" rel="noopener noreferrer" title="redis-commander — standalone + cluster">
                  🧰 Redis Explorer
                </a>
                <button
                  className={`tb-btn tb-flush${flushState === 'done' ? ' tb-ok' : flushState === 'error' ? ' tb-err' : ''}`}
                  onClick={flushRedis}
                  disabled={flushState === 'flushing'}
                  title="Flush all Redis keys"
                >
                  {flushState === 'flushing' ? '🗑 Flushing…' : flushState === 'done' ? '🗑 Flushed' : flushState === 'error' ? '🗑 Error' : '🗑 Flush Redis'}
                </button>
              </div>
            </div>

            {/* Group 2: Data Pipeline */}
            {syncEnvs.length > 0 && (
              <div className="tb-floating-section" title="Data Pipeline">
                <div className="tb-floating-section-label">Data Pipeline</div>
                <div className="tb-floating-section-row">
                  <button
                    className={`tb-btn tb-sync${syncStatus === 'done' ? ' tb-ok' : syncStatus === 'error' ? ' tb-err' : ''}${syncRunning ? ' tb-running' : ''}`}
                    onClick={handleSyncBarClick}
                    title={syncRunning
                      ? `Sync from ${syncFrom} in progress — click to view logs / change source`
                      : `Open Sync Data modal (current source: ${syncFrom}). Imports data from the S3 bundle, applies local-testing-data + feature-migrations, then restarts services.`}
                  >
                    ⚡ {syncRunning ? `Sync · ${syncFrom} · ${elapsed}s` : syncStatus === 'done' ? 'Synced' : syncStatus === 'error' ? 'Sync Error' : `Sync Data`}
                  </button>
                </div>
              </div>
            )}

            {/* Group 3: Client Applications */}
            <div className="tb-floating-section" title="Client Applications">
              <div className="tb-floating-section-label">Client Applications</div>
              <div className="tb-floating-section-row">
                <button
                  className={`tb-btn tb-cc${ccData?.ready ? ' tb-ok' : ccData?.error ? ' tb-err' : ''}${ccData?.running && !ccData.ready ? ' tb-running' : ''}`}
                  onClick={openCcModal}
                  title="Open Control Center launcher (start / stop / open inside)"
                >
                  🚀 Control Center
                  {ccData?.ready && <span className="tb-status-chip is-ready" style={{ marginLeft: 6 }}><span className="tb-status-chip-dot" />ready</span>}
                  {ccData?.running && !ccData.ready && <span className="tb-status-chip is-running" style={{ marginLeft: 6 }}><span className="tb-status-chip-dot" />{ccElapsed}s</span>}
                  {ccData?.error && <span className="tb-status-chip is-error" style={{ marginLeft: 6 }}><span className="tb-status-chip-dot" />error</span>}
                </button>
                {/* Customer + Driver are independent launchers — separate
                    button, modal, Metro, and log buffer each. */}
                <NativeAppLauncher app="customer" label="Customer App" icon="📱" />
                <NativeAppLauncher app="driver"   label="Driver App"   icon="🛺" />
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
    </>
  );
};
