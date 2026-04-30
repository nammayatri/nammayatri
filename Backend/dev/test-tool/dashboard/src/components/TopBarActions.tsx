import React, { useEffect, useRef, useState } from 'react';
import './TopBarActions.css';

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

  useEffect(() => {
    fetch(`${PROXY_BASE}/api/config-sync/envs`)
      .then(r => r.json())
      .then(d => {
        if (Array.isArray(d.envs)) setSyncEnvs(d.envs);
        if (d.default) setSyncFrom(d.default);
      })
      .catch(() => { });
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

  const triggerConfigSync = async () => {
    if (syncRunning) { setStatusOpen(true); return; }
    if (!window.confirm(`Sync data from ${syncFrom} → local? Rider/driver/mock-registry will be restarted.`)) return;
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

  const flushRedis = async () => {
    if (flushState === 'flushing') return;
    if (!window.confirm('Flush ALL Redis keys? This cannot be undone.')) return;
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

  const elapsed = statusData?.started_at
    ? Math.floor(((statusData.finished_at ?? Date.now() / 1000) - statusData.started_at))
    : 0;

  return (
    <>
      <button
        className={`tb-btn tb-flush${flushState === 'done' ? ' tb-ok' : flushState === 'error' ? ' tb-err' : ''}`}
        onClick={flushRedis}
        disabled={flushState === 'flushing'}
        title="Flush all Redis keys"
      >
        {flushState === 'flushing' ? 'Flushing…' : flushState === 'done' ? 'Flushed' : flushState === 'error' ? 'Error' : 'Flush Redis'}
      </button>
      {syncEnvs.length > 0 && (
        <>
          <select
            className="tb-select"
            value={syncFrom}
            onChange={e => setSyncFrom(e.target.value)}
            disabled={syncRunning}
            title="Source environment to sync data from"
          >
            {syncEnvs.map(env => (<option key={env} value={env}>{env}</option>))}
          </select>
          <button
            className={`tb-btn tb-sync${syncStatus === 'done' ? ' tb-ok' : syncStatus === 'error' ? ' tb-err' : ''}${syncRunning ? ' tb-running' : ''}`}
            onClick={triggerConfigSync}
            title={syncRunning
              ? 'A sync is in progress — click to view live logs'
              : `Import data from ${syncFrom} S3 bundle, apply local-testing-data + feature-migrations, then restart services`}
          >
            {syncBtnLabel}
          </button>
        </>
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
            {statusData?.error && (
              <div className="tb-modal-error">⚠ {statusData.error}</div>
            )}
            <pre ref={logBoxRef} className="tb-modal-log">
              {statusData?.log?.length ? statusData.log.join('\n') : '(no log lines yet)'}
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
