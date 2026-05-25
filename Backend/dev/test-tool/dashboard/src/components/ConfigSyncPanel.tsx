import React, { useEffect, useRef, useState } from 'react';
import { configSyncBaseFor } from '../config';
import './ConfigSyncPanel.css';

type Phase = 'idle' | 'export' | 'patch' | 'done' | 'error' | 'cancelled' | 'skipped';

interface SyncMarker {
  from?: string;
  to?: string;
  s3_prefix?: string;
  s3_bucket?: string;
  synced_at?: number;
  task_id?: string;
}

interface TaskView {
  id: string;
  cmd: string;
  status: 'running' | 'succeeded' | 'failed' | 'cancelled';
  started_at: number | null;
  finished_at: number | null;
  returncode: number | null;
  log: string[];
}

// to=local is intentionally hardcoded: this panel only seeds the dev box.
// If we ever need to sync into staging from here, lift it into a select.
const TO_ENV = 'local';

const FROM_ENVS = ['prod', 'prod_international', 'master', 'env'];

const formatDur = (start: number | null, end: number | null): string => {
  if (!start) return '';
  const t = (end ?? Date.now() / 1000) - start;
  if (t < 60) return `${t.toFixed(1)}s`;
  const m = Math.floor(t / 60);
  const s = Math.floor(t % 60);
  return `${m}m${s.toString().padStart(2, '0')}s`;
};

export const ConfigSyncPanel: React.FC = () => {
  const [fromEnv, setFromEnv] = useState<string>(() =>
    localStorage.getItem('configsync.from') || 'prod_international');
  const [versionPrefix, setVersionPrefix] = useState<string>(() =>
    localStorage.getItem('configsync.versionPrefix') || 'v1');
  const [s3Bucket, setS3Bucket] = useState<string>(() =>
    localStorage.getItem('configsync.s3Bucket') || 'backend-ny-config-sync');
  const [parallel, setParallel] = useState<number>(() =>
    Number(localStorage.getItem('configsync.parallel') || 10));

  const [phase, setPhase] = useState<Phase>('idle');
  const [exportTask, setExportTask] = useState<TaskView | null>(null);
  const [patchTask, setPatchTask] = useState<TaskView | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [skipMessage, setSkipMessage] = useState<string | null>(null);
  const logRef = useRef<HTMLPreElement | null>(null);
  const cancelledRef = useRef(false);
  // Track the currently-running task id so the Stop button can call
  // /tasks/<id>/stop on the right one even mid-phase-transition.
  const liveTaskIdRef = useRef<string | null>(null);
  const [stopping, setStopping] = useState(false);

  useEffect(() => { localStorage.setItem('configsync.from', fromEnv); }, [fromEnv]);
  useEffect(() => { localStorage.setItem('configsync.versionPrefix', versionPrefix); }, [versionPrefix]);
  useEffect(() => { localStorage.setItem('configsync.s3Bucket', s3Bucket); }, [s3Bucket]);
  useEffect(() => { localStorage.setItem('configsync.parallel', String(parallel)); }, [parallel]);

  useEffect(() => {
    if (logRef.current) logRef.current.scrollTop = logRef.current.scrollHeight;
  }, [exportTask?.log, patchTask?.log]);

  const s3Prefix = `${fromEnv}_to_${TO_ENV}/${versionPrefix}`;
  // Each --from env may live in its own cluster, so the config-sync server
  // address is keyed by the source env (with localhost:8090 as fallback).
  const configSyncBase = configSyncBaseFor(fromEnv);

  const startTask = async (path: string, body: Record<string, unknown>): Promise<string> => {
    const r = await fetch(`${configSyncBase}${path}`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(body),
    });
    if (!r.ok) {
      const detail = await r.text();
      throw new Error(`POST ${path} failed: ${r.status} ${detail}`);
    }
    const j = await r.json();
    return j.task_id as string;
  };

  const pollTask = async (id: string, setter: (t: TaskView) => void): Promise<TaskView> => {
    while (true) {
      if (cancelledRef.current) throw new Error('cancelled');
      const r = await fetch(`${configSyncBase}/tasks/${id}`);
      if (!r.ok) throw new Error(`GET /tasks/${id} failed: ${r.status}`);
      const t: TaskView = await r.json();
      setter(t);
      if (t.status !== 'running') return t;
      await new Promise(res => setTimeout(res, 1500));
    }
  };

  const run = async () => {
    cancelledRef.current = false;
    setError(null);
    setSkipMessage(null);
    setExportTask(null);
    setPatchTask(null);
    liveTaskIdRef.current = null;
    try {
      try {
        const r = await fetch(`${configSyncBase}/sync-marker`);
        if (r.ok) {
          const j: { marker: SyncMarker | null } = await r.json();
          if (j.marker && j.marker.from === fromEnv) {
            const when = j.marker.synced_at
              ? new Date(j.marker.synced_at * 1000).toLocaleString()
              : 'previously';
            setSkipMessage(`Already synced from ${fromEnv} (${when}). Delete <repo-root>/data/config-sync/metadata.json to force re-sync.`);
            setPhase('skipped');
            return;
          }
        }
      } catch {
        // marker fetch is best-effort — fall through and run the sync.
      }

      setPhase('export');
      const exportId = await startTask('/export', { from: fromEnv, parallel });
      liveTaskIdRef.current = exportId;
      const exp = await pollTask(exportId, setExportTask);
      liveTaskIdRef.current = null;
      if (exp.status === 'cancelled') { setPhase('cancelled'); return; }
      if (exp.status !== 'succeeded') {
        setPhase('error');
        setError(`export failed (rc=${exp.returncode})`);
        return;
      }

      setPhase('patch');
      const patchId = await startTask('/patch', {
        from: fromEnv,
        to: TO_ENV,
        s3: true,
        s3_bucket: s3Bucket,
        s3_prefix: s3Prefix,
      });
      liveTaskIdRef.current = patchId;
      const pat = await pollTask(patchId, setPatchTask);
      liveTaskIdRef.current = null;
      if (pat.status === 'cancelled') { setPhase('cancelled'); return; }
      if (pat.status !== 'succeeded') {
        setPhase('error');
        setError(`patch failed (rc=${pat.returncode})`);
        return;
      }
      setPhase('done');
    } catch (e: unknown) {
      const msg = e instanceof Error ? e.message : String(e);
      if (msg !== 'cancelled') setError(msg);
      setPhase('error');
    }
  };

  const stop = async () => {
    const id = liveTaskIdRef.current;
    if (!id) {
      // Already finished or no live task — just stop polling locally.
      cancelledRef.current = true;
      return;
    }
    setStopping(true);
    try {
      const r = await fetch(`${configSyncBase}/tasks/${id}/stop`, { method: 'POST' });
      if (!r.ok) {
        const txt = await r.text();
        setError(`stop failed: ${r.status} ${txt}`);
      }
      // Also tell the local poller to bail so the chain doesn't kick off the
      // next step against a freshly-cancelled task.
      cancelledRef.current = true;
    } catch (e: unknown) {
      setError(e instanceof Error ? e.message : String(e));
    } finally {
      setStopping(false);
    }
  };

  const running = phase === 'export' || phase === 'patch';
  // After both run, exportTask is still in memory — let the user flip back to
  // it while patch runs / after it finishes, instead of locking the log view
  // to the most recent task.
  const [logSelect, setLogSelect] = useState<'export' | 'patch' | 'auto'>('auto');
  const effectiveSelect: 'export' | 'patch' =
    logSelect !== 'auto' ? logSelect : (phase === 'patch' ? 'patch' : 'export');
  const activeTask = effectiveSelect === 'patch' ? patchTask : exportTask;
  // When we know we're mid-export/patch but haven't received the first
  // /tasks/<id> response yet, surface that explicitly instead of leaving the
  // user staring at a "PENDING" badge — that's almost always a CORS/network
  // problem on /tasks rather than the task actually being pending.
  const showSkeletonLog = running && !activeTask;

  return (
    <div className="configsync-panel">
      <div className="configsync-header">
        <h2>Config Sync</h2>
        <span className="configsync-subtitle">
          export from <code>{fromEnv}</code> → patch into <code>{TO_ENV}</code>, then publish to S3
        </span>
      </div>

      <div className="configsync-form">
        <label className="configsync-field">
          <span>From env</span>
          <select value={fromEnv} disabled={running}
                  onChange={e => setFromEnv(e.target.value)}>
            {FROM_ENVS.map(e => <option key={e} value={e}>{e}</option>)}
          </select>
        </label>

        <label className="configsync-field">
          <span>Version prefix</span>
          <input type="text" value={versionPrefix} disabled={running}
                 onChange={e => setVersionPrefix(e.target.value)}
                 placeholder="v1" />
        </label>

        <label className="configsync-field">
          <span>S3 bucket</span>
          <input type="text" value={s3Bucket} disabled={running}
                 onChange={e => setS3Bucket(e.target.value)} />
        </label>

        <label className="configsync-field">
          <span>Parallel exports</span>
          <input type="number" min={1} max={32} value={parallel} disabled={running}
                 onChange={e => setParallel(Math.max(1, Number(e.target.value) || 1))} />
        </label>

        <div className="configsync-resolved">
          <span>To: <code>{TO_ENV}</code></span>
          <span>S3 key: <code>s3://{s3Bucket}/{s3Prefix}.zip</code></span>
          <span>Server: <code>{configSyncBase}</code></span>
        </div>

        <div className="configsync-actions">
          <button
            className="configsync-run"
            disabled={running || !fromEnv || !versionPrefix.trim() || !s3Bucket.trim()}
            onClick={run}>
            {running ? 'Running…' : 'Export + Patch + Publish'}
          </button>
          {running && (
            <button
              className="configsync-cancel"
              onClick={stop}
              disabled={stopping}
              title="SIGTERM the running config_transfer subprocess (and its passetto-server child)">
              {stopping ? 'Stopping…' : 'Stop'}
            </button>
          )}
        </div>

        {error && <div className="configsync-error">{error}</div>}
        {skipMessage && phase === 'skipped' && (
          <div className="configsync-skip">{skipMessage}</div>
        )}
      </div>

      <div className="configsync-steps">
        <Step
          label="1. Export"
          task={exportTask}
          phase={phase}
          mine="export"
        />
        <Step
          label="2. Patch + S3"
          task={patchTask}
          phase={phase}
          mine="patch"
        />
      </div>

      {(activeTask || showSkeletonLog || phase === 'done' || phase === 'error') && (
        <div className="configsync-log-wrap">
          <div className="configsync-log-tabs">
            <button
              className={`configsync-log-tab ${effectiveSelect === 'export' ? 'active' : ''}`}
              onClick={() => setLogSelect('export')}
              disabled={!exportTask && !showSkeletonLog}>
              Export {exportTask ? `(${exportTask.log.length})` : ''}
            </button>
            <button
              className={`configsync-log-tab ${effectiveSelect === 'patch' ? 'active' : ''}`}
              onClick={() => setLogSelect('patch')}
              disabled={!patchTask && phase !== 'patch'}>
              Patch + S3 {patchTask ? `(${patchTask.log.length})` : ''}
            </button>
          </div>
          <div className="configsync-log-header">
            <span>
              {activeTask
                ? `${activeTask.cmd} log`
                : showSkeletonLog
                  ? `${phase} — waiting for first /tasks response…`
                  : 'log'}
              {activeTask && (
                <span className={`configsync-pill configsync-pill-${activeTask.status}`}>
                  {activeTask.status === 'running'
                    ? `running · ${formatDur(activeTask.started_at, null)}`
                    : activeTask.status === 'succeeded'
                      ? `done · ${formatDur(activeTask.started_at, activeTask.finished_at)}`
                      : activeTask.status === 'cancelled'
                        ? `cancelled · ${formatDur(activeTask.started_at, activeTask.finished_at)}`
                        : `failed · rc=${activeTask.returncode ?? '?'}`}
                </span>
              )}
            </span>
            <button
              className="configsync-copy"
              onClick={() => {
                const text = activeTask?.log?.join('\n') || '';
                navigator.clipboard?.writeText(text).catch(() => { });
              }}
              disabled={!activeTask?.log?.length}
              title="Copy log to clipboard">
              📋 Copy
            </button>
          </div>
          <pre ref={logRef} className="configsync-log">
            {activeTask
              ? (activeTask.log.length ? activeTask.log.join('\n') : '(no output yet — polling)')
              : `Started ${phase} task on ${configSyncBase}; polling /tasks/<id>.\nIf this stays here, check the browser Network tab — likely CORS or the server isn't running with the latest code.`}
          </pre>
          <div className="configsync-log-footer">
            <span>{activeTask?.log?.length ?? 0} lines · polling every 1.5s</span>
            {activeTask?.id && <span>task: {activeTask.id}</span>}
          </div>
        </div>
      )}
    </div>
  );
};

const Step: React.FC<{
  label: string;
  task: TaskView | null;
  phase: Phase;
  mine: 'export' | 'patch';
}> = ({ label, task, phase, mine }) => {
  const isActive = phase === mine;
  const status: TaskView['status'] | 'pending' | 'starting' = task
    ? task.status
    : phase === 'done' || (phase === 'patch' && mine === 'export') ? 'succeeded'
    : isActive ? 'starting'
    : 'pending';
  return (
    <div className={`configsync-step status-${status} ${isActive ? 'active' : ''}`}>
      <div className="configsync-step-label">{label}</div>
      <div className="configsync-step-status">{status}</div>
      {task && (
        <div className="configsync-step-meta">
          rc={task.returncode ?? '—'} · {formatDur(task.started_at, task.finished_at)}
        </div>
      )}
    </div>
  );
};

