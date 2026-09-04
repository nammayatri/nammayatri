import React, { useEffect, useRef, useState } from 'react';
import { configSyncBaseFor } from '../config';
import './ConfigSyncPanel.css';

interface TaskView {
  id: string;
  cmd: string;
  status: 'running' | 'succeeded' | 'failed' | 'cancelled';
  started_at: number | null;
  finished_at: number | null;
  returncode: number | null;
  log: string[];
}

interface AvailableVersion {
  version: number;
  metadata: string;
}

// to=local is intentionally hardcoded: this panel only seeds the dev box.
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

// Export/Patch/Publish is now DB Manager's job — it's the properly
// role-gated (MASTER/ADMIN), centrally-run path for actually touching real
// master/prod/prod_international databases. This panel's only job is the
// other half: pick an already-published version (from metadata.json, which
// DB Manager writes to S3 after a successful patch) and pull it down into
// this dev box. No direct export/patch trigger lives here anymore.
export const ConfigSyncPanel: React.FC = () => {
  const [importFromEnv, setImportFromEnv] = useState<string>(() =>
    localStorage.getItem('configsync.import.from') || 'master');
  const [versions, setVersions] = useState<AvailableVersion[]>([]);
  const [versionsLoading, setVersionsLoading] = useState(false);
  const [versionsError, setVersionsError] = useState<string | null>(null);
  const [selectedVersion, setSelectedVersion] = useState<string>('');
  const [dryRun, setDryRun] = useState(false);
  const [importPhase, setImportPhase] = useState<'idle' | 'running' | 'done' | 'error' | 'cancelled'>('idle');
  const [importTask, setImportTask] = useState<TaskView | null>(null);
  const [importError, setImportError] = useState<string | null>(null);
  const [importStopping, setImportStopping] = useState(false);
  const importCancelledRef = useRef(false);
  const importLiveTaskIdRef = useRef<string | null>(null);
  const importLogRef = useRef<HTMLPreElement | null>(null);

  useEffect(() => { localStorage.setItem('configsync.import.from', importFromEnv); }, [importFromEnv]);

  useEffect(() => {
    if (importLogRef.current) importLogRef.current.scrollTop = importLogRef.current.scrollHeight;
  }, [importTask?.log]);

  const importDirection = `${importFromEnv}_to_${TO_ENV}`;
  const importConfigSyncBase = configSyncBaseFor(importFromEnv);

  const startTask = async (base: string, path: string, body: Record<string, unknown>): Promise<string> => {
    const r = await fetch(`${base}${path}`, {
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

  const pollTask = async (
    base: string, id: string, cancelRef: React.MutableRefObject<boolean>, setter: (t: TaskView) => void
  ): Promise<TaskView> => {
    while (true) {
      if (cancelRef.current) throw new Error('cancelled');
      const r = await fetch(`${base}/tasks/${id}`);
      if (!r.ok) throw new Error(`GET /tasks/${id} failed: ${r.status}`);
      const t: TaskView = await r.json();
      setter(t);
      if (t.status !== 'running') return t;
      await new Promise(res => setTimeout(res, 1500));
    }
  };

  const fetchVersions = async () => {
    setVersionsLoading(true);
    setVersionsError(null);
    try {
      const r = await fetch(`${importConfigSyncBase}/versions?direction=${importDirection}`);
      if (!r.ok) {
        const detail = await r.text();
        throw new Error(`GET /versions failed: ${r.status} ${detail}`);
      }
      const j: { versions: AvailableVersion[] } = await r.json();
      const sorted = [...(j.versions || [])].sort((a, b) => b.version - a.version);
      setVersions(sorted);
      setSelectedVersion(prev =>
        sorted.some(v => `v${v.version}` === prev) ? prev : sorted.length ? `v${sorted[0].version}` : '');
    } catch (e: unknown) {
      setVersionsError(e instanceof Error ? e.message : String(e));
      setVersions([]);
      setSelectedVersion('');
    } finally {
      setVersionsLoading(false);
    }
  };

  // Re-fetch whenever the source env (hence the direction/server) changes —
  // fetchVersions itself is intentionally not memoized; only importFromEnv
  // should re-trigger this.
  // eslint-disable-next-line react-hooks/exhaustive-deps
  useEffect(() => { fetchVersions(); }, [importFromEnv]);

  const runImport = async () => {
    if (!selectedVersion) return;
    importCancelledRef.current = false;
    setImportError(null);
    setImportTask(null);
    importLiveTaskIdRef.current = null;
    setImportPhase('running');
    try {
      const taskId = await startTask(importConfigSyncBase, '/import', {
        from: importFromEnv,
        to: TO_ENV,
        version: selectedVersion,
        dry_run: dryRun,
      });
      importLiveTaskIdRef.current = taskId;
      const t = await pollTask(importConfigSyncBase, taskId, importCancelledRef, setImportTask);
      importLiveTaskIdRef.current = null;
      if (t.status === 'cancelled') { setImportPhase('cancelled'); return; }
      if (t.status !== 'succeeded') {
        setImportPhase('error');
        setImportError(`import failed (rc=${t.returncode})`);
        return;
      }
      setImportPhase('done');
    } catch (e: unknown) {
      const msg = e instanceof Error ? e.message : String(e);
      if (msg !== 'cancelled') setImportError(msg);
      setImportPhase('error');
    }
  };

  const stopImport = async () => {
    const id = importLiveTaskIdRef.current;
    if (!id) {
      importCancelledRef.current = true;
      return;
    }
    setImportStopping(true);
    try {
      const r = await fetch(`${importConfigSyncBase}/tasks/${id}/stop`, { method: 'POST' });
      if (!r.ok) {
        const txt = await r.text();
        setImportError(`stop failed: ${r.status} ${txt}`);
      }
      importCancelledRef.current = true;
    } catch (e: unknown) {
      setImportError(e instanceof Error ? e.message : String(e));
    } finally {
      setImportStopping(false);
    }
  };

  const importRunning = importPhase === 'running';

  return (
    <div className="configsync-panel">
      <div className="configsync-header">
        <h2>Config Sync — Import</h2>
        <span className="configsync-subtitle">
          pulls an already-patched, already-published zip from S3 (via <code>metadata.json</code>) and imports it into <code>{TO_ENV}</code>. Export/Patch/Publish now happens exclusively through DB Manager.
        </span>
      </div>

      <div className="configsync-form">
        <label className="configsync-field">
          <span>From env</span>
          <select value={importFromEnv} disabled={importRunning}
                  onChange={e => setImportFromEnv(e.target.value)}>
            {FROM_ENVS.map(e => <option key={e} value={e}>{e}</option>)}
          </select>
        </label>

        <label className="configsync-field">
          <span>Version</span>
          <select value={selectedVersion} disabled={importRunning || versionsLoading || !versions.length}
                  onChange={e => setSelectedVersion(e.target.value)}>
            {!versions.length && (
              <option value="">{versionsLoading ? 'loading…' : 'no published versions'}</option>
            )}
            {versions.map(v => (
              <option key={v.version} value={`v${v.version}`}>
                v{v.version}{v.metadata ? ` — ${v.metadata}` : ''}
              </option>
            ))}
          </select>
        </label>

        <label className="configsync-field" style={{ flexDirection: 'row', alignItems: 'center', gap: 6 }}>
          <input type="checkbox" checked={dryRun} disabled={importRunning}
                 onChange={e => setDryRun(e.target.checked)} />
          <span>Dry run (write SQL only, don't execute)</span>
        </label>

        <div className="configsync-resolved">
          <span>To: <code>{TO_ENV}</code></span>
          <span>Direction: <code>{importDirection}</code></span>
          <span>Server: <code>{importConfigSyncBase}</code></span>
        </div>

        <div className="configsync-actions">
          <button className="configsync-run" disabled={importRunning || !selectedVersion} onClick={runImport}>
            {importRunning ? 'Importing…' : 'Import'}
          </button>
          <button className="configsync-cancel" onClick={fetchVersions} disabled={versionsLoading || importRunning}
                  title="Re-fetch metadata.json">
            {versionsLoading ? 'Refreshing…' : '↻ Refresh versions'}
          </button>
          {importRunning && (
            <button className="configsync-cancel" onClick={stopImport} disabled={importStopping}
                    title="SIGTERM the running config_transfer subprocess">
              {importStopping ? 'Stopping…' : 'Stop'}
            </button>
          )}
        </div>

        {versionsError && <div className="configsync-error">Could not load versions: {versionsError}</div>}
        {importError && <div className="configsync-error">{importError}</div>}
      </div>

      {(importTask || importRunning) && (
        <div className="configsync-log-wrap">
          <div className="configsync-log-header">
            <span>
              import log
              {importTask && (
                <span className={`configsync-pill configsync-pill-${importTask.status}`}>
                  {importTask.status === 'running'
                    ? `running · ${formatDur(importTask.started_at, null)}`
                    : importTask.status === 'succeeded'
                      ? `done · ${formatDur(importTask.started_at, importTask.finished_at)}`
                      : importTask.status === 'cancelled'
                        ? `cancelled · ${formatDur(importTask.started_at, importTask.finished_at)}`
                        : `failed · rc=${importTask.returncode ?? '?'}`}
                </span>
              )}
            </span>
            <button
              className="configsync-copy"
              onClick={() => {
                const text = importTask?.log?.join('\n') || '';
                navigator.clipboard?.writeText(text).catch(() => { });
              }}
              disabled={!importTask?.log?.length}
              title="Copy log to clipboard">
              📋 Copy
            </button>
          </div>
          <pre ref={importLogRef} className="configsync-log">
            {importTask
              ? (importTask.log.length ? importTask.log.join('\n') : '(no output yet — polling)')
              : `Started import task on ${importConfigSyncBase}; polling /tasks/<id>.`}
          </pre>
          <div className="configsync-log-footer">
            <span>{importTask?.log?.length ?? 0} lines · polling every 1.5s</span>
            {importTask?.id && <span>task: {importTask.id}</span>}
          </div>
        </div>
      )}
    </div>
  );
};
