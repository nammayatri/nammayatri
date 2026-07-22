import React, { useEffect, useState } from 'react';
import { Terminal } from './Terminal';
import {
  LOCAL_API_BASE,
  setContextApiBase,
  getContextApiBaseOverride,
  getContextApiBaseDefault,
} from '../config';
import {
  remoteDeploy,
  remoteStart,
  remoteStop,
  remoteStatus,
  remoteClearData,
  remoteSessions,
  remoteSyncCaddyPort,
  remoteCabalClean,
  resolveDevbox,
  remoteLogList,
  remoteLogTail,
  remoteLogClear,
  setupSsh,
  RemoteTarget,
  DevboxAssignment,
} from '../services/remote';
import './RemoteStackPanel.css';

interface PanelState {
  deploySession?: string;
  startSession?: string;
  clearSession?: string;
  cabalCleanSession?: string;
  startCols?: number;
  startRows?: number;
  status?: string;
  error?: string;
  caddyPort?: number;
}

const DEFAULT_REMOTE_DIR = '/tmp/nammayatri';
// Canonical start command — sent server-side as the inner command of the PTY.
// The exact argv (including this string) is echoed into the terminal by
// local-api as the first line of the session, so users see what runs.
const DEFAULT_COMMAND = 'cd Backend && nix develop .#backend -c , run-mobility-stack-dev';
const FORM_STORAGE_KEY = 'ny.remoteStack.target';
const MODE_STORAGE_KEY = 'ny.remoteStack.mode';

type RunMode = 'local' | 'devbox';

const loadStoredTarget = (): RemoteTarget => {
  try {
    const raw = window.localStorage.getItem(FORM_STORAGE_KEY);
    if (raw) return { ...defaultTarget(), ...JSON.parse(raw) };
  } catch {
    /* ignore */
  }
  return defaultTarget();
};

const loadStoredMode = (): RunMode => {
  try {
    const raw = window.localStorage.getItem(MODE_STORAGE_KEY);
    if (raw === 'local' || raw === 'devbox') return raw;
    // Migration from the old 5-field form: a stored remote host means devbox.
    const t = loadStoredTarget();
    if (t.host && !['localhost', '127.0.0.1', '::1'].includes(t.host.trim())) return 'devbox';
  } catch {
    /* ignore */
  }
  return 'local';
};

const defaultTarget = (): RemoteTarget => ({
  host: 'localhost',
  user: '',
  port: 22,
  identityFile: '',
  devName: '',
  remoteDir: DEFAULT_REMOTE_DIR,
  copyMode: 'rsync',
  command: DEFAULT_COMMAND,
});

export const RemoteStackPanel: React.FC = () => {
  const [target, setTarget] = useState<RemoteTarget>(loadStoredTarget);
  const [state, setState] = useState<PanelState>({});
  const [busy, setBusy] = useState<'deploy' | 'start' | 'stop' | 'clear-data' | 'cabal-clean' | null>(null);
  const [override] = useState<string | null>(getContextApiBaseOverride());
  const [mode, setMode] = useState<RunMode>(loadStoredMode);
  const [devbox, setDevbox] = useState<DevboxAssignment | null>(null);
  const [devboxErr, setDevboxErr] = useState<string | null>(null);
  const [resolving, setResolving] = useState(false);
  const [sshStatus, setSshStatus] = useState<{ ok: boolean; message?: string } | null>(null);

  // ── Log viewer ──
  const [logsOpen, setLogsOpen] = useState(false);
  const [logFiles, setLogFiles] = useState<string[]>([]);
  const [selectedLog, setSelectedLog] = useState<string>('');
  const [logContent, setLogContent] = useState<string>('');
  const [logError, setLogError] = useState<string | null>(null);
  const [logLoading, setLogLoading] = useState(false);
  const [logFollow, setLogFollow] = useState(true);
  const [logTruncated, setLogTruncated] = useState(false);
  const [logSearch, setLogSearch] = useState('');
  const [logSort, setLogSort] = useState<'none' | 'asc' | 'desc'>('none');
  const [logRefreshNonce, setLogRefreshNonce] = useState(0);
  const [logClearing, setLogClearing] = useState(false);
  const logBodyRef = React.useRef<HTMLPreElement | null>(null);
  const logSortRef = React.useRef(logSort);
  logSortRef.current = logSort;

  // Matches "2026-07-21 12:53:09.269707939" (space or T separator, optional frac).
  // The format is fixed-width + zero-padded, so lexicographic string order == time order.
  const TS_RE = /(\d{4}-\d{2}-\d{2}[ T]\d{2}:\d{2}:\d{2}(?:\.\d+)?)/;

  // Client-side view: filter by search, then optionally sort by timestamp.
  const displayedLog = React.useMemo(() => {
    let lines = logContent.split('\n');
    const q = logSearch.trim().toLowerCase();
    if (q) lines = lines.filter(l => l.toLowerCase().includes(q));
    if (logSort !== 'none') {
      // Each line's key = its own timestamp, else the previous line's (so a
      // multi-line entry's continuation stays grouped with its parent).
      let last = '';
      const keyed = lines.map((line, i) => {
        const m = line.match(TS_RE);
        if (m) last = m[1];
        return { line, i, ts: last };
      });
      keyed.sort((a, b) => {
        if (a.ts !== b.ts) {
          if (!a.ts) return 1;   // untimestamped lines sink to the end
          if (!b.ts) return -1;
          const cmp = a.ts < b.ts ? -1 : 1;
          return logSort === 'asc' ? cmp : -cmp;
        }
        return a.i - b.i;        // stable
      });
      lines = keyed.map(k => k.line);
    }
    return lines.join('\n');
  }, [logContent, logSearch, logSort, TS_RE]);
  
  const matchCount = React.useMemo(
    () => (logSearch.trim() ? displayedLog.split('\n').filter(Boolean).length : 0),
    [displayedLog, logSearch],
  );

  const isLocalhost = !target.host || ['localhost', '127.0.0.1', '::1'].includes(target.host.trim());
  const hasDevName = isLocalhost || !!target.devName?.trim();
  // In Dev-Box mode nothing may run until an assignment is resolved —
  // otherwise a stale localStorage target could be deployed to.
  const devboxNotReady = mode === 'devbox' && (resolving || !devbox);

  // Apply a resolved devbox assignment to the deploy target and verify SSH.
  const applyAssignment = async (a: DevboxAssignment) => {
    setDevbox(a);
    setTarget({
      host: a.host || '',
      user: a.sshUser || '',
      port: a.port || 22,
      identityFile: '',
      devName: a.id || '',
      remoteDir: a.remoteDir || DEFAULT_REMOTE_DIR,
      copyMode: 'rsync',
      command: DEFAULT_COMMAND,
    });
    if (a.host && a.sshUser) {
      try {
        const res = await setupSsh(a.host, a.sshUser, a.port || 22);
        setSshStatus(res.status === 'ok'
          ? { ok: true, message: res.message }
          : { ok: false, message: res.message });
      } catch (e) {
        setSshStatus({ ok: false, message: `SSH check failed: ${e}` });
      }
    }
  };

  // Resolve the devbox assignment (auto-picks lowest-RAM machine on first use;
  // afterwards reuses the pin from .devbox-id.json at the repo root).
  const resolveAssignment = async (forceNew = false) => {
    setResolving(true);
    setDevboxErr(null);
    setSshStatus(null);
    try {
      const a = await resolveDevbox(forceNew);
      if (a.error || !a.host || !a.id) {
        setDevbox(null);
        setDevboxErr(a.error || 'devbox resolution returned no machine');
      } else {
        await applyAssignment(a);
      }
    } catch (e) {
      setDevbox(null);
      setDevboxErr(`devbox resolution failed: ${e}`);
    } finally {
      setResolving(false);
    }
  };

  // Mode switch: Local uses this machine directly (no SSH/rsync); Dev-Box
  // resolves the pinned/auto-assigned fleet machine.
  useEffect(() => {
    try { window.localStorage.setItem(MODE_STORAGE_KEY, mode); } catch { /* ignore */ }
    if (mode === 'local') {
      setDevbox(null);
      setDevboxErr(null);
      setSshStatus(null);
      setTarget(defaultTarget());
    } else {
      resolveAssignment(false);
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [mode]);

  const onReassign = () => {
    if (window.confirm(
      'Re-assign this checkout to the currently least-loaded dev-box?\n' +
      'A new developer id is generated — any state under the old remote dir is left behind.'
    )) {
      resolveAssignment(true);
    }
  };

  // Persist form fields across page reloads / tab switches.
  useEffect(() => {
    try {
      window.localStorage.setItem(FORM_STORAGE_KEY, JSON.stringify(target));
    } catch {
      /* ignore */
    }
    // Sync the host to local-api so launcher specs can use ${host}.
    fetch(`${LOCAL_API_BASE}/api/remote/host`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ host: target.host || 'localhost' }),
    }).catch(() => {});
  }, [target]);

  // On mount, ask local-api whether any sessions are still alive and reattach
  // their terminals. This makes the panel survive React tab switches /
  // dashboard reloads — the PTY itself lives in local-api, not in React.
  useEffect(() => {
    let cancelled = false;
    (async () => {
      try {
        const sessions = await remoteSessions();
        if (cancelled) return;
        const live = sessions.filter(s => s.running);
        if (live.length === 0) return;
        const deploy = live.find(s => s.kind === 'deploy');
        const start = live.find(s => s.kind === 'start');
        const clear = live.find(s => s.kind === 'clear-data');
        const cabalClean = live.find(s => s.kind === 'cabal-clean');
        setState(prev => ({
          ...prev,
          deploySession: prev.deploySession ?? deploy?.id,
          startSession: prev.startSession ?? start?.id,
          startCols: prev.startCols ?? start?.cols,
          startRows: prev.startRows ?? start?.rows,
          clearSession: prev.clearSession ?? clear?.id,
          cabalCleanSession: prev.cabalCleanSession ?? cabalClean?.id,
          status: start ? `mobility-stack-dev still running on ${start.host}` : prev.status,
        }));
      } catch {
        /* offline / local-api down — leave empty state */
      }
    })();
    return () => { cancelled = true; };
  }, []);

  // Generic poller: when a session ends, clear it on success, keep on failure.
  const usePoller = (
    sessionId: string | undefined,
    onExit: (exitCode: number | null | undefined) => void,
  ) => {
    useEffect(() => {
      if (!sessionId) return;
      const t = setInterval(async () => {
        const s = await remoteStatus(sessionId!);
        if (!s.running) {
          onExit(s.exitCode);
          clearInterval(t);
        }
      }, 2000);
      return () => clearInterval(t);
      // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [sessionId]);
  };

  usePoller(state.deploySession, code =>
    setState(prev =>
      code === 0
        ? { ...prev, deploySession: undefined, status: 'Deploy complete.' }
        : { ...prev, status: `Deploy exited (code ${code}).` },
    ),
  );
  usePoller(state.startSession, code =>
    setState(prev =>
      code === 0
        ? { ...prev, startSession: undefined, status: 'Stack exited cleanly.' }
        : { ...prev, status: `Stack exited (code ${code}).` },
    ),
  );
  usePoller(state.clearSession, code =>
    setState(prev =>
      code === 0
        ? { ...prev, clearSession: undefined, status: 'Clear-data complete.' }
        : { ...prev, status: `Clear-data exited (code ${code}).` },
    ),
  );
  usePoller(state.cabalCleanSession, code =>
    setState(prev =>
      code === 0
        ? { ...prev, cabalCleanSession: undefined, status: 'Cabal clean complete.' }
        : { ...prev, status: `Cabal clean exited (code ${code}).` },
    ),
  );

  const onDeploy = async () => {
    setBusy('deploy');
    setState(prev => ({ ...prev, error: undefined, status: 'Deploying…' }));
    try {
      const res = await remoteDeploy(target);
      if (res.error) {
        setState(prev => ({ ...prev, error: res.error, status: undefined }));
      } else if (res.skipped) {
        setState(prev => ({
          ...prev,
          deploySession: res.session,
          status: 'No copy needed (localhost or copyMode=skip).',
        }));
      } else {
        setState(prev => ({
          ...prev,
          deploySession: res.session,
          status: 'rsync running…',
        }));
      }
    } catch (e) {
      const msg = e instanceof Error ? e.message : String(e);
      setState(prev => ({ ...prev, error: `Deploy failed: ${msg}`, status: undefined }));
    } finally {
      setBusy(null);
    }
  };

  const onClearData = async () => {
    setBusy('clear-data');
    setState(prev => ({ ...prev, error: undefined, status: 'Clearing data…' }));
    try {
      const res = await remoteClearData(target);
      if (res.error || !res.session) {
        setState(prev => ({ ...prev, error: res.error || 'clear-data failed', status: undefined }));
      } else {
        setState(prev => ({ ...prev, clearSession: res.session, status: 'clear-data running…' }));
      }
    } catch (e) {
      const msg = e instanceof Error ? e.message : String(e);
      setState(prev => ({ ...prev, error: `Clear-data failed: ${msg}`, status: undefined }));
    } finally {
      setBusy(null);
    }
  };

  const onStopClearData = async () => {
    if (!state.clearSession) return;
    setBusy('clear-data');
    try {
      await remoteStop(state.clearSession);
      setState(prev => ({ ...prev, clearSession: undefined, status: 'Clear-data stopped.' }));
    } catch (e) {
      setState(prev => ({ ...prev, clearSession: undefined, error: `Stop failed: ${e}`, status: undefined }));
    } finally {
      setBusy(null);
    }
  };

  const onStart = async () => {
    setBusy('start');
    setState(prev => ({ ...prev, error: undefined, status: 'Starting…' }));
    try {
      // Always send the canonical Start command so the field displayed in
      // the UI matches what local-api executes (even if an older value got
      // persisted to localStorage from a previous build).
      const res = await remoteStart({ ...target, command: DEFAULT_COMMAND });
      if (res.error || !res.session) {
        setState(prev => ({ ...prev, error: res.error || 'start failed', status: undefined }));
      } else {
        setState(prev => ({
          ...prev,
          startSession: res.session,
          startCols: res.cols,
          startRows: res.rows,
          status: `mobility-stack-dev running on ${target.host}`,
        }));
      }
    } catch (e) {
      const msg = e instanceof Error ? e.message : String(e);
      setState(prev => ({ ...prev, error: `Start failed: ${msg}`, status: undefined }));
    } finally {
      setBusy(null);
    }
  };

  const onStop = async () => {
    if (!state.startSession) return;
    setBusy('stop');
    try {
      await remoteStop(state.startSession);
      setState(prev => ({ ...prev, startSession: undefined, status: 'Stopped.' }));
    } catch (e) {
      const msg = e instanceof Error ? e.message : String(e);
      setState(prev => ({ ...prev, startSession: undefined, error: `Stop failed: ${msg}`, status: undefined }));
    } finally {
      setBusy(null);
    }
  };

  useEffect(() => {
    if (!state.startSession || isLocalhost || !target.devName?.trim()) return;
    let cancelled = false;
    let attempts = 0;
    let iv: number | undefined;
    const stop = () => { if (iv != null) { window.clearInterval(iv); iv = undefined; } };
    const tick = async () => {
      if (cancelled || ++attempts > 45) { stop(); return; }
      try {
        const res = await remoteSyncCaddyPort(target);
        if (!cancelled && res.caddyPort != null) {
          setState(prev => ({ ...prev, caddyPort: res.caddyPort }));
          stop();
        }
      } catch { /* registry not ready yet — keep polling */ }
    };
    tick();
    iv = window.setInterval(tick, 7000);
    return () => { cancelled = true; stop(); };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [state.startSession]);

  const onCabalClean = async () => {
    setBusy('cabal-clean');
    setState(prev => ({ ...prev, error: undefined, status: 'Running cabal clean…' }));
    try {
      const res = await remoteCabalClean(target);
      if (res.error || !res.session) {
        setState(prev => ({ ...prev, error: res.error || 'cabal clean failed', status: undefined }));
      } else {
        setState(prev => ({ ...prev, cabalCleanSession: res.session, status: 'cabal clean running…' }));
      }
    } catch (e) {
      const msg = e instanceof Error ? e.message : String(e);
      setState(prev => ({ ...prev, error: `Cabal clean failed: ${msg}`, status: undefined }));
    } finally {
      setBusy(null);
    }
  };

  const onUseRemoteContextApi = async () => {
    const host = target.host || 'localhost';
    let ctxPort = 7082;
    try {
      const res = await remoteSyncCaddyPort(target);
      if (res.contextApiPort != null) ctxPort = res.contextApiPort;
    } catch { /* registry unavailable — fall back to base port */ }
    const url = `http://${host}:${ctxPort}`;
    setContextApiBase(url); // reloads page
  };

  const onResetContextApi = () => setContextApiBase(null);

  // Open the log viewer: fetch the list of *.log files from the stack workspace.
  const onOpenLogs = async () => {
    setLogsOpen(true);
    setLogError(null);
    setLogContent('');
    setLogLoading(true);
    try {
      const res = await remoteLogList(target);
      if (res.error) {
        setLogError(res.error);
        setLogFiles([]);
      } else {
        const files = res.files || [];
        setLogFiles(files);
        // Keep the previous selection if it's still present, else pick a sensible default.
        const pref = files.includes(selectedLog)
          ? selectedLog
          : (files.find(f => f.includes('rider-app-exe')) || files[0] || '');
        setSelectedLog(pref);
        if (!files.length) setLogError('No *.log files found — is the stack running?');
      }
    } catch (e) {
      setLogError(`Failed to list logs: ${e}`);
    } finally {
      setLogLoading(false);
    }
  };

  // Fetch log content. follow ON → live tail (auto-refresh 2s, auto-scroll);
  // follow OFF → the FULL log, fetched once (refresh via the ↻ button).
  useEffect(() => {
    if (!logsOpen || !selectedLog) return;
    let cancelled = false;
    let iv: number | undefined;
    const fetchLog = async () => {
      try {
        const res = await remoteLogTail({ ...target, file: selectedLog, full: !logFollow, lines: 2000 });
        if (cancelled) return;
        if (res.error) {
          setLogError(res.error);
        } else {
          setLogError(null);
          setLogTruncated(!!res.truncated);
          setLogContent(res.content || '(empty)');
          if (logFollow && logSortRef.current === 'none' && logBodyRef.current) {
            const el = logBodyRef.current;
            requestAnimationFrame(() => { el.scrollTop = el.scrollHeight; });
          }
        }
      } catch (e) {
        if (!cancelled) setLogError(`Tail failed: ${e}`);
      }
    };
    fetchLog();
    if (logFollow) iv = window.setInterval(fetchLog, 2000);
    return () => { cancelled = true; if (iv != null) window.clearInterval(iv); };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [logsOpen, selectedLog, logFollow, logRefreshNonce]);

  // Truncate the selected log to empty on the box, then re-fetch.
  const onClearLogs = async () => {
    if (!selectedLog) return;
    if (!window.confirm(`Clear ${selectedLog}? This empties the file on ${isLocalhost ? 'this machine' : target.host}.`)) return;
    setLogClearing(true);
    try {
      const res = await remoteLogClear({ ...target, file: selectedLog });
      if (res.error) setLogError(res.error);
      else { setLogContent(''); setLogError(null); setLogRefreshNonce(n => n + 1); }
    } catch (e) {
      setLogError(`Clear failed: ${e}`);
    } finally {
      setLogClearing(false);
    }
  };

  return (
    <div className="remote-stack-panel">
      <div className="rsp-header">
        <h2>Remote Stack</h2>
        <p className="rsp-sub">
          Run the stack (<code>, run-mobility-stack-dev</code>: backend + test-context-api + mock-server)
          either on this machine (<b>Local</b>) or on an auto-assigned fleet <b>Dev-Box</b>.
          First Dev-Box use picks the least-loaded machine and pins it to this checkout
          via <code>.devbox-id.json</code>; after that the same machine and workspace are
          reused automatically.
        </p>
      </div>

      <div className="rsp-form">
        <label>
          <span>Run on</span>
          <select value={mode} onChange={e => setMode(e.target.value as RunMode)}>
            <option value="local">Local (this machine)</option>
            <option value="devbox">Dev-Box (auto-assigned)</option>
          </select>
        </label>
      </div>

      {mode === 'devbox' && (
        <div className="rsp-meta" style={{ marginTop: 8 }}>
          {resolving && <div>Resolving dev-box assignment…</div>}
          {devboxErr && (
            <div style={{ color: '#e53e3e' }}>
              {devboxErr}{' '}
              <button onClick={() => resolveAssignment(false)} style={{ marginLeft: 8 }}>Retry</button>
            </div>
          )}
          {devbox && !resolving && (
            <>
              <div>
                <span className="rsp-label">Assigned dev-box:</span>{' '}
                <code>{devbox.machine}</code> — <code>{devbox.host}</code>
                {devbox.resources?.cpu && (
                  <span> [{devbox.resources.cpu}, {devbox.resources.ram}]</span>
                )}
                {devbox.usage?.ram && <span> · RAM in use: {devbox.usage.ram}</span>}
                {devbox.created && <span className="rsp-tag">new assignment</span>}
                {devbox.repinned && <span className="rsp-tag">re-pinned (old machine offline)</span>}
              </div>
              <div>
                <span className="rsp-label">Developer id:</span> <code>{devbox.id}</code>
                {' '}<span className="rsp-label">workspace:</span> <code>{devbox.remoteDir}</code>
                <button
                  onClick={onReassign}
                  style={{ marginLeft: 12 }}
                  title="Drop the pin and re-pick the least-loaded dev-box (generates a new developer id)"
                >
                  Re-assign
                </button>
              </div>
            </>
          )}
        </div>
      )}

      <div className="rsp-actions">
        <button onClick={onDeploy} disabled={!!busy || devboxNotReady || !hasDevName || !!state.deploySession || !!state.startSession || !!state.clearSession || !!state.cabalCleanSession || (!isLocalhost && (!sshStatus || !sshStatus.ok))}>
          {busy === 'deploy' ? 'Deploying…' : 'Deploy (rsync)'}
        </button>
        {state.clearSession ? (
          <button onClick={onStopClearData} disabled={busy === 'clear-data'} style={{ background: '#e53e3e' }}>
            {busy === 'clear-data' ? 'Stopping…' : 'Stop Clear data'}
          </button>
        ) : (
          <button onClick={onClearData} disabled={!!busy || devboxNotReady || !hasDevName || !!state.deploySession || !!state.startSession || !!state.cabalCleanSession} title="Wipe runtime data under <repo>/data (postgres, kafka, metabase, …) using `, clear-data`.">
            {busy === 'clear-data' ? 'Clearing…' : 'Clear data'}
          </button>
        )}
        {state.startSession ? (
          <button
            onClick={onStop}
            disabled={busy === 'stop'}
            style={{ background: '#e53e3e' }}
            title="Stops ONLY the running mobility-stack-dev session — deploy / clear-data / cabal-clean are unaffected"
          >
            {busy === 'stop' ? 'Stopping…' : 'Stop mobility-stack-dev'}
          </button>
        ) : (
          <button onClick={onStart} disabled={!!busy || devboxNotReady || !hasDevName || !!state.deploySession || !!state.clearSession || !!state.cabalCleanSession}>
            {busy === 'start' ? 'Starting…' : 'Start mobility-stack-dev'}
          </button>
        )}
        <button
          onClick={onCabalClean}
          disabled={!!busy || devboxNotReady || !hasDevName || !!state.deploySession || !!state.startSession || !!state.clearSession}
          title="Run `cabal clean` in the Backend directory to clear stale build artifacts (fixes GHC panics / package-database corruption)"
        >
          {busy === 'cabal-clean' ? 'Cleaning…' : 'Cabal Clean'}
        </button>
        <button
          onClick={onOpenLogs}
          disabled={!state.startSession}
          title="View a service log (rider-app-exe.log, …) from the running stack's workspace"
        >
          View Logs
        </button>
        <span className="rsp-spacer" />
        <button onClick={onUseRemoteContextApi} disabled={!state.startSession} title="Point the dashboard at this host's test-context-api (port 7082) and reload">
          Use this context-api
        </button>
        <button onClick={onResetContextApi} disabled={!override} title="Reset to local test-context-api and reload">
          Reset
        </button>
      </div>

      <div className="rsp-meta">
        <div>
          <span className="rsp-label">local-api:</span> <code>{LOCAL_API_BASE}</code>
        </div>
        <div>
          <span className="rsp-label">context-api:</span>{' '}
          <code>{override || getContextApiBaseDefault()}</code>
          {override && <span className="rsp-tag">override</span>}
        </div>
        {state.caddyPort != null && (
          <div>
            <span className="rsp-label">Caddy port:</span>{' '}
            <code>{state.caddyPort}</code>
            {target.host && !isLocalhost && (
              <> — <code>http://{target.host}:{state.caddyPort}</code></>
            )}
          </div>
        )}
        {state.status && <div className="rsp-status">{state.status}</div>}
        {state.error && <div className="rsp-error">{state.error}</div>}
        {sshStatus && !isLocalhost && (
          sshStatus.ok
            ? <div className="rsp-status">SSH: {sshStatus.message}</div>
            : <div className="rsp-error" style={{ whiteSpace: 'pre-wrap' }}>
                SSH not set up. Run this command in your terminal:{'\n\n'}
                <code style={{ background: '#333', padding: '4px 8px', borderRadius: '4px', userSelect: 'all' }}>
                  {sshStatus.message?.match(/ssh-copy-id.*/)?.[0] || sshStatus.message}
                </code>
                {'\n\n'}Then select the machine again.
              </div>
        )}
      </div>

      <div className="rsp-terminals">
        {state.deploySession && (
          <div className="rsp-terminal">
            <div className="rsp-term-title">deploy ({target.host})</div>
            <Terminal
              baseUrl={LOCAL_API_BASE}
              pathPrefix="/api/remote"
              attachSessionId={state.deploySession}
            />
          </div>
        )}
        {state.clearSession && (
          <div className="rsp-terminal">
            <div className="rsp-term-title">clear-data ({target.host})</div>
            <Terminal
              baseUrl={LOCAL_API_BASE}
              pathPrefix="/api/remote"
              attachSessionId={state.clearSession}
            />
          </div>
        )}
        {state.cabalCleanSession && (
          <div className="rsp-terminal">
            <div className="rsp-term-title">cabal-clean ({target.host})</div>
            <Terminal
              baseUrl={LOCAL_API_BASE}
              pathPrefix="/api/remote"
              attachSessionId={state.cabalCleanSession}
            />
          </div>
        )}
        {state.startSession && (
          <div className="rsp-terminal">
            <div className="rsp-term-title">mobility-stack-dev ({target.host})</div>
            <Terminal
              baseUrl={LOCAL_API_BASE}
              pathPrefix="/api/remote"
              attachSessionId={state.startSession}
            />
          </div>
        )}
        {!state.deploySession && !state.startSession && !state.clearSession && !state.cabalCleanSession && (
          <div className="rsp-empty">
            Configure a target above, then click Deploy and Start.
          </div>
        )}
      </div>

      {logsOpen && (
        <div className="rsp-log-overlay" onClick={() => setLogsOpen(false)}>
          <div className="rsp-log-modal" onClick={e => e.stopPropagation()}>
            <div className="rsp-log-head">
              <span className="rsp-log-title">Service logs</span>
              <select
                className="rsp-log-select"
                value={selectedLog}
                onChange={e => { setSelectedLog(e.target.value); setLogContent(''); }}
                disabled={logLoading || !logFiles.length}
              >
                {!logFiles.length && <option value="">(no logs)</option>}
                {logFiles.map(f => <option key={f} value={f}>{f}</option>)}
              </select>
              <button className="rsp-log-btn" onClick={() => setLogRefreshNonce(n => n + 1)} title="Refresh now">↻</button>
              <label className="rsp-log-follow" title="On: live tail (auto-refresh). Off: load the FULL log.">
                <input type="checkbox" checked={logFollow} onChange={e => setLogFollow(e.target.checked)} />
                follow
              </label>
              <button
                className="rsp-log-btn rsp-log-btn-danger"
                onClick={onClearLogs}
                disabled={logClearing || !selectedLog}
                title="Empty this log file on the box"
              >
                {logClearing ? 'Clearing…' : 'Clear Logs'}
              </button>
              <span className="rsp-spacer" />
              <span className="rsp-log-src">{isLocalhost ? 'local' : `${target.user}@${target.host}`}</span>
              <button className="rsp-log-btn rsp-log-close" onClick={() => setLogsOpen(false)} title="Close">✕</button>
            </div>
            <div className="rsp-log-searchbar">
              <input
                className="rsp-log-search"
                type="text"
                placeholder="Search this log (case-insensitive, filters matching lines)…"
                value={logSearch}
                onChange={e => setLogSearch(e.target.value)}
              />
              {logSearch.trim() && (
                <span className="rsp-log-matches">
                  {matchCount} match{matchCount === 1 ? '' : 'es'}
                  <button className="rsp-log-btn" onClick={() => setLogSearch('')} title="Clear search">clear</button>
                </span>
              )}
              <label className="rsp-log-sortlabel" title="Sort lines by their timestamp">
                sort
                <select
                  className="rsp-log-sort"
                  value={logSort}
                  onChange={e => setLogSort(e.target.value as 'none' | 'asc' | 'desc')}
                >
                  <option value="none">file order</option>
                  <option value="asc">time ↑ (oldest first)</option>
                  <option value="desc">time ↓ (newest first)</option>
                </select>
              </label>
            </div>
            {logError && <div className="rsp-log-error">{logError}</div>}
            <pre className="rsp-log-body" ref={logBodyRef}>
              {logLoading ? 'Loading…' : (displayedLog || (logSearch.trim() ? '(no matching lines)' : '(waiting for output…)'))}
            </pre>
            <div className="rsp-log-foot">
              {logFollow ? 'live tail · auto-refresh 2s' : 'full log' }
              {logTruncated && ' (showing last 25 MB)'}
              {selectedLog && !isLocalhost && <> · <code>{target.remoteDir}/{selectedLog}</code></>}
            </div>
          </div>
        </div>
      )}
    </div>
  );
};
