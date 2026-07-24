import React, { useEffect, useState } from 'react';
import { Terminal } from './Terminal';
import { LOCAL_API_BASE } from '../config';
import {
  remoteDeploy,
  remoteStart,
  remoteStop,
  remoteStatus,
  remoteClearData,
  remoteSessions,
  remoteCabalClean,
  remotePreflight,
  remoteMark,
  resolveDevbox,
  setupSsh,
  remoteSshCopyId,
  openRemoteEditor,
  editorAvailable,
  RemoteTarget,
  DevboxAssignment,
  PreflightResponse,
} from '../services/remote';
import { RemoteLogsModal } from './RemoteLogsModal';
import './RemoteStackPanel.css';

interface PanelState {
  deploySession?: string;
  startSession?: string;
  clearSession?: string;
  cabalCleanSession?: string;
  sshCopySession?: string;
  startCols?: number;
  startRows?: number;
  status?: string;
  error?: string;
}

const DEFAULT_REMOTE_DIR = '/tmp/nammayatri';
// Canonical start command — sent server-side as the inner command of the PTY.
// The exact argv (including this string) is echoed into the terminal by
// local-api as the first line of the session, so users see what runs.
const DEFAULT_COMMAND = 'cd Backend && nix develop .#backend -c , run-mobility-stack-dev';
const FORM_STORAGE_KEY = 'ny.remoteStack.target';
const MODE_STORAGE_KEY = 'ny.remoteStack.mode';

type RunMode = 'local' | 'devbox';

// ── Start pipeline ──
// Deploy and Cabal Clean are not manual buttons any more: Start runs them
// automatically when local-api's pre-flight says they are needed (workspace
// files changed since the last rsync / the last run's commit is no longer in
// this branch's history). These steps are what the progress strip renders.
type StepKey = 'check' | 'deploy' | 'cabal-clean' | 'start';
type StepState = 'pending' | 'running' | 'done' | 'skipped' | 'failed';

interface Step {
  key: StepKey;
  label: string;
  state: StepState;
  detail?: string;
}

const INITIAL_STEPS: Step[] = [
  { key: 'check', label: 'Check workspace & git', state: 'pending' },
  { key: 'deploy', label: 'Deploy (rsync)', state: 'pending' },
  { key: 'cabal-clean', label: 'Cabal clean', state: 'pending' },
  { key: 'start', label: 'Start mobility-stack-dev', state: 'pending' },
];

const STEP_ICON: Record<StepState, string> = {
  pending: '○',
  running: '◐',
  done: '✓',
  skipped: '–',
  failed: '✕',
};

// ServiceDiscovery reports usage as "4.8Gi (8%)" / "12%" — pull the percentage
// out so it can drive a meter; null when the shape is anything else.
const usagePct = (usage?: string): number | null => {
  if (!usage) return null;
  const m = usage.match(/(\d+(?:\.\d+)?)\s*%/);
  if (!m) return null;
  const n = Number(m[1]);
  return Number.isFinite(n) ? Math.min(100, Math.max(0, n)) : null;
};

const usageLevel = (pct: number): 'ok' | 'warn' | 'hot' =>
  pct >= 85 ? 'hot' : pct >= 60 ? 'warn' : 'ok';

const Stat: React.FC<{ label: string; value: string; usage?: string }> = ({ label, value, usage }) => {
  const pct = usagePct(usage);
  return (
    <div className="rsp-stat" title={usage ? `${label}: ${usage} of ${value} in use` : `${label}: ${value}`}>
      <div className="rsp-stat-top">
        <span className="rsp-stat-label">{label}</span>
        <span className="rsp-stat-value">{value}</span>
      </div>
      <div className="rsp-stat-bar">
        <div className={`rsp-meter rsp-meter-${pct != null ? usageLevel(pct) : 'none'}`}>
          <span style={{ width: `${pct ?? 0}%` }} />
        </div>
        <span className={`rsp-stat-usage${pct == null ? ' rsp-stat-usage-none' : ''}`}>
          {pct != null ? usage : 'n/a'}
        </span>
      </div>
    </div>
  );
};

// "3m ago" / "2h ago" — deploy times only need to be readable at a glance; the
// exact timestamp is the element's title.
const timeAgo = (epochSeconds?: number | null): string => {
  if (!epochSeconds) return '';
  const secs = Math.max(0, Math.floor(Date.now() / 1000 - epochSeconds));
  if (secs < 45) return 'just now';
  const mins = Math.round(secs / 60);
  if (mins < 60) return `${mins}m ago`;
  const hours = Math.round(mins / 60);
  if (hours < 24) return `${hours}h ago`;
  return `${Math.round(hours / 24)}d ago`;
};

const timeExact = (epochSeconds?: number | null): string =>
  epochSeconds ? new Date(epochSeconds * 1000).toLocaleString() : '';

const shortHash = (h?: string): string => (h ? h.slice(0, 8) : '—');

const VSCodeIcon: React.FC = () => (
  <svg viewBox="0 0 32 32" width="13" height="13" aria-hidden="true">
    <path d="M24.6 2.2 30.4 5a1.4 1.4 0 0 1 .8 1.3v19.4a1.4 1.4 0 0 1-.8 1.3l-5.8 2.8a1.4 1.4 0 0 1-1.6-.27L9.3 16.9l-4.6 3.5-2.9-1.5a1 1 0 0 1 0-1.8l2.9-1.5-2.9-1.5a1 1 0 0 1 0-1.8l2.9-1.5 4.6 3.5L23 2.5a1.4 1.4 0 0 1 1.6-.3Zm.4 7.2-9 6.6 9 6.6V9.4Z" fill="#22a7f0" />
  </svg>
);

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
  const [busy, setBusy] = useState<'start' | 'stop' | 'clear-data' | null>(null);
  const [steps, setSteps] = useState<Step[] | null>(null);
  const [mode, setMode] = useState<RunMode>(loadStoredMode);
  const [devbox, setDevbox] = useState<DevboxAssignment | null>(null);
  const [devboxErr, setDevboxErr] = useState<string | null>(null);
  const [resolving, setResolving] = useState(false);
  const [sshStatus, setSshStatus] = useState<{ ok: boolean; message?: string } | null>(null);
  const [editorOk, setEditorOk] = useState(false);

  // ── Log viewer ── (contents live in RemoteLogsModal)
  const [logsOpen, setLogsOpen] = useState(false);

  // ── Sync state ── last deploy + whether the workspace has drifted since.
  // Same pre-flight the Start pipeline runs, just read for display.
  const [sync, setSync] = useState<PreflightResponse | null>(null);
  const [syncing, setSyncing] = useState(false);

  const isLocalhost = !target.host || ['localhost', '127.0.0.1', '::1'].includes(target.host.trim());
  const hasDevName = isLocalhost || !!target.devName?.trim();
  // In Dev-Box mode nothing may run until an assignment is resolved —
  // otherwise a stale localStorage target could be deployed to.
  const devboxNotReady = mode === 'devbox' && (resolving || !devbox);

  // Re-run the pre-flight purely to refresh the readout (fingerprinting the
  // workspace takes a moment, hence the explicit ⟳ rather than a poller).
  const refreshSync = async () => {
    if (isLocalhost || !target.user || !target.devName) {
      setSync(null);
      return;
    }
    setSyncing(true);
    try {
      setSync(await remotePreflight(target));
    } catch (e) {
      setSync({ error: e instanceof Error ? e.message : String(e) });
    } finally {
      setSyncing(false);
    }
  };

  useEffect(() => {
    refreshSync();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [target.host, target.user, target.devName, isLocalhost]);

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
        if (res.status === 'ok') {
          setSshStatus({ ok: true, message: res.message });
        } else {
          // No passwordless path yet — run ssh-copy-id right here so the only
          // thing left for the user is typing the dev-box password once.
          setSshStatus({ ok: false, message: res.message });
          const started = await remoteSshCopyId({
            host: a.host, user: a.sshUser, port: a.port || 22,
          });
          if (started.session) {
            setState(prev => ({ ...prev, sshCopySession: started.session }));
          }
        }
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

  // Only offer the open-in-editor button when `code` exists on this machine.
  useEffect(() => {
    let cancelled = false;
    editorAvailable()
      .then(res => { if (!cancelled) setEditorOk(!!res.available); })
      .catch(() => { if (!cancelled) setEditorOk(false); });
    return () => { cancelled = true; };
  }, []);

  const onOpenEditor = async () => {
    setState(prev => ({ ...prev, status: 'Opening VS Code…', error: undefined }));
    try {
      const res = await openRemoteEditor(target);
      setState(prev => res.error
        ? { ...prev, status: undefined, error: res.error }
        : { ...prev, status: `Opened ${res.opened}`, error: undefined });
    } catch (e) {
      const msg = e instanceof Error ? e.message : String(e);
      setState(prev => ({ ...prev, status: undefined, error: `Open editor failed: ${msg}` }));
    }
  };

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

  // Deploy / cabal-clean run as part of the Start pipeline — the progress strip
  // reports their outcome, so these pollers only retire the finished terminal.
  usePoller(state.deploySession, code => {
    setState(prev => (code === 0 ? { ...prev, deploySession: undefined } : prev));
    refreshSync();  // a finished rsync (Start's or the watcher's) moves both readouts
  });
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
    setState(prev => (code === 0 ? { ...prev, cabalCleanSession: undefined } : prev)),
  );

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

  const patchStep = (key: StepKey, patch: Partial<Step>) =>
    setSteps(prev => (prev ? prev.map(s => (s.key === key ? { ...s, ...patch } : s)) : prev));

  // Resolve once a session's process exits; yields its exit code.
  const waitForSession = (sessionId: string) =>
    new Promise<number | null>(resolve => {
      const iv = window.setInterval(async () => {
        try {
          const s = await remoteStatus(sessionId);
          if (!s.running) {
            window.clearInterval(iv);
            resolve(s.exitCode ?? null);
          }
        } catch {
          /* local-api hiccup — keep polling */
        }
      }, 1500);
    });

  // Start = pre-flight → (deploy if the workspace changed) → (cabal clean if
  // the last run's commit left this branch's history) → run the stack. Each
  // stage's outcome is reflected in the progress strip as it happens.
  const onStart = async () => {
    setBusy('start');
    setState(prev => ({ ...prev, error: undefined, status: undefined }));
    setSteps(INITIAL_STEPS);
    const fail = (key: StepKey, detail: string) => {
      patchStep(key, { state: 'failed', detail });
      setState(prev => ({ ...prev, error: detail, status: undefined }));
    };
    try {
      patchStep('check', { state: 'running' });
      const pre = await remotePreflight(target);
      if (pre.error) { fail('check', pre.error); return; }
      patchStep('check', {
        state: 'done',
        detail: pre.gitHead ? `HEAD ${pre.gitHead.slice(0, 8)}` : undefined,
      });

      if (pre.needsDeploy) {
        patchStep('deploy', { state: 'running', detail: pre.deployReason });
        const res = await remoteDeploy(target);
        if (res.error || !res.session) { fail('deploy', res.error || 'deploy failed'); return; }
        setState(prev => ({ ...prev, deploySession: res.session, status: 'rsync running…' }));
        const code = await waitForSession(res.session);
        if (code !== 0) { fail('deploy', `rsync exited (code ${code})`); return; }
        patchStep('deploy', { state: 'done', detail: 'workspace synced' });
      } else {
        patchStep('deploy', { state: 'skipped', detail: pre.deployReason });
      }

      if (pre.needsCabalClean) {
        patchStep('cabal-clean', { state: 'running', detail: pre.cabalCleanReason });
        const res = await remoteCabalClean(target);
        if (res.error || !res.session) { fail('cabal-clean', res.error || 'cabal clean failed'); return; }
        setState(prev => ({ ...prev, cabalCleanSession: res.session, status: 'cabal clean running…' }));
        const code = await waitForSession(res.session);
        if (code !== 0) { fail('cabal-clean', `cabal clean exited (code ${code})`); return; }
        patchStep('cabal-clean', { state: 'done', detail: 'stale build artifacts removed' });
      } else {
        patchStep('cabal-clean', { state: 'skipped', detail: pre.cabalCleanReason });
      }

      patchStep('start', { state: 'running' });
      // Always send the canonical Start command so the field displayed in
      // the UI matches what local-api executes (even if an older value got
      // persisted to localStorage from a previous build).
      const res = await remoteStart({ ...target, command: DEFAULT_COMMAND });
      if (res.error || !res.session) { fail('start', res.error || 'start failed'); return; }
      await remoteMark(target, 'start');
      patchStep('start', { state: 'done', detail: `running on ${target.host}` });
      setState(prev => ({
        ...prev,
        startSession: res.session,
        startCols: res.cols,
        startRows: res.rows,
        status: `mobility-stack-dev running on ${target.host}`,
      }));
    } catch (e) {
      const msg = e instanceof Error ? e.message : String(e);
      setState(prev => ({ ...prev, error: `Start failed: ${msg}`, status: undefined }));
      setSteps(prev => (prev ? prev.map(s => (s.state === 'running' ? { ...s, state: 'failed', detail: msg } : s)) : prev));
    } finally {
      setBusy(null);
      refreshSync();
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

  return (
    <div className="remote-stack-panel">
      <div className="rsp-header">
        <h2>Remote Stack</h2>
        <p className="rsp-sub">
          Runs the backend, test-context-api and mock-server — on this machine
          (<b>Local</b>) or on a fleet <b>Dev-Box</b>. The first Dev-Box run picks the
          least-loaded machine, authorizes your SSH key and pins the machine to this
          checkout; afterwards the same machine and workspace are reused, and code sync,
          rebuilds and ports are all handled for you. Press ▶ to start.
        </p>
      </div>

      <div className="rsp-form">
        <div className="rsp-run-row">
        <label>
          <span>Run on</span>
          <select value={mode} onChange={e => setMode(e.target.value as RunMode)}>
            <option value="local">Local (this machine)</option>
            <option value="devbox">Dev-Box (auto-assigned)</option>
          </select>
        </label>

        {state.startSession ? (
          <button
            className="rsp-icon-btn rsp-icon-stop"
            onClick={onStop}
            disabled={busy === 'stop'}
            aria-label="Stop mobility-stack-dev"
            title="Stop the running mobility-stack-dev session (deploy / clear-data / cabal-clean are unaffected)"
          >
            {busy === 'stop' ? '◌' : '■'}
          </button>
        ) : (
          <button
            className="rsp-icon-btn rsp-icon-play"
            onClick={onStart}
            disabled={!!busy || devboxNotReady || !hasDevName || !!state.clearSession || (!isLocalhost && (!sshStatus || !sshStatus.ok))}
            aria-label="Start mobility-stack-dev"
            title="Start mobility-stack-dev — deploys (if local files changed) and cabal-cleans (if the last run's commit left this branch's history) first"
          >
            {busy === 'start' ? '◌' : '▶'}
          </button>
        )}

        {state.clearSession ? (
          <button
            className="rsp-icon-btn rsp-icon-stop"
            onClick={onStopClearData}
            disabled={busy === 'clear-data'}
            aria-label="Stop clear data"
            title="Stop the running `, clear-data`"
          >
            {busy === 'clear-data' ? '◌' : '■'}
          </button>
        ) : (
          <button
            className="rsp-icon-btn rsp-icon-danger"
            onClick={onClearData}
            disabled={!!busy || devboxNotReady || !hasDevName || !!state.deploySession || !!state.startSession || !!state.cabalCleanSession}
            aria-label="Clear data"
            title="Clear data — wipes runtime state under <workspace>/data (postgres, kafka, metabase, …) using `, clear-data`"
          >
            {busy === 'clear-data' ? '◌' : '🗑'}
          </button>
        )}

        <button
          className="rsp-icon-btn"
          onClick={() => setLogsOpen(true)}
          disabled={!state.startSession}
          aria-label="View logs"
          title="View logs — tail a service log (rider-app-exe.log, …) from the running stack's workspace"
        >
          🗒
        </button>
        </div>
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
            <div className="rsp-devbox-card">
              <div className="rsp-devbox-head">
                <span className="rsp-devbox-machine">{devbox.machine}</span>
                <code className="rsp-devbox-host">{devbox.host}</code>
                {devbox.created && <span className="rsp-tag">new assignment</span>}
                {devbox.repinned && <span className="rsp-tag">re-pinned (old machine offline)</span>}
                {sshStatus && (
                  <span
                    className={`rsp-ssh-pill ${sshStatus.ok ? 'rsp-ssh-ok' : 'rsp-ssh-bad'}`}
                    title={sshStatus.message}
                  >
                    {sshStatus.ok ? '🔑 SSH authorized' : '⚠ SSH not set up'}
                  </span>
                )}
                <span className="rsp-spacer" />
                <button
                  className="rsp-btn-ghost"
                  onClick={onReassign}
                  title="Drop the pin and re-pick the least-loaded dev-box (generates a new developer id)"
                >
                  ⟳ Re-assign
                </button>
              </div>

              <div className="rsp-stats">
                <Stat label="CPU" value={devbox.resources?.cpu || '—'} usage={devbox.usage?.cpu} />
                <Stat label="Memory" value={devbox.resources?.ram || '—'} usage={devbox.usage?.ram} />
                <Stat label="Storage" value={devbox.resources?.storage || '—'} usage={devbox.usage?.storage} />
              </div>

              <dl className="rsp-devbox-ids">
                <dt>Developer id</dt>
                <dd><code>{devbox.id}</code></dd>
                <dt>Workspace</dt>
                <dd>
                  <code>{devbox.remoteDir}</code>
                  {editorOk && (
                    <button
                      className="rsp-btn-ghost"
                      onClick={onOpenEditor}
                      title={`Open ${devbox.remoteDir} on ${devbox.host} in VS Code over Remote-SSH`}
                    >
                      <VSCodeIcon /> Open Remote SSH
                    </button>
                  )}
                </dd>
              </dl>

              <div className="rsp-sync">
                {sync?.error ? (
                  <span className="rsp-sync-pill rsp-sync-unknown" title={sync.error}>? check failed</span>
                ) : (
                  <span
                    className={`rsp-sync-pill ${sync?.needsDeploy ? 'rsp-sync-stale' : 'rsp-sync-fresh'}`}
                    title={sync?.deployReason || 'workspace not checked yet'}
                  >
                    {sync?.needsDeploy ? '● local changes' : '✓ in sync'}
                  </span>
                )}

                <span className="rsp-sync-text">
                  {sync?.deployedAt ? (
                    <>
                      last deployed{' '}
                      <b title={timeExact(sync.deployedAt)}>{timeAgo(sync.deployedAt)}</b>
                    </>
                  ) : (
                    'never deployed to this dev-box'
                  )}
                </span>

                <code
                  className="rsp-sync-hash"
                  title={`local  ${sync?.workspaceHash || '—'}\ndeployed ${sync?.storedWorkspaceHash || '—'}`}
                >
                  local {shortHash(sync?.workspaceHash)} · deployed {shortHash(sync?.storedWorkspaceHash)}
                </code>

                <span className="rsp-spacer" />
                {sync?.checkedAt && !syncing && (
                  <span className="rsp-sync-checked" title={timeExact(sync.checkedAt)}>
                    checked {timeAgo(sync.checkedAt)}
                  </span>
                )}
                <button
                  className="rsp-btn-ghost"
                  onClick={refreshSync}
                  disabled={syncing}
                  title="Re-fingerprint the workspace and re-read the last deploy time"
                >
                  {syncing ? '◌ Checking…' : '⟳ Refresh'}
                </button>
              </div>
            </div>
          )}
        </div>
      )}

      {steps && (
        <ol className="rsp-steps">
          {steps.map(s => (
            <li key={s.key} className={`rsp-step rsp-step-${s.state}`}>
              <span className="rsp-step-icon">{STEP_ICON[s.state]}</span>
              <span className="rsp-step-text">
                <span className="rsp-step-label">{s.label}</span>
                {s.detail && <span className="rsp-step-detail">{s.detail}</span>}
              </span>
            </li>
          ))}
        </ol>
      )}

      <div className="rsp-meta">
        {/* Endpoint/port readouts live in Tools → Service Ports. */}
        {state.status && <div className="rsp-status">{state.status}</div>}
        {state.error && <div className="rsp-error">{state.error}</div>}
        {sshStatus && !isLocalhost && !sshStatus.ok && (
          <div className="rsp-error" style={{ whiteSpace: 'pre-wrap' }}>
                SSH not set up. Run this command in your terminal:{'\n\n'}
                <code style={{ background: '#333', padding: '4px 8px', borderRadius: '4px', userSelect: 'all' }}>
                  {sshStatus.message?.match(/ssh-copy-id.*/)?.[0] || sshStatus.message}
                </code>
                {'\n\n'}Then select the machine again.
              </div>
        )}
      </div>

      <div className="rsp-terminals">
        {state.sshCopySession && !sshStatus?.ok && (
          <div className="rsp-terminal">
            <div className="rsp-term-title">
              authorizing SSH key on {target.user}@{target.host} — type the dev-box password once
            </div>
            <Terminal
              baseUrl={LOCAL_API_BASE}
              pathPrefix="/api/remote"
              attachSessionId={state.sshCopySession}
            />
          </div>
        )}
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
            Pick where to run above, then press ▶ — deploy and cabal clean run
            automatically whenever they are needed.
          </div>
        )}
      </div>

      {logsOpen && (
        <RemoteLogsModal
          target={target}
          isLocalhost={isLocalhost}
          onClose={() => setLogsOpen(false)}
        />
      )}
    </div>
  );
};
