import React, { useEffect, useRef, useState } from 'react';
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
  setAutoDeploy,
  publishedDataSources,
  workspaceDataStatus,
  workspaceDataSync,
  devboxCcStatus,
  devboxCcStart,
  devboxCcStop,
  DevboxCcStatus,
  RemoteTarget,
  DevboxAssignment,
  PreflightResponse,
  DataSource,
  DataSourceName,
  WorkspaceDataStatus,
} from '../services/remote';
import { RemoteLogsModal } from './RemoteLogsModal';
import { showConfirm } from './Dialogs';
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
// Local runs take the fixed ports.nix map (nothing else is competing for them,
// and no registry/Caddyfile is generated). A dev-box is shared, so its stack
// resolves free ports at startup and fronts them with caddy.
const CMD_FIXED_PORTS = 'cd Backend && nix develop .#backend -c , run-mobility-stack-dev';
const CMD_AVAILABLE_PORTS = 'cd Backend && nix develop .#backend -c , run-mobility-stack-dev-on-available-ports';
const FORM_STORAGE_KEY = 'ny.remoteStack.target';
const MODE_STORAGE_KEY = 'ny.remoteStack.mode';
const DATA_STORAGE_KEY = 'ny.remoteStack.data';

// Data loaded into a dev-box workspace on Start. version '' = "latest safe":
// whatever local-api recommends for that source at Start time.
interface DataPick { from: DataSourceName; version: string }

const loadStoredDataPick = (): DataPick => {
  try {
    const p = JSON.parse(window.localStorage.getItem(DATA_STORAGE_KEY) || 'null');
    if (p && typeof p.from === 'string') {
      return { from: p.from, version: typeof p.version === 'string' ? p.version : '' };
    }
  } catch { /* ignore */ }
  return { from: 'master', version: '' };
};

const VERSION_STATUS_TEXT: Record<string, string> = {
  verified: 'verified',
  not_verified: 'not verified',
  not_stable: 'not stable',
};

const shortDate = (iso?: string): string =>
  iso ? new Date(iso).toLocaleDateString(undefined, { day: 'numeric', month: 'short' }) : '';

const sleep = (ms: number) => new Promise<void>(resolve => window.setTimeout(resolve, ms));

type RunMode = 'local' | 'devbox';

// ── Start pipeline ──
// Deploy and Cabal Clean are not manual buttons any more: Start runs them
// automatically when local-api's pre-flight says they are needed (workspace
// files changed since the last rsync / the last run's commit is no longer in
// this branch's history). These steps are what the progress strip renders.
type StepKey = 'check' | 'deploy' | 'cabal-clean' | 'start' | 'data';
type StepState = 'pending' | 'running' | 'done' | 'skipped' | 'failed';

interface Step {
  key: StepKey;
  label: string;
  state: StepState;
  detail?: string;
}

const stepsFor = (localRun: boolean): Step[] => [
  { key: 'check', label: 'Check workspace & git', state: 'pending' },
  // Local runs in place — nothing to rsync, so the stage is not even shown.
  // The git-HEAD check that decides a cabal clean still applies.
  ...(localRun ? [] : [{ key: 'deploy' as StepKey, label: 'Deploy (rsync)', state: 'pending' as StepState }]),
  { key: 'cabal-clean', label: 'Cabal clean', state: 'pending' },
  { key: 'start', label: localRun ? 'Start mobility-stack-dev (fixed ports)' : 'Start mobility-stack-dev (available ports)', state: 'pending' },
  // Loads the picked source/version once the stack's context-api answers.
  { key: 'data', label: 'Load data', state: 'pending' },
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
  command: CMD_FIXED_PORTS,
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
  const [urlCopied, setUrlCopied] = useState<string | null>(null);

  // Same clipboard + textarea fallback as ServicePortsModal's CopyButton.
  const copyWorkspaceUrl = async (url: string) => {
    try {
      await navigator.clipboard.writeText(url);
    } catch {
      const ta = document.createElement('textarea');
      ta.value = url;
      document.body.appendChild(ta);
      ta.select();
      document.execCommand('copy');
      document.body.removeChild(ta);
    }
    setUrlCopied(url);
    window.setTimeout(() => setUrlCopied(null), 1200);
  };

  // ── Log viewer ── (contents live in RemoteLogsModal)
  const [logsOpen, setLogsOpen] = useState(false);

  // ── Sync state ── last deploy + whether the workspace has drifted since.
  // Same pre-flight the Start pipeline runs, just read for display.
  const [sync, setSync] = useState<PreflightResponse | null>(null);
  const [syncing, setSyncing] = useState(false);

  // ── Workspace data ── which published config Start loads into a dev-box
  // workspace, and what the workspace holds now. The version list comes from
  // DB Manager's metadata.json (via local-api); the workspace is reached
  // through the dev-box front desk, so this needs devbox.frontdeskOk.
  const [dataSources, setDataSources] = useState<DataSource[]>([]);
  const [dataPick, setDataPick] = useState<DataPick>(loadStoredDataPick);
  const [dataStatus, setDataStatus] = useState<WorkspaceDataStatus | null>(null);
  const [dataBusy, setDataBusy] = useState<string | null>(null);  // Re-sync progress
  const [dataNote, setDataNote] = useState<string | null>(null);  // last Re-sync failure
  const dataAbort = useRef(false);                                 // set by Stop

  // ── Control centre on the dev-box ── this checkout's control-center/ (rsynced
  // with the backend) run next to the workspace's rider-app / driver-app.
  const [cc, setCc] = useState<DevboxCcStatus | null>(null);
  const [ccBusy, setCcBusy] = useState<string | null>(null);  // Launch / Stop progress
  const [ccLogOpen, setCcLogOpen] = useState(false);
  const ccActive = cc?.state === 'starting' || cc?.state === 'running';

  const refreshCc = async () => {
    if (mode !== 'devbox' || !devbox) { setCc(null); return; }
    try {
      setCc(await devboxCcStatus());
    } catch { /* local-api down — keep the last reading */ }
  };

  useEffect(() => {
    refreshCc();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [mode, devbox?.id]);

  // While it is coming up (first run installs Node + npm packages), follow it.
  useEffect(() => {
    if (cc?.state !== 'starting') return;
    const t = window.setInterval(refreshCc, 3000);
    return () => window.clearInterval(t);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [cc?.state]);

  const ws = mode === 'devbox' && devbox?.frontdeskOk ? devbox.workspaceUrl : undefined;
  const pickedSource = dataSources.find(s => s.name === dataPick.from);
  const pickedVersion = dataPick.version
    || (pickedSource?.recommended != null ? `v${pickedSource.recommended}` : '');
  const held = dataStatus?.last_synced;
  // The control centre needs rider-app / driver-app answering and the seeded
  // login user, i.e. data loaded. Without a front desk we cannot read that, so
  // a started stack has to do.
  const ccReady = !!state.startSession
    && (!ws || (!!held && !dataStatus?.unreachable && !dataStatus?.running && !dataBusy));
  const heldStatus = dataSources
    .find(s => s.name === held?.from)?.versions
    .find(v => `v${v.version}` === held?.version)?.status;

  useEffect(() => {
    try { window.localStorage.setItem(DATA_STORAGE_KEY, JSON.stringify(dataPick)); } catch { /* ignore */ }
  }, [dataPick]);

  useEffect(() => {
    if (mode !== 'devbox') return;
    publishedDataSources()
      .then(r => setDataSources(r.sources || []))
      .catch(() => setDataSources([]));
  }, [mode]);

  // A remembered pick that is no longer published (e.g. prod hidden, a version
  // removed) falls back to master / the first source, at "latest safe".
  useEffect(() => {
    if (dataSources.length === 0) return;
    const src = dataSources.find(s => s.name === dataPick.from);
    if (!src) {
      const fallback = dataSources.find(s => s.name === 'master') || dataSources[0];
      setDataPick({ from: fallback.name, version: '' });
    } else if (dataPick.version && !src.versions.some(v => `v${v.version}` === dataPick.version)) {
      setDataPick({ from: src.name, version: '' });
    }
  }, [dataSources, dataPick]);

  const refreshDataStatus = async () => {
    if (!ws) { setDataStatus(null); return; }
    try {
      setDataStatus(await workspaceDataStatus(ws));
    } catch {
      setDataStatus(null);
    }
  };

  useEffect(() => {
    refreshDataStatus();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [ws]);

  const statusOrUnreachable = (wsUrl: string): Promise<WorkspaceDataStatus> =>
    workspaceDataStatus(wsUrl).catch(() => ({ unreachable: true }));

  // Load from/version into the workspace: wait for its context-api, skip when it
  // already holds exactly that (unless forced), otherwise import and wait for that
  // run to finish. Never throws; `report` gets progress text for the caller.
  const syncWorkspaceData = async (
    wsUrl: string, from: DataSourceName, version: string, force: boolean,
    report: (detail: string) => void,
  ): Promise<{ state: 'done' | 'skipped' | 'failed'; detail: string }> => {
    const label = `${from} ${version}`;
    dataAbort.current = false;

    report('waiting for the stack…');
    const upBy = Date.now() + 30 * 60_000;  // a first build can take a while
    let st = await statusOrUnreachable(wsUrl);
    while (st.unreachable && Date.now() < upBy && !dataAbort.current) {
      await sleep(5000);
      st = await statusOrUnreachable(wsUrl);
    }
    if (dataAbort.current) return { state: 'skipped', detail: 'stack stopped' };
    if (st.unreachable) return { state: 'failed', detail: 'stack did not come up within 30 min' };

    if (!force && !st.running && st.last_synced?.from === from && st.last_synced?.version === version) {
      setDataStatus(st);
      return { state: 'skipped', detail: `already ${label}` };
    }

    report(`syncing ${label}…`);
    const before = st.started_at ?? null;
    const r = await workspaceDataSync(wsUrl, from, version).catch(e => ({ error: String(e) }));
    if (r.error) return { state: 'failed', detail: r.error };

    // Status may still show the previous run for a moment: wait for a new
    // started_at, then for it to stop running.
    const doneBy = Date.now() + 30 * 60_000;
    do {
      await sleep(3000);
      st = await statusOrUnreachable(wsUrl);
    } while (!dataAbort.current && Date.now() < doneBy
             && (st.unreachable || st.running || (st.started_at ?? null) === before));
    setDataStatus(st);
    if (dataAbort.current) return { state: 'skipped', detail: 'stack stopped' };
    if (st.running || st.unreachable) return { state: 'failed', detail: 'sync did not finish within 30 min' };
    return st.error ? { state: 'failed', detail: st.error } : { state: 'done', detail: label };
  };

  const onResync = async () => {
    if (!ws || !pickedSource || !pickedVersion) return;
    const ok = await showConfirm(
      `Load ${pickedSource.name} ${pickedVersion} into this workspace?\n\n`
      + 'Test users are re-seeded and rider-app, driver-app and mock-registry restart.',
      { title: 'Re-sync data', confirmLabel: `Load ${pickedSource.name} ${pickedVersion}`, variant: 'info' },
    );
    if (!ok) return;
    setDataNote(null);
    setDataBusy('starting…');
    const out = await syncWorkspaceData(ws, pickedSource.name, pickedVersion, true, setDataBusy);
    setDataBusy(null);
    if (out.state === 'failed') setDataNote(out.detail);
  };

  const isLocalhost = !target.host || ['localhost', '127.0.0.1', '::1'].includes(target.host.trim());
  const hasDevName = isLocalhost || !!target.devName?.trim();
  // In Dev-Box mode nothing may run until an assignment is resolved —
  // otherwise a stale localStorage target could be deployed to.
  const devboxNotReady = mode === 'devbox' && (resolving || !devbox);

  const handleModeChange = (next: RunMode) => {
    if (next === mode) return;
    setMode(next);
    try {
      window.localStorage.setItem(MODE_STORAGE_KEY, next);
      window.localStorage.removeItem('ny.contextApiBase');
      window.sessionStorage.clear();
    } catch { /* ignore */ }
    window.location.reload();
  };

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
    refreshDataStatus();  // the same ⟳ also re-reads what data the workspace holds
    refreshCc();          // … and the control centre's state
  };

  useEffect(() => {
    refreshSync();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [target.host, target.user, target.devName, isLocalhost]);

  // Apply a resolved devbox assignment to the deploy target and verify SSH.
  const applyAssignment = async (a: DevboxAssignment) => {
    setDevbox(a);
    if (a.host && a.sshUser && a.id) {
      setAutoDeploy({
        host: a.host, user: a.sshUser, port: a.port || 22,
        devName: a.id, copyMode: 'rsync',
      }).catch(() => { /* local-api down */ });
    }
    setTarget({
      host: a.host || '',
      user: a.sshUser || '',
      port: a.port || 22,
      identityFile: '',
      devName: a.id || '',
      remoteDir: a.remoteDir || DEFAULT_REMOTE_DIR,
      copyMode: 'rsync',
      command: CMD_AVAILABLE_PORTS,
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
      // Nothing to rsync in Local mode — stop the background watcher so a pin
      // left from an earlier dev-box session cannot keep syncing.
      setAutoDeploy({ enabled: false, reason: 'Local mode selected' }).catch(() => { /* local-api down */ });
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
    setSteps(stepsFor(isLocalhost));
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

      if (pre.needsDeploy && !isLocalhost) {
        patchStep('deploy', { state: 'running', detail: pre.deployReason });
        const res = await remoteDeploy(target);
        if (res.error || !res.session) { fail('deploy', res.error || 'deploy failed'); return; }
        setState(prev => ({ ...prev, deploySession: res.session, status: 'rsync running…' }));
        const code = await waitForSession(res.session);
        if (code !== 0) { fail('deploy', `rsync exited (code ${code})`); return; }
        patchStep('deploy', { state: 'done', detail: 'workspace synced' });
      } else if (!isLocalhost) {
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
      const command = mode === 'devbox' ? CMD_AVAILABLE_PORTS : CMD_FIXED_PORTS;
      const res = await remoteStart({ ...target, command });
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

      // Load data: the stack is only launched at this point; the data step
      // waits for it, and its outcome never stops the stack.
      if (isLocalhost) {
        patchStep('data', { state: 'skipped', detail: 'local run: use Tools → Sync Data' });
      } else if (!ws) {
        patchStep('data', { state: 'skipped', detail: 'front desk not installed on this dev-box' });
      } else if (!pickedSource || !pickedVersion) {
        patchStep('data', { state: 'skipped', detail: 'no published data version found' });
      } else {
        patchStep('data', { state: 'running', detail: 'waiting for the stack…' });
        const out = await syncWorkspaceData(ws, pickedSource.name, pickedVersion, false,
          detail => patchStep('data', { state: 'running', detail }));
        patchStep('data', { state: out.state, detail: out.detail });
      }
    } catch (e) {
      const msg = e instanceof Error ? e.message : String(e);
      setState(prev => ({ ...prev, error: `Start failed: ${msg}`, status: undefined }));
      setSteps(prev => (prev ? prev.map(s => (s.state === 'running' ? { ...s, state: 'failed', detail: msg } : s)) : prev));
    } finally {
      setBusy(null);
      refreshSync();
    }
  };

  // Launch = make sure the dev-box has this checkout's latest files (the same
  // pre-flight + rsync as Start), then start control-center there. Copying first
  // means what runs is what you just edited, not the previous deploy.
  const onCcLaunch = async () => {
    setCcLogOpen(false);
    try {
      setCcBusy('checking your code…');
      const pre = await remotePreflight(target);
      if (pre.error) { setCc({ state: 'unknown', error: pre.error }); return; }
      if (pre.needsDeploy) {
        setCcBusy('deploying your changes…');
        const res = await remoteDeploy(target);
        if (res.error || !res.session) { setCc({ state: 'unknown', error: res.error || 'deploy failed' }); return; }
        const code = await waitForSession(res.session);
        if (code !== 0) { setCc({ state: 'unknown', error: `rsync exited (code ${code})` }); return; }
      }
      setCcBusy('starting…');
      setCc(await devboxCcStart());
    } catch (e) {
      setCc({ state: 'unknown', error: e instanceof Error ? e.message : String(e) });
    } finally {
      setCcBusy(null);
      refreshSync();
    }
  };

  const onCcStop = async () => {
    setCcBusy('stopping…');
    try {
      setCc(await devboxCcStop());
    } catch (e) {
      setCc({ state: 'unknown', error: e instanceof Error ? e.message : String(e) });
    } finally {
      setCcBusy(null);
    }
  };

  // Restart runs exactly the latest deployed files (Vite only hot-reloads some).
  const onCcRestart = async () => {
    await onCcStop();
    await onCcLaunch();
  };

  const onStop = async () => {
    if (!state.startSession) return;
    dataAbort.current = true;  // ends a "Load data" wait still in progress
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
          <select value={mode} onChange={e => handleModeChange(e.target.value as RunMode)}>
            <option value="local">Local (this machine)</option>
            <option value="devbox">Dev-Box (auto-assigned)</option>
          </select>
        </label>

        {mode === 'devbox' && (
          <>
            <label className="rsp-data-src">
              <span>Data</span>
              <select
                id="rsp-data-src"
                value={pickedSource ? dataPick.from : ''}
                onChange={e => setDataPick({ from: e.target.value as DataSourceName, version: '' })}
                disabled={!!busy || !!dataBusy || dataSources.length === 0}
                title="Config data loaded into the workspace on Start (published by DB Manager)"
              >
                {dataSources.length === 0 && <option value="">no published data</option>}
                {dataSources.map(s => <option key={s.name} value={s.name}>{s.name}</option>)}
              </select>
            </label>
            <label className="rsp-data-ver">
              <span>Version</span>
              <select
                id="rsp-data-ver"
                value={dataPick.version}
                onChange={e => setDataPick(p => ({ ...p, version: e.target.value }))}
                disabled={!!busy || !!dataBusy || !pickedSource}
                title="“latest safe” = newest verified version, else the newest one not marked not stable"
              >
                <option value="">
                  {pickedSource?.recommended != null
                    ? `latest safe (v${pickedSource.recommended})`
                    : 'no usable version'}
                </option>
                {pickedSource?.versions.map(v => (
                  <option key={v.version} value={`v${v.version}`}>
                    v{v.version} · {VERSION_STATUS_TEXT[v.status || 'not_verified'] || v.status}
                    {v.created_at ? ` · ${shortDate(v.created_at)}` : ''}
                    {v.uploaded_by ? ` · ${v.uploaded_by}` : ''}
                  </option>
                ))}
              </select>
            </label>
          </>
        )}

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
            disabled={!!busy || devboxNotReady || !hasDevName || !!state.deploySession || !!state.startSession || !!state.cabalCleanSession || ccActive}
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
            <div className="rsp-devbox-error" role="alert">
              <span className="rsp-devbox-error-icon" aria-hidden="true">⚠</span>
              <span className="rsp-devbox-error-text">{devboxErr}</span>
              <button
                className="rsp-btn-retry"
                onClick={() => resolveAssignment(false)}
                disabled={resolving}
                title="Ask the base station again and re-resolve the assignment"
              >
                {resolving ? '◌ Retrying…' : '⟳ Retry'}
              </button>
            </div>
          )}
          {devbox && !resolving && (
            <div className="rsp-devbox-card">
              <div className="rsp-devbox-head">
                <span className="rsp-devbox-machine">{devbox.machine}</span>
                <code
                  className="rsp-devbox-host"
                  title={`local ${devbox.localIp || 'n/a'}${devbox.localSubnet ? ` — same subnet as ${devbox.localSubnet}` : ' — different subnet'}\nvpn   ${devbox.vpnHost || 'n/a'}${devbox.vpnSubnet ? ` — same subnet as ${devbox.vpnSubnet}` : ' — different subnet'}`}
                >
                  {devbox.host}
                </code>
                {devbox.route && (
                  <span className="rsp-route-pill" title={devbox.route === 'local'
                    ? `reached on its local address${devbox.localSubnet ? ` — same subnet ${devbox.localSubnet}` : ' (routed, not same subnet)'}`
                    : `reached on its VPN address${devbox.vpnSubnet ? ` — same subnet ${devbox.vpnSubnet}` : ''}; its local address did not answer`}>
                    via {devbox.route.toUpperCase()}
                  </span>
                )}
                {devbox.created && <span className="rsp-tag">new assignment</span>}
                {devbox.repinned && <span className="rsp-tag">re-assigned (old machine left the fleet)</span>}
                {devbox.ipUpdated && (
                  <span className="rsp-tag" title={devbox.message}>
                    IP changed{devbox.previousHost ? ` from ${devbox.previousHost}` : ''}
                  </span>
                )}
                {devbox.lostRoute && (
                  <span className="rsp-tag rsp-tag-warn" title={devbox.message}>⚠ old box lost route</span>
                )}
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

              {devbox.message && (
                <div className={`rsp-devbox-note${devbox.lostRoute ? ' rsp-devbox-note-warn' : ''}`}>
                  {devbox.message}
                </div>
              )}

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
                <dt>Workspace URL</dt>
                <dd className="rsp-ws-url">
                  {devbox.frontdeskOk && devbox.workspaceUrl ? (
                    <>
                      <code>{devbox.workspaceUrl}</code>
                      <button
                        className="rsp-btn-ghost"
                        onClick={() => copyWorkspaceUrl(devbox.workspaceUrl!)}
                        title="Copy this workspace's stable URL (dev-box front desk) — it stays the same when ports change"
                      >
                        {urlCopied === devbox.workspaceUrl ? '✓ Copied' : '⧉ Copy'}
                      </button>
                      <a
                        className="rsp-btn-ghost"
                        href={`${devbox.workspaceUrl}/rider-app/v2/`}
                        target="_blank"
                        rel="noreferrer"
                        title="rider-app health through the front desk"
                      >
                        rider-app ↗
                      </a>
                      <a
                        className="rsp-btn-ghost"
                        href={`${devbox.workspaceUrl}/dynamic-offer-driver-app/ui/`}
                        target="_blank"
                        rel="noreferrer"
                        title="driver-app health through the front desk"
                      >
                        driver-app ↗
                      </a>
                    </>
                  ) : (
                    <span
                      className="rsp-sync-checked"
                      title="Install it on the dev-box: python3 scripts/daemonset/worker/devbox/frontdesk/setup.py (nix-the-builder)"
                    >
                      front desk not installed on this dev-box
                    </span>
                  )}
                </dd>
                {devbox.frontdeskOk && devbox.publicUrl && (
                  <>
                    <dt>Public URL</dt>
                    <dd className="rsp-ws-url">
                      <code>{devbox.publicUrl}</code>
                      <button
                        className="rsp-btn-ghost"
                        onClick={() => copyWorkspaceUrl(devbox.publicUrl!)}
                        title="Copy the URL testers' phones use on any network (hopper gateway) — rider-app and driver-app only"
                      >
                        {urlCopied === devbox.publicUrl ? '✓ Copied' : '⧉ Copy'}
                      </button>
                      <a
                        className="rsp-btn-ghost"
                        href={`${devbox.publicUrl}/rider-app/v2/`}
                        target="_blank"
                        rel="noreferrer"
                        title="rider-app health through hopper's gateway"
                      >
                        rider-app ↗
                      </a>
                      <a
                        className="rsp-btn-ghost"
                        href={`${devbox.publicUrl}/dynamic-offer-driver-app/ui/`}
                        target="_blank"
                        rel="noreferrer"
                        title="driver-app health through hopper's gateway"
                      >
                        driver-app ↗
                      </a>
                    </dd>
                  </>
                )}
                <dt>Data</dt>
                <dd className="rsp-data-row">
                  {!ws ? (
                    <span className="rsp-sync-checked">needs the front desk</span>
                  ) : dataBusy ? (
                    <span className="rsp-sync-checked">◐ {dataBusy}</span>
                  ) : dataStatus?.unreachable ? (
                    <span className="rsp-sync-checked">stack not running</span>
                  ) : !held && dataStatus?.error ? (
                    <span className="rsp-sync-checked" title={dataStatus.error}>status unavailable: {dataStatus.error}</span>
                  ) : held ? (
                    <>
                      <code>{held.from} {held.version || '(version not recorded)'}</code>
                      <span className="rsp-sync-checked" title={timeExact(held.finished_at)}>
                        synced {timeAgo(held.finished_at)}
                      </span>
                      {heldStatus && heldStatus !== 'verified' && (
                        <span className="rsp-tag">{VERSION_STATUS_TEXT[heldStatus] || heldStatus}</span>
                      )}
                    </>
                  ) : (
                    <span className="rsp-sync-checked">no data loaded yet</span>
                  )}
                  {dataNote && <span className="rsp-tag rsp-tag-warn" title={dataNote}>✗ {dataNote}</span>}
                  {ws && (
                    <button
                      className="rsp-btn-ghost"
                      onClick={onResync}
                      disabled={!!busy || !!dataBusy || !pickedVersion || !dataStatus || !!dataStatus.unreachable}
                      title={`Load ${dataPick.from} ${pickedVersion || ''} now: re-seeds test users and restarts rider/driver`}
                    >
                      ⟳ Re-sync
                    </button>
                  )}
                </dd>
                <dt>Control centre</dt>
                <dd className="rsp-cc-row">
                  {ccBusy ? (
                    <span className="rsp-sync-checked">◐ {ccBusy}</span>
                  ) : cc?.state === 'running' ? (
                    <>
                      <a className="rsp-btn-ghost" href={cc.url} target="_blank" rel="noreferrer">Open ↗ {cc.url}</a>
                      <code title="What was launched (recorded at launch)">
                        {cc.branch || '?'} @ {cc.commit || '?'}{cc.dirty ? ' + uncommitted' : ''}
                      </code>
                      <span className="rsp-sync-checked" title={timeExact(cc.startedAt)}>launched {timeAgo(cc.startedAt)}</span>
                      {cc.changedSinceLaunch && (
                        <span className="rsp-tag" title="Files were deployed after launch. Vite reloads most of them; Restart runs exactly the latest.">
                          code changed since launch
                        </span>
                      )}
                      {sync?.needsDeploy && (
                        <span className="rsp-tag" title={sync.deployReason}>local changes not deployed</span>
                      )}
                      {cc.changedSinceLaunch && (
                        <button className="rsp-btn-ghost" onClick={onCcRestart} disabled={!!busy}>⟳ Restart</button>
                      )}
                      <button className="rsp-btn-ghost" onClick={onCcStop}>■ Stop</button>
                      {/* control-center uses crypto.randomUUID, which browsers only allow on
                          https or localhost; a plain-http dev-box address needs a one-time allow. */}
                      <span
                        className="rsp-sync-checked"
                        title={`Chrome: open chrome://flags/#unsafely-treat-insecure-origin-as-secure, add ${cc.url}, Enabled, Relaunch.\nOr: ssh -N -L ${cc.port}:127.0.0.1:${cc.port} ${devbox?.sshUser}@${devbox?.host} and open http://localhost:${cc.port}`}
                      >
                        “crypto.randomUUID is not a function”? allow {cc.url} once in chrome://flags (Insecure origins treated as secure)
                      </span>
                    </>
                  ) : cc?.state === 'starting' ? (
                    <>
                      <span className="rsp-sync-checked">◐ starting… (first run installs Node + npm packages: a few minutes)</span>
                      <button className="rsp-btn-ghost" onClick={onCcStop}>■ Stop</button>
                    </>
                  ) : (
                    <>
                      {cc?.state === 'exited' && <span className="rsp-tag rsp-tag-warn">exited: see log</span>}
                      <button
                        className="rsp-btn-ghost"
                        onClick={onCcLaunch}
                        disabled={!!busy || !ccReady || cc?.checkoutPresent === false}
                        title="Copy your latest control-center edits to the dev-box, then run it next to this workspace's rider-app / driver-app"
                      >
                        ▶ Launch
                      </button>
                      {!state.startSession ? (
                        <span className="rsp-sync-checked">start the stack first</span>
                      ) : !ccReady && (
                        <span className="rsp-sync-checked">waiting for the stack and data…</span>
                      )}
                      {cc?.checkoutPresent === false && (
                        <span className="rsp-sync-checked">no control-center/ in the workspace: keep your checkout at &lt;repo&gt;/control-center</span>
                      )}
                    </>
                  )}
                  {cc?.error && !ccBusy && <span className="rsp-tag rsp-tag-warn" title={cc.error}>✗ {cc.error}</span>}
                  {!!cc?.log?.length && !ccBusy && (
                    <button className="rsp-btn-ghost" onClick={() => setCcLogOpen(o => !o)}>{ccLogOpen ? 'hide log' : 'log'}</button>
                  )}
                </dd>
                {ccLogOpen && !!cc?.log?.length && (
                  <>
                    <dt />
                    <dd className="rsp-cc-logdd"><pre className="rsp-cc-log">{cc.log.join('\n')}</pre></dd>
                  </>
                )}
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
