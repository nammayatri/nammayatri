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
  fetchMachines,
  setupSsh,
  RemoteTarget,
  MachineInfo,
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

const loadStoredTarget = (): RemoteTarget => {
  try {
    const raw = window.localStorage.getItem(FORM_STORAGE_KEY);
    if (raw) return { ...defaultTarget(), ...JSON.parse(raw) };
  } catch {
    /* ignore */
  }
  return defaultTarget();
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
  const [machines, setMachines] = useState<MachineInfo[]>([]);
  const [selectedMachine, setSelectedMachine] = useState<string>('localhost');
  const [sshStatus, setSshStatus] = useState<{ ok: boolean; message?: string } | null>(null);

  const isLocalhost = !target.host || ['localhost', '127.0.0.1', '::1'].includes(target.host.trim());
  const hasDevName = isLocalhost || !!target.devName?.trim();

  // Fetch available machines from ServiceDiscovery API
  useEffect(() => {
    let cancelled = false;
    (async () => {
      try {
        const res = await fetchMachines();
        if (cancelled || !res.machines) return;
        setMachines(res.machines);
        // If current host matches a machine, select it
        const match = res.machines.find(
          m => m.bestIp === target.host || m.localIp === target.host || m.awsIp === target.host
        );
        if (match) {
          setSelectedMachine(match.name);
          // Sync user and host from ServiceDiscovery (overrides stale localStorage values)
          setTarget(prev => ({ ...prev, host: match.bestIp, user: match.user }));
          // Run SSH check for auto-selected remote machine
          try {
            const sshRes = await setupSsh(match.bestIp, match.user, target.port);
            if (!cancelled) {
              setSshStatus(sshRes.status === 'ok'
                ? { ok: true, message: sshRes.message }
                : { ok: false, message: sshRes.message });
            }
          } catch (e) {
            if (!cancelled) setSshStatus({ ok: false, message: `SSH check failed: ${e}` });
          }
        }
      } catch { /* ServiceDiscovery unavailable */ }
    })();
    return () => { cancelled = true; };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  const onMachineSelect = async (value: string) => {
    setSelectedMachine(value);
    setSshStatus(null);
    if (value === 'localhost') {
      setTarget(prev => ({ ...prev, host: 'localhost', user: '' }));
    } else {
      const m = machines.find(m => m.name === value);
      if (m) {
        setTarget(prev => ({ ...prev, host: m.bestIp, user: m.user }));
        try {
          const res = await setupSsh(m.bestIp, m.user, target.port);
          if (res.status === 'ok') {
            setSshStatus({ ok: true, message: res.message });
          } else {
            setSshStatus({ ok: false, message: res.message });
          }
        } catch (e) {
          setSshStatus({ ok: false, message: `SSH check failed: ${e}` });
        }
      }
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

  const update = (k: keyof RemoteTarget, v: string | number) => {
    setTarget(prev => {
      const next = { ...prev, [k]: v };
      // Auto-derive remoteDir from devName.
      if (k === 'devName' && typeof v === 'string') {
        const name = v.trim().toLowerCase();
        next.devName = name;
        next.remoteDir = name ? `/tmp/${name}/nammayatri` : DEFAULT_REMOTE_DIR;
      }
      return next;
    });
  };

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
    } finally {
      setBusy(null);
    }
  };

  const onSyncCaddyPort = async () => {
    if (!target.devName?.trim() || isLocalhost) return;
    try {
      const res = await remoteSyncCaddyPort(target);
      if (res.caddyPort != null) {
        setState(prev => ({ ...prev, caddyPort: res.caddyPort }));
      } else if (res.error) {
        setState(prev => ({ ...prev, error: res.error }));
      }
    } catch (err) {
      const msg = err instanceof Error ? err.message : String(err);
      setState(prev => ({ ...prev, error: `Caddy port sync failed: ${msg}` }));
    }
  };

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
    } finally {
      setBusy(null);
    }
  };

  const onUseRemoteContextApi = () => {
    const host = target.host || 'localhost';
    const url = `http://${host}:7082`;
    setContextApiBase(url); // reloads page
  };

  const onResetContextApi = () => setContextApiBase(null);

  return (
    <div className="remote-stack-panel">
      <div className="rsp-header">
        <h2>Remote Stack</h2>
        <p className="rsp-sub">
          SSH into a host, rsync the repo, then launch <code>, run-mobility-stack-dev</code>
          (backend + test-context-api + mock-server). Streams the PTY back to this panel.
          For <code>localhost</code>, no SSH or rsync is used.
        </p>
      </div>

      <div className="rsp-form">
        <label>
          <span>Machine</span>
          <select value={selectedMachine} onChange={e => onMachineSelect(e.target.value)}>
            <option value="localhost">localhost</option>
            {machines.filter(m => m.role === 'worker' && m.type === 'dev-box').map(m => (
              <option key={m.name} value={m.name}>
                {m.user || m.name} — {m.bestIp} [{m.resources?.cpu}, {m.resources?.ram}]
              </option>
            ))}
          </select>
        </label>
        <label>
          <span>Port</span>
          <input
            type="number"
            value={target.port || 22}
            onChange={e => update('port', Number(e.target.value) || 22)}
            disabled={isLocalhost}
          />
        </label>
        <label>
          <span>Developer name (Full name without spaces and special characters) {!isLocalhost && <span style={{ color: '#e53e3e' }}>*</span>}</span>
          <input
            value={target.devName || ''}
            onChange={e => update('devName', e.target.value)}
            placeholder="e.g. rohitsaini"
            disabled={isLocalhost}
            required={!isLocalhost}
            style={!isLocalhost && !target.devName?.trim() ? { borderColor: '#e53e3e' } : undefined}
          />
          {!isLocalhost && !target.devName?.trim() && (
            <span style={{ color: '#e53e3e', fontSize: '0.8em' }}>Required for remote hosts</span>
          )}
        </label>
        <label>
          <span>Remote dir</span>
          <input
            value={target.remoteDir || DEFAULT_REMOTE_DIR}
            onChange={e => update('remoteDir', e.target.value)}
            disabled={isLocalhost || !!target.devName?.trim()}
            title={target.devName?.trim() ? 'Auto-derived from developer name' : undefined}
          />
        </label>
        <label>
          <span>Copy mode</span>
          <select
            value={target.copyMode || 'rsync'}
            onChange={e => update('copyMode', e.target.value)}
            disabled={isLocalhost}
          >
            <option value="rsync">rsync</option>
            <option value="skip">skip</option>
          </select>
        </label>
      </div>

      <div className="rsp-actions">
        <button onClick={onDeploy} disabled={!!busy || !hasDevName || (!isLocalhost && (!sshStatus || !sshStatus.ok))}>
          {busy === 'deploy' ? 'Deploying…' : 'Deploy (rsync)'}
        </button>
        <button onClick={onClearData} disabled={!!busy || !hasDevName} title="Wipe runtime data under <repo>/data (postgres, kafka, metabase, …) using `, clear-data`.">
          {busy === 'clear-data' ? 'Clearing…' : 'Clear data'}
        </button>
        <button onClick={onStart} disabled={!!busy || !hasDevName}>
          {busy === 'start' ? 'Starting…' : 'Start mobility-stack-dev'}
        </button>
        <button onClick={onStop} disabled={!state.startSession || busy === 'stop' || !hasDevName}>
          {busy === 'stop' ? 'Stopping…' : 'Stop'}
        </button>
        <button
          onClick={onSyncCaddyPort}
          disabled={!state.startSession || !target.devName?.trim() || isLocalhost}
          title="Read the resolved Caddy port from the running stack and store it in the registry"
        >
          Sync Caddy Port
        </button>
        <button
          onClick={onCabalClean}
          disabled={!!busy || !hasDevName}
          title="Run `cabal clean` in the Backend directory to clear stale build artifacts (fixes GHC panics / package-database corruption)"
        >
          {busy === 'cabal-clean' ? 'Cleaning…' : 'Cabal Clean'}
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
    </div>
  );
};
