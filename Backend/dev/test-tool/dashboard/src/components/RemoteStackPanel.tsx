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
  RemoteTarget,
} from '../services/remote';
import './RemoteStackPanel.css';

interface PanelState {
  deploySession?: string;
  startSession?: string;
  clearSession?: string;
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
  remoteDir: DEFAULT_REMOTE_DIR,
  copyMode: 'rsync',
  command: DEFAULT_COMMAND,
});

export const RemoteStackPanel: React.FC = () => {
  const [target, setTarget] = useState<RemoteTarget>(loadStoredTarget);
  const [state, setState] = useState<PanelState>({});
  const [busy, setBusy] = useState<'deploy' | 'start' | 'stop' | 'clear-data' | null>(null);
  const [override] = useState<string | null>(getContextApiBaseOverride());

  const isLocalhost = !target.host || ['localhost', '127.0.0.1', '::1'].includes(target.host.trim());

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
        setState(prev => ({
          ...prev,
          deploySession: prev.deploySession ?? deploy?.id,
          startSession: prev.startSession ?? start?.id,
          startCols: prev.startCols ?? start?.cols,
          startRows: prev.startRows ?? start?.rows,
          clearSession: prev.clearSession ?? clear?.id,
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

  const update = (k: keyof RemoteTarget, v: string | number) =>
    setTarget(prev => ({ ...prev, [k]: v }));

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
        setState(prev => ({ ...prev, deploySession: res.session, status: 'rsync running…' }));
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
          <span>Host</span>
          <input value={target.host} onChange={e => update('host', e.target.value)} />
        </label>
        <label>
          <span>User</span>
          <input
            value={target.user || ''}
            onChange={e => update('user', e.target.value)}
            placeholder={isLocalhost ? '(not used)' : 'e.g. ubuntu'}
            disabled={isLocalhost}
          />
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
          <span>Identity file</span>
          <input
            value={target.identityFile || ''}
            onChange={e => update('identityFile', e.target.value)}
            placeholder="~/.ssh/id_ed25519 (optional)"
            disabled={isLocalhost}
          />
        </label>
        <label>
          <span>Remote dir</span>
          <input
            value={target.remoteDir || DEFAULT_REMOTE_DIR}
            onChange={e => update('remoteDir', e.target.value)}
            disabled={isLocalhost}
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
        <button onClick={onDeploy} disabled={!!busy}>
          {busy === 'deploy' ? 'Deploying…' : 'Deploy (rsync)'}
        </button>
        <button onClick={onClearData} disabled={!!busy} title="Wipe runtime data under <repo>/data (postgres, kafka, metabase, …) using `, clear-data`.">
          {busy === 'clear-data' ? 'Clearing…' : 'Clear data'}
        </button>
        <button onClick={onStart} disabled={!!busy}>
          {busy === 'start' ? 'Starting…' : 'Start mobility-stack-dev'}
        </button>
        <button onClick={onStop} disabled={!state.startSession || busy === 'stop'}>
          {busy === 'stop' ? 'Stopping…' : 'Stop'}
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
        {state.status && <div className="rsp-status">{state.status}</div>}
        {state.error && <div className="rsp-error">{state.error}</div>}
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
        {!state.deploySession && !state.startSession && !state.clearSession && (
          <div className="rsp-empty">
            Configure a target above, then click Deploy and Start.
          </div>
        )}
      </div>
    </div>
  );
};
