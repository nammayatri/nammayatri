import { LOCAL_API_BASE } from '../config';

export interface RemoteTarget {
  host: string;
  user?: string;
  port?: number;
  identityFile?: string;
  remoteDir?: string;
  copyMode?: 'rsync' | 'skip';
  command?: string;
  devName?: string;
  cols?: number;
  rows?: number;
}

export interface RemoteSessionResponse {
  session?: string;
  error?: string;
  skipped?: boolean;
  cols?: number;
  rows?: number;
}

// Resolved port map of the stack this checkout points at. local-api reads it
// straight off the stack host's .devbox-ports.json (over SSH for a devbox), so
// this is the only place ports come from — nothing is mirrored locally.
export interface DevboxPortsResponse {
  source?: string;
  host?: string;
  devKey?: string;
  dir?: string;
  ports?: Record<string, number>;
  /** Services reverse-proxied by caddy at /<name>/* — scraped from the Caddyfile. */
  caddyRoutes?: string[];
  caddyPort?: number | null;
  contextApiPort?: number | null;
  error?: string;
}

export type RemoteSessionKind = 'deploy' | 'start' | 'clear-data' | 'cabal-clean';

export interface DevboxAssignment {
  id?: string;
  machine?: string;
  host?: string;
  sshUser?: string;
  port?: number;
  remoteDir?: string;
  copyMode?: 'rsync' | 'skip';
  resources?: { cpu?: string; ram?: string; storage?: string };
  usage?: { cpu?: string; ram?: string; storage?: string };
  created?: boolean;
  repinned?: boolean;
  error?: string;
}

export interface RemoteStatus {
  id?: string;
  kind?: RemoteSessionKind;
  host?: string;
  running?: boolean;
  exitCode?: number | null;
  startedAt?: number;
  finishedAt?: number | null;
  lastLines?: string[];
  cols?: number;
  rows?: number;
  error?: string;
}

const json = async (path: string, body?: unknown): Promise<any> => {
  const res = await fetch(`${LOCAL_API_BASE}${path}`, {
    method: body ? 'POST' : 'GET',
    headers: body ? { 'Content-Type': 'application/json' } : undefined,
    body: body ? JSON.stringify(body) : undefined,
  });
  return res.json();
};

export const remoteDeploy = (t: RemoteTarget): Promise<RemoteSessionResponse> =>
  json('/api/remote/deploy', t);

export const remoteStart = (t: RemoteTarget): Promise<RemoteSessionResponse> =>
  json('/api/remote/start', t);

export const remoteClearData = (t: RemoteTarget): Promise<RemoteSessionResponse> =>
  json('/api/remote/clear-data', t);

export const remoteStop = (session: string): Promise<{ stopped?: boolean; error?: string }> =>
  json('/api/remote/stop', { session });

export const remoteStatus = (session: string): Promise<RemoteStatus> =>
  fetch(`${LOCAL_API_BASE}/api/remote/status?session=${encodeURIComponent(session)}`).then(r => r.json());

export interface RemoteSessionSummary {
  id: string;
  kind: RemoteSessionKind;
  host: string;
  running: boolean;
  exitCode: number | null;
  startedAt: number;
  cols?: number;
  rows?: number;
}

export const remoteSessions = (): Promise<RemoteSessionSummary[]> =>
  fetch(`${LOCAL_API_BASE}/api/remote/sessions`).then(r => r.json());

export const fetchDevboxPorts = (
  opts: { host?: string; refresh?: boolean } = {},
): Promise<DevboxPortsResponse> => {
  const qs = new URLSearchParams();
  if (opts.host) qs.set('host', opts.host);
  if (opts.refresh) qs.set('refresh', '1');
  const q = qs.toString();
  return fetch(`${LOCAL_API_BASE}/api/devbox/ports${q ? `?${q}` : ''}`, { cache: 'no-store' })
    .then(r => r.json());
};

export const remoteCabalClean = (t: RemoteTarget): Promise<RemoteSessionResponse> =>
  json('/api/remote/cabal-clean', t);

export interface PreflightResponse {
  key?: string;
  needsDeploy?: boolean;
  deployReason?: string;
  needsCabalClean?: boolean;
  cabalCleanReason?: string;
  gitHead?: string;
  storedGitHead?: string;
  workspaceHash?: string;
  storedWorkspaceHash?: string;
  deployedAt?: number;
  startedAt?: number;
  checkedAt?: number;
  autoDeploy?: AutoDeployStatus;
  error?: string;
}

export interface AutoDeployStatus {
  busy?: boolean;
  pollSeconds?: number;
  lastCheckAt?: number | null;
  lastDeployAt?: number | null;
  lastResult?: string | null;
  lastSession?: string | null;
}

export const remotePreflight = (t: RemoteTarget): Promise<PreflightResponse> =>
  json('/api/remote/preflight', t);

export const remoteMark = (t: RemoteTarget, stage: 'deploy' | 'start'): Promise<{ error?: string }> =>
  json('/api/remote/mark', { ...t, stage });

export const resolveDevbox = (forceNew = false): Promise<DevboxAssignment> =>
  json(`/api/devbox/resolve${forceNew ? '?new=1' : ''}`);

export interface LogListResponse { files?: string[]; error?: string }
export interface LogTailResponse { file?: string; content?: string; truncated?: boolean; error?: string }
export interface LogClearResponse { cleared?: boolean; file?: string; error?: string }

export const remoteLogList = (t: RemoteTarget): Promise<LogListResponse> =>
  json('/api/remote/logs', t);

export const remoteLogTail = (t: RemoteTarget & { file: string; lines?: number; full?: boolean }): Promise<LogTailResponse> =>
  json('/api/remote/log-tail', t);

export const remoteLogClear = (t: RemoteTarget & { file: string }): Promise<LogClearResponse> =>
  json('/api/remote/log-clear', t);

export interface MachineInfo {
  name: string;
  role: 'base' | 'worker';
  localIp: string;
  awsIp: string;
  bestIp: string;
  user: string;
  type?: string;
  resources: { cpu?: string; ram?: string; storage?: string };
}

export interface MachinesResponse {
  machines: MachineInfo[];
  myIps: string[];
  error?: string;
}

export const fetchMachines = (): Promise<MachinesResponse> =>
  fetch(`${LOCAL_API_BASE}/api/remote/machines`).then(r => r.json());

export const setupSsh = (host: string, user: string, port?: number): Promise<{
  status?: string; message?: string; command?: string; publicKey?: string;
  viaRelay?: string; error?: string;
}> => json('/api/remote/setup-ssh', { host, user, port: port || 22 });

// Interactive one-time key install: runs ssh-copy-id in a PTY the panel attaches
// its terminal to, so the devbox password is typed inside the dashboard.
export const remoteSshCopyId = (t: RemoteTarget): Promise<RemoteSessionResponse> =>
  json('/api/remote/ssh-copy-id', t);

export const openRemoteEditor = (
  t: RemoteTarget,
): Promise<{ opened?: string; error?: string }> =>
  json('/api/remote/open-editor', t);

// `code` on PATH? The panel only offers the open-in-editor button when it is.
export const editorAvailable = (): Promise<{
  available?: boolean; cli?: string; path?: string; error?: string;
}> => fetch(`${LOCAL_API_BASE}/api/remote/editor-available`).then(r => r.json());
