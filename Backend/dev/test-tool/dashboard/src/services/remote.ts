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
// straight off the stack host's data/ports.json (over SSH for a devbox), so
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
  /** Which address answered: the machine's own 'local' one, or its 'vpn' one. */
  route?: 'local' | 'vpn' | '';
  localIp?: string;
  vpnHost?: string;
  localSubnet?: string;
  vpnSubnet?: string;
  /** Same machine, new IP in discovery — local records were re-pointed. */
  ipUpdated?: boolean;
  previousHost?: string;
  /** Pinned machine answered on neither address, so it was re-assigned. */
  lostRoute?: boolean;
  /** http://<host>:8888/<id> — this workspace through the dev-box front desk;
   *  stays the same when the workspace's own ports change. */
  workspaceUrl?: string;
  /** Whether the dev-box front desk answered its health check. */
  frontdeskOk?: boolean;
  /** Human-readable explanation of whichever recovery path was taken. */
  message?: string;
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
  enabled?: boolean;
  reason?: string;
  watchers?: number;
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

/** Arm the auto-deploy watcher for a dev-box target, or disarm it for Local. */
export const setAutoDeploy = (
  arg: { enabled: false; reason?: string } | (RemoteTarget & { enabled?: true }),
): Promise<AutoDeployStatus> => json('/api/remote/auto-deploy', arg);

export const resolveDevbox = (forceNew = false): Promise<DevboxAssignment> =>
  json(`/api/devbox/resolve${forceNew ? '?new=1' : ''}`);

// ── Workspace data (config-sync) — local-api forwards these to the workspace's
// test-context-api through the dev-box front desk (ws = Workspace URL).

export type DataSourceName = 'master' | 'prod' | 'prod_international';

/** One version DB Manager published for a source (<source>_to_local/metadata.json). */
export interface PublishedVersion {
  version: number;
  status?: 'verified' | 'not_verified' | 'not_stable';
  metadata?: string;
  created_at?: string;
  uploaded_by?: string;
}

export interface DataSource {
  name: DataSourceName;
  versions: PublishedVersion[];
  /** Picked by the "latest, but safe" rule; null when nothing qualifies. */
  recommended?: number | null;
}

export interface WorkspaceDataStatus {
  running?: boolean;
  started_at?: number | null;
  error?: string | null;
  /** What the workspace holds; version is null for syncs recorded before it was tracked. */
  last_synced?: { from: string; version?: string | null; finished_at: number } | null;
  log?: string[];
  /** Front desk answered but the stack (its context-api) is not up yet — wait and retry. */
  unreachable?: boolean;
}

const wsQuery = (ws: string) => `ws=${encodeURIComponent(ws)}`;

/** Read straight from S3 by local-api, so it works before the stack is up. */
export const publishedDataSources = (): Promise<{ sources: DataSource[]; errors?: string[] }> =>
  json('/api/devbox/data/sources');

export const workspaceDataStatus = (ws: string): Promise<WorkspaceDataStatus> =>
  json(`/api/devbox/data/status?${wsQuery(ws)}`);

export const workspaceDataSync = (
  ws: string, from: DataSourceName, version: string,
): Promise<{ started?: boolean; error?: string }> =>
  json('/api/devbox/data/sync', { ws, from, version });

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
