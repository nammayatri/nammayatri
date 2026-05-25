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

export interface RegistryResponse {
  devName?: string;
  caddyPort?: number;
  dir?: string;
  error?: string;
}

export type RemoteSessionKind = 'deploy' | 'start' | 'clear-data';

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

export const remoteSyncCaddyPort = (t: RemoteTarget): Promise<RegistryResponse> =>
  json('/api/remote/sync-caddy-port', t);
