import { LOCAL_API_BASE } from '../config';

// Backend execution for ny-qa-automation collections (NY/MSIL/YS groups in
// CollectionRunner). These run via Newman on test-local-api because they rely
// on pm.execution.setNextRequest(), which the in-browser postman-runtime.ts
// does not implement. Discovery (suites/environments) goes through the
// regular /api/collections + /api/collection endpoints on test-context-api —
// see context-api/server.py's _scan_qa_collections(). This module only
// covers actually running one.

export interface QaCollectionRef {
  directory: string;
  filename: string;
}

export interface QaRunStartPayload {
  collections: QaCollectionRef[];
  envFile: string;
  concurrency: number;
}

async function asJson<T>(resp: Response): Promise<T> {
  if (!resp.ok) {
    const body = await resp.text().catch(() => '');
    throw new Error(`${resp.status} ${resp.statusText}${body ? `: ${body}` : ''}`);
  }
  return resp.json();
}

export async function startQaCollectionRun(payload: QaRunStartPayload): Promise<string> {
  const resp = await fetch(`${LOCAL_API_BASE}/api/qa-collections/run`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(payload),
  });
  const { runId } = await asJson<{ runId: string }>(resp);
  return runId;
}

export async function stopQaCollectionRun(runId: string): Promise<void> {
  await fetch(`${LOCAL_API_BASE}/api/qa-collections/stop/${runId}`, { method: 'POST' });
}

export function qaCollectionEventsUrl(runId: string): string {
  return `${LOCAL_API_BASE}/api/qa-collections/events/${runId}`;
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export type QaRunEvent = { type: string; [key: string]: any };

export interface QaRunDetail {
  runId: string;
  status: 'running' | 'passed' | 'failed' | 'stopped';
  triggeredBy: string;
  collections: string[];
  startedAt: number;
  finishedAt: number | null;
  passed: number;
  failed: number;
  events: QaRunEvent[];
}

export async function fetchQaRunDetail(runId: string): Promise<QaRunDetail> {
  const resp = await fetch(`${LOCAL_API_BASE}/api/qa-collections/runs/${runId}`);
  return asJson<QaRunDetail>(resp);
}

export interface QaSyncResult {
  ok: boolean;
  dir?: string;
  output?: string;
  error?: string;
}

/** git clone-or-pull ny-qa-automation on disk, then re-scan /api/collections
 * to pick it up. A failed sync (e.g. no git access) still returns a normal
 * {ok: false, output} — not an exception — so the caller can show it inline. */
export async function syncQaCollectionsRepo(): Promise<QaSyncResult> {
  try {
    const resp = await fetch(`${LOCAL_API_BASE}/api/qa-collections/sync`, { method: 'POST' });
    return await resp.json();
  } catch (e: any) {
    return { ok: false, error: e?.message ?? String(e) };
  }
}
