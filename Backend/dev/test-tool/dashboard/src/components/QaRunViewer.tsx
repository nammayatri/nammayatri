import React, { useEffect, useRef, useState } from 'react';
import './QaRunViewer.css';
import { fetchQaRunDetail, qaCollectionEventsUrl, checkActiveQaRun, QaRunDetail, QaRunEvent } from '../services/qaCollectionsRunner';
import { LogPanel } from './LogPanel';
import type { LogEntry } from '../types';
import type { PostmanStepResult } from '../services/api';

// Deep-link target for a QA run: open this dashboard with ?qaRunId=<id> (e.g.
// the link System Control Centre stores on a triggered run) and it renders
// that run's full detail — live via SSE while it's still going, or the
// persisted event log (incl. request/response bodies) once it's finished —
// with the same step-list + expandable-request/response UI as a normal
// backend run started from the Collections tab, not just a flat log dump.
// Mounted unconditionally in App.tsx. Also self-attaches to whichever run is
// currently active even without ?qaRunId= in the URL — only one QA run can
// be in flight at a time (server-enforced), so anyone opening the dashboard
// while one is happening should see it, not go looking for a link.

type RunMeta = Omit<QaRunDetail, 'events'>;

interface QaStepRow {
  key: string;
  collection: string;
  name: string;
  status: 'running' | 'pass' | 'fail';
  method?: string;
  durationMs?: number;
  result?: PostmanStepResult;
}

interface CollectionSummary {
  key: string;
  label: string;
  status: 'running' | 'done';
  passed: number;
  failed: number;
  error?: string;
}

interface PendingStep {
  collection: string;
  name: string;
  method?: string;
  url?: string;
  status?: number;
  responseTime?: number;
  error?: string;
  requestBody?: unknown;
  requestHeaders?: Record<string, string>;
  responseBody?: unknown;
  responseHeaders?: Record<string, string>;
  assertions: Array<{ name: string; passed: boolean; error?: string }>;
}

function fmtDur(start: number, end: number | null): string {
  const ms = (end ?? Date.now()) - start;
  if (ms < 1000) return `${ms}ms`;
  return `${(ms / 1000).toFixed(1)}s`;
}

function parseIfJson(body: unknown): unknown {
  if (typeof body !== 'string') return body;
  try {
    return JSON.parse(body);
  } catch {
    return body;
  }
}

function headersToObject(hs: unknown): Record<string, string> | undefined {
  if (!Array.isArray(hs)) return undefined;
  return Object.fromEntries(hs.map((h: any) => [h.key, h.value]));
}

export const QaRunViewer: React.FC = () => {
  const [runId, setRunId] = useState<string | null>(() => new URLSearchParams(window.location.search).get('qaRunId'));
  const [meta, setMeta] = useState<RunMeta | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [dismissed, setDismissed] = useState(false);
  const [logs, setLogs] = useState<LogEntry[]>([]);
  const [steps, setSteps] = useState<QaStepRow[]>([]);
  const [collections, setCollections] = useState<CollectionSummary[]>([]);
  const [expandedSteps, setExpandedSteps] = useState<Set<string>>(new Set());
  const esRef = useRef<EventSource | null>(null);
  const initializedRef = useRef(false);
  const pendingRef = useRef<Map<string, PendingStep>>(new Map());

  // Self-attach: no ?qaRunId= yet, but a run is happening anyway.
  useEffect(() => {
    if (runId) return;
    let cancelled = false;
    const poll = async () => {
      const id = await checkActiveQaRun();
      if (!cancelled && id) {
        setRunId(id);
        const url = new URL(window.location.href);
        url.searchParams.set('qaRunId', id);
        window.history.replaceState(null, '', url.toString());
      }
    };
    poll();
    const timer = setInterval(poll, 5000);
    return () => { cancelled = true; clearInterval(timer); };
  }, [runId]);

  const addLog = (level: LogEntry['level'], message: string, extra?: Partial<LogEntry>) => {
    setLogs(prev => [...prev, { time: new Date().toLocaleTimeString(), level, message, ...extra }]);
  };

  const flushPending = (collection: string) => {
    const p = pendingRef.current.get(collection);
    if (!p) return;
    pendingRef.current.delete(collection);
    const httpFailed = !!p.error || (typeof p.status === 'number' && p.status >= 400);
    const anyAssertFail = p.assertions.some(a => !a.passed);
    const failed = httpFailed || anyAssertFail;
    const assertSummary = p.assertions.length > 0
      ? ` [${p.assertions.filter(a => a.passed).length}/${p.assertions.length} assertions]`
      : '';
    const message = p.error
      ? `FAIL ${p.name}: ${p.error}`
      : `${failed ? 'FAIL' : 'PASS'} ${p.name} (${p.responseTime ?? '?'}ms, ${p.status ?? '?'})${assertSummary}`;
    addLog(failed ? 'error' : 'success', `[${p.collection}] ${message}`, {
      request: { method: p.method ?? '', url: p.url ?? '', body: p.requestBody, headers: p.requestHeaders },
      response: { status: p.status ?? 0, body: p.responseBody, headers: p.responseHeaders },
    });
  };

  const upsertStep = (key: string, collection: string, name: string, patch: Partial<QaStepRow>) => {
    setSteps(prev => {
      const idx = prev.findIndex(s => s.key === key);
      if (idx === -1) {
        return [...prev, { key, collection, name, status: 'running', ...patch }];
      }
      const next = [...prev];
      next[idx] = { ...next[idx], ...patch };
      return next;
    });
  };

  const ingest = (msg: QaRunEvent) => {
    switch (msg.type) {
      case 'run_started':
        addLog('info', `-- run started: ${msg.collections} collections, concurrency ${msg.concurrency} --`);
        break;
      case 'collection_started': {
        const key = `${msg.directory}/${msg.collection}`;
        setCollections(prev => prev.some(c => c.key === key)
          ? prev
          : [...prev, { key, label: key, status: 'running', passed: 0, failed: 0 }]);
        addLog('info', `▶ ${key} started`);
        break;
      }
      case 'item_start': {
        const collection = msg.collection ?? '';
        flushPending(collection);
        pendingRef.current.set(collection, { collection, name: msg.name, assertions: [] });
        const key = `${collection}::${msg.name}`;
        upsertStep(key, collection, msg.name, { status: 'running' });
        break;
      }
      case 'request': {
        const collection = msg.collection ?? '';
        const p = pendingRef.current.get(collection);
        if (p && p.name === msg.name) {
          p.method = msg.method;
          p.url = msg.url;
          p.status = msg.status;
          p.responseTime = msg.responseTime;
          p.error = msg.error;
          p.requestBody = parseIfJson(msg.requestBody);
          p.requestHeaders = headersToObject(msg.requestHeaders);
          p.responseBody = parseIfJson(msg.responseBody);
          p.responseHeaders = headersToObject(msg.responseHeaders);
        }
        const key = `${collection}::${msg.name}`;
        const httpFailed = !!msg.error || (typeof msg.status === 'number' && msg.status >= 400);
        upsertStep(key, collection, msg.name, {
          status: httpFailed ? 'fail' : 'pass',
          method: msg.method,
          durationMs: msg.responseTime,
          result: {
            ok: !httpFailed,
            status: msg.status ?? 0,
            data: parseIfJson(msg.responseBody),
            elapsed: msg.responseTime ?? 0,
            assertions: [],
            consoleLogs: [],
            serviceLogs: {},
            resolvedUrl: msg.url ?? '',
            responseHeaders: headersToObject(msg.responseHeaders),
            upstreamMs: msg.responseTime ?? 0,
          },
        });
        break;
      }
      case 'assertion': {
        const collection = msg.collection ?? '';
        const p = pendingRef.current.get(collection);
        if (p && p.name === msg.item) {
          p.assertions.push({ name: msg.name, passed: msg.passed, error: msg.error ?? undefined });
        }
        const key = `${collection}::${msg.item}`;
        setSteps(prev => {
          const idx = prev.findIndex(s => s.key === key);
          if (idx === -1) return prev;
          const existing = prev[idx];
          const assertions = [...(existing.result?.assertions ?? []), { name: msg.name, passed: msg.passed, error: msg.error ?? undefined }];
          const anyFail = assertions.some(a => !a.passed);
          const next = [...prev];
          next[idx] = {
            ...existing,
            status: anyFail ? 'fail' : existing.status,
            result: existing.result ? { ...existing.result, assertions } : {
              ok: !anyFail, status: 0, data: undefined, elapsed: 0, assertions,
              consoleLogs: [], serviceLogs: {}, resolvedUrl: '', upstreamMs: 0,
            },
          };
          return next;
        });
        break;
      }
      case 'collection_result': {
        const collection = msg.collection ?? '';
        flushPending(collection);
        const key = `${msg.directory}/${msg.collection}`;
        setCollections(prev => prev.map(c => c.key === key
          ? { ...c, status: 'done', passed: msg.passed ?? 0, failed: msg.failed ?? 0, error: msg.error }
          : c));
        addLog(msg.failed > 0 || msg.error ? 'error' : 'success', `■ ${key} done — ${msg.passed ?? 0}✓ ${msg.failed ?? 0}✗${msg.error ? ` (${msg.error})` : ''}`);
        break;
      }
      case 'run_complete':
        pendingRef.current.forEach((_, c) => flushPending(c));
        addLog(msg.failed > 0 ? 'error' : 'success',
          `-- run complete: ${msg.passed}✓ ${msg.failed}✗ in ${msg.durationMs}ms${msg.aborted ? ' (stopped)' : ''} --`);
        break;
      case 'error':
        addLog('error', `error: ${msg.error}`);
        break;
      default:
        break;
    }
  };

  useEffect(() => {
    if (!runId) return;
    let cancelled = false;

    const load = async () => {
      try {
        const d = await fetchQaRunDetail(runId);
        if (cancelled) return;
        const { events: fetchedEvents, ...rest } = d;
        setMeta(rest);
        setError(null);
        if (!initializedRef.current) {
          fetchedEvents.forEach(ingest);
          initializedRef.current = true;
        }
        if (rest.status === 'running' && !esRef.current) {
          const es = new EventSource(qaCollectionEventsUrl(runId));
          esRef.current = es;
          es.onmessage = (ev) => {
            let msg: QaRunEvent;
            try { msg = JSON.parse(ev.data); } catch { return; }
            ingest(msg);
            if (msg.type === 'run_complete' || msg.type === 'error') {
              es.close();
              esRef.current = null;
              load(); // one more fetch to reconcile final status/passed/failed
            }
          };
          es.onerror = () => { /* server closes the stream on completion — expected */ };
        }
      } catch (e: any) {
        if (!cancelled) setError(e?.message ?? String(e));
      }
    };

    load();
    const poll = setInterval(() => { if (!esRef.current) load(); }, 5000);

    return () => {
      cancelled = true;
      esRef.current?.close();
      clearInterval(poll);
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [runId]);

  if (!runId || dismissed) return null;

  const toggleStep = (key: string) => {
    setExpandedSteps(prev => {
      const next = new Set(prev);
      if (next.has(key)) next.delete(key); else next.add(key);
      return next;
    });
  };

  // Group steps by collection, in first-seen order (matches `collections`).
  const stepsByCollection = new Map<string, QaStepRow[]>();
  for (const s of steps) {
    if (!stepsByCollection.has(s.collection)) stepsByCollection.set(s.collection, []);
    stepsByCollection.get(s.collection)!.push(s);
  }

  return (
    <div className="qarv-overlay">
      <div className="qarv-panel qarv-panel-rich">
        <div className="qarv-header">
          <span>QA Run <code>{runId}</code></span>
          {meta && <span className={`qarv-status qarv-status-${meta.status}`}>{meta.status}</span>}
          <span className="qarv-spacer" />
          <button className="qarv-close" onClick={() => setDismissed(true)} title="Close (doesn't stop the run)">✕</button>
        </div>
        {error && <div className="qarv-error">Could not load run {runId}: {error}</div>}
        {meta && (
          <div className="qarv-meta">
            <span>{meta.collections.length} collections</span>
            <span>{meta.passed}✓ {meta.failed}✗</span>
            <span>{fmtDur(meta.startedAt, meta.finishedAt)}</span>
            <span>triggered by {meta.triggeredBy}</span>
          </div>
        )}
        <div className="qarv-content">
          <div className="cr-steps qarv-steps">
            {!meta && !error && <div className="qarv-loading">loading…</div>}
            {Array.from(stepsByCollection.entries()).map(([collection, rows]: [string, QaStepRow[]]) => {
              const summary = collections.find(c => c.key.endsWith(`/${collection}`) || c.key === collection);
              return (
                <div key={collection} className="cr-node">
                  <div className="cr-node-header">
                    <span className="cr-node-title">
                      {collection}
                      {summary && (
                        <span className={`qarv-collection-badge qarv-collection-${summary.status}`}>
                          {summary.status === 'running' ? 'running…' : `${summary.passed}✓ ${summary.failed}✗`}
                        </span>
                      )}
                    </span>
                  </div>
                  {rows.map((step: QaStepRow) => {
                    const isExpanded = expandedSteps.has(step.key);
                    return (
                      <div key={step.key} className={`cr-step cr-step-${step.status}`}>
                        <div className="cr-step-header" onClick={() => toggleStep(step.key)}>
                          <span className={`cr-dot cr-dot-${step.status}`} />
                          {step.method && <span className="cr-step-method">{step.method}</span>}
                          <span className="cr-step-name">{step.name}</span>
                          {step.status === 'running' && <span className="cr-spinner" />}
                          {step.durationMs != null && <span className="cr-duration">{step.durationMs}ms</span>}
                          {step.result && step.result.status > 0 && (
                            <span className={`cr-status-code ${step.result.status >= 400 ? 'cr-status-error' : ''}`}>{step.result.status}</span>
                          )}
                        </div>
                        {isExpanded && step.result && (
                          <div className="cr-step-detail">
                            {step.result.assertions.length > 0 && (
                              <div className="cr-assertions">
                                {step.result.assertions.map((a: { name: string; passed: boolean; error?: string }, i: number) => (
                                  <div key={i} className={`cr-assertion ${a.passed ? 'cr-assert-pass' : 'cr-assert-fail'}`}>
                                    {a.passed ? '✓' : '✗'} {a.name}
                                    {a.error && <span className="cr-assert-error"> — {a.error}</span>}
                                  </div>
                                ))}
                              </div>
                            )}
                            <pre className="cr-response-body">{JSON.stringify(step.result.data, null, 2)}</pre>
                          </div>
                        )}
                      </div>
                    );
                  })}
                </div>
              );
            })}
          </div>
          <div className="qarv-logs">
            <LogPanel logs={logs} onClear={() => setLogs([])} />
          </div>
        </div>
      </div>
    </div>
  );
};
