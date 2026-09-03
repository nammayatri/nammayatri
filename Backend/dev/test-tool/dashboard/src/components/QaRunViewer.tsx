import React, { useEffect, useRef, useState } from 'react';
import './QaRunViewer.css';
import { fetchQaRunDetail, qaCollectionEventsUrl, QaRunDetail, QaRunEvent } from '../services/qaCollectionsRunner';

// Deep-link target for a QA run: open this dashboard with ?qaRunId=<id> (e.g.
// the link System Control Centre stores on a triggered run) and it renders
// that run's full detail — live via SSE while it's still going, or the
// persisted event log (incl. request/response bodies for failures) once it's
// finished — without needing to select anything in the Collections tab.
// Mounted unconditionally in App.tsx; renders nothing when the param is absent.

type RunMeta = Omit<QaRunDetail, 'events'>;

function fmtDur(start: number, end: number | null): string {
  const ms = (end ?? Date.now()) - start;
  if (ms < 1000) return `${ms}ms`;
  return `${(ms / 1000).toFixed(1)}s`;
}

export const QaRunViewer: React.FC = () => {
  const [runId] = useState<string | null>(() => new URLSearchParams(window.location.search).get('qaRunId'));
  const [meta, setMeta] = useState<RunMeta | null>(null);
  const [events, setEvents] = useState<QaRunEvent[]>([]);
  const [error, setError] = useState<string | null>(null);
  const [dismissed, setDismissed] = useState(false);
  const esRef = useRef<EventSource | null>(null);
  const initializedRef = useRef(false);

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
        // Only seed from the persisted history once — after that, further
        // events come from the live SSE stream so we don't double-count on
        // the fallback poll's periodic re-fetches.
        if (!initializedRef.current) {
          setEvents(fetchedEvents);
          initializedRef.current = true;
        }
        if (rest.status === 'running' && !esRef.current) {
          const es = new EventSource(qaCollectionEventsUrl(runId));
          esRef.current = es;
          es.onmessage = (ev) => {
            let msg: QaRunEvent;
            try { msg = JSON.parse(ev.data); } catch { return; }
            setEvents(prev => [...prev, msg]);
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
    // Cheap safety net: if SSE never connects (run already finished, or the
    // connection drops), keep status/counts fresh via polling.
    const poll = setInterval(() => { if (!esRef.current) load(); }, 5000);

    return () => {
      cancelled = true;
      esRef.current?.close();
      clearInterval(poll);
    };
  }, [runId]);

  if (!runId || dismissed) return null;

  return (
    <div className="qarv-overlay">
      <div className="qarv-panel">
        <div className="qarv-header">
          <span>QA Run <code>{runId}</code></span>
          {meta && <span className={`qarv-status qarv-status-${meta.status}`}>{meta.status}</span>}
          <span className="qarv-spacer" />
          <button className="qarv-close" onClick={() => setDismissed(true)} title="Close (doesn't stop the run)">✕</button>
        </div>
        {error && <div className="qarv-error">Could not load run {runId}: {error}</div>}
        {meta && (
          <div className="qarv-meta">
            <span>{meta.collections.join(', ')}</span>
            <span>{meta.passed}✓ {meta.failed}✗</span>
            <span>{fmtDur(meta.startedAt, meta.finishedAt)}</span>
            <span>triggered by {meta.triggeredBy}</span>
          </div>
        )}
        <div className="qarv-log">
          {!meta && !error && <div className="qarv-loading">loading…</div>}
          {events.map((ev, i) => <QaLogLine key={i} ev={ev} />)}
        </div>
      </div>
    </div>
  );
};

const QaLogLine: React.FC<{ ev: QaRunEvent }> = ({ ev }) => {
  switch (ev.type) {
    case 'collection_started':
      return <div className="qarv-line qarv-line-info">▶ {ev.directory}/{ev.collection} started</div>;
    case 'item_start':
      return <div className="qarv-line qarv-line-muted">&nbsp;&nbsp;→ {ev.name}</div>;
    case 'request': {
      const failed = !!ev.error || (typeof ev.status === 'number' && ev.status >= 400);
      if (!failed) {
        return (
          <div className="qarv-line qarv-line-muted">
            &nbsp;&nbsp;{ev.method} {ev.status} · {ev.responseTime}ms {ev.name}
          </div>
        );
      }
      return (
        <details className="qarv-failure">
          <summary className="qarv-line qarv-line-fail">
            ✗ {ev.method ?? ''} {ev.name}{ev.status ? ` — HTTP ${ev.status}` : ''}{ev.error ? ` — ${ev.error}` : ''}
          </summary>
          {ev.url && <div className="qarv-detail-row"><b>URL</b> {ev.url}</div>}
          {ev.requestBody && <><div className="qarv-detail-row"><b>Request body</b></div><pre className="qarv-body">{ev.requestBody}</pre></>}
          {ev.responseBody && <><div className="qarv-detail-row"><b>Response body</b></div><pre className="qarv-body">{ev.responseBody}</pre></>}
        </details>
      );
    }
    case 'assertion':
      return (
        <div className={`qarv-line ${ev.passed ? 'qarv-line-pass' : 'qarv-line-fail'}`}>
          {ev.passed ? '✓' : '✗'} {ev.name}{ev.error ? ` — ${ev.error}` : ''}
        </div>
      );
    case 'collection_result':
      return (
        <div className="qarv-line qarv-line-info">
          ■ {ev.directory}/{ev.collection} done — {ev.passed}✓ {ev.failed}✗
        </div>
      );
    case 'run_complete':
      return (
        <div className="qarv-line qarv-line-summary">
          ── run complete: {ev.passed}✓ {ev.failed}✗ in {ev.durationMs}ms{ev.aborted ? ' (stopped)' : ''} ──
        </div>
      );
    case 'error':
      return <div className="qarv-line qarv-line-fail">error: {ev.error}</div>;
    default:
      return null;
  }
};
