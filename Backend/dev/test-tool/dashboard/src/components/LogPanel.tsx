import React, { useEffect, useRef, useState, useMemo } from 'react';
import { LogEntry } from '../types';
import './LogPanel.css';

interface Props {
  logs: LogEntry[];
  onClear: () => void;
}

function highlightInline(text: string, needle?: string) {
  if (!needle) return text;
  const q = needle.toLowerCase();
  const out: Array<React.ReactNode> = [];
  let i = 0;
  const lower = text.toLowerCase();
  while (i < text.length) {
    const idx = lower.indexOf(q, i);
    if (idx === -1) {
      out.push(text.slice(i));
      break;
    }
    if (idx > i) out.push(text.slice(i, idx));
    out.push(
      <mark className="log-highlight" key={idx}>
        {text.slice(idx, idx + needle.length)}
      </mark>
    );
    i = idx + needle.length;
  }
  return <>{out}</>;
}

function countMatches(text: string, needle?: string): number {
  if (!needle) return 0;
  const q = needle.toLowerCase();
  const lower = text.toLowerCase();
  let count = 0;
  let i = 0;
  while (true) {
    const idx = lower.indexOf(q, i);
    if (idx === -1) break;
    count++;
    i = idx + q.length;
  }
  return count;
}

function LogEntryRow({ entry, highlight }: { entry: LogEntry; highlight?: string }) {
  const [expanded, setExpanded] = useState(false);
  const hasDetails = entry.request || entry.response;

  // Auto-expand whenever the active search matches something inside this
  // entry's request / response / service logs — saves a click.
  useEffect(() => {
    if (!highlight) return;
    const q = highlight.toLowerCase();
    const matchesInside =
      (entry.request?.url ?? '').toLowerCase().includes(q) ||
      JSON.stringify(entry.request?.body ?? '').toLowerCase().includes(q) ||
      JSON.stringify(entry.response?.body ?? '').toLowerCase().includes(q) ||
      (entry.serviceLogs && Object.values(entry.serviceLogs).some((l) => l.toLowerCase().includes(q)));
    if (matchesInside) setExpanded(true);
  }, [highlight, entry]);

  return (
    <div className="log-entry">
      <div className={`log-entry-header ${hasDetails ? 'clickable' : ''}`} onClick={() => hasDetails && setExpanded(!expanded)}>
        <span className="log-time">{entry.time}</span>
        <span className={`log-msg log-${entry.level}`}>{highlightInline(entry.message, highlight)}</span>
        {hasDetails && <span className="log-toggle">{expanded ? '\u25BC' : '\u25B6'}</span>}
      </div>
      {expanded && (
        <div className="log-details">
          {entry.request && (
            <div className="log-detail-section">
              <span className="log-detail-label">Request</span>
              <span className="log-detail-method">{entry.request.method}</span>
              <span className="log-detail-url">{highlightInline(entry.request.url, highlight)}</span>
              {entry.request.body && (
                <pre className="log-detail-body">{highlightInline(JSON.stringify(entry.request.body, null, 2), highlight)}</pre>
              )}
            </div>
          )}
          {entry.response && (
            <div className="log-detail-section">
              <span className="log-detail-label">Response</span>
              <span className={`log-detail-status ${entry.response.status >= 400 ? 'status-error' : 'status-ok'}`}>
                {entry.response.status}
              </span>
              {entry.response.body && (
                <pre className="log-detail-body">{highlightInline(JSON.stringify(entry.response.body, null, 2), highlight)}</pre>
              )}
            </div>
          )}
          {entry.serviceLogs && Object.keys(entry.serviceLogs).length > 0 && (
            <ServiceLogsSection logs={entry.serviceLogs} globalHighlight={highlight} />
          )}
        </div>
      )}
    </div>
  );
}

function ServiceLogsSection({ logs, globalHighlight }: { logs: Record<string, string>; globalHighlight?: string }) {
  const [activeTab, setActiveTab] = useState<string>(Object.keys(logs)[0] || '');
  const [svcSearch, setSvcSearch] = useState('');
  const services = Object.keys(logs);

  // If the user is typing in the global search, use that as a
  // line-filter inside service logs too unless they've overridden it
  // locally.
  const effectiveFilter = svcSearch || globalHighlight || '';

  const filteredLog = useMemo(() => {
    if (!activeTab || !logs[activeTab]) return '';
    if (!effectiveFilter) return logs[activeTab];
    return logs[activeTab]
      .split('\n')
      .filter((line) => line.toLowerCase().includes(effectiveFilter.toLowerCase()))
      .join('\n');
  }, [activeTab, logs, effectiveFilter]);

  const matchCount = countMatches(filteredLog, effectiveFilter);

  return (
    <div className="log-detail-section">
      <span className="log-detail-label">
        Service Logs ({services.length})
        {effectiveFilter && <span className="svc-log-matches"> — {matchCount} match{matchCount === 1 ? '' : 'es'}</span>}
      </span>
      <div className="service-log-tabs">
        {services.map((svc) => (
          <button
            key={svc}
            className={`service-log-tab ${svc === activeTab ? 'active' : ''}`}
            onClick={() => setActiveTab(svc)}
          >
            {svc.replace('-exe', '')}
          </button>
        ))}
        <input
          className="svc-log-search"
          type="text"
          placeholder={globalHighlight ? `Filter (override global: "${globalHighlight}")` : 'Filter logs...'}
          value={svcSearch}
          onChange={(e) => setSvcSearch(e.target.value)}
        />
      </div>
      {activeTab && filteredLog && (
        <pre className="log-detail-body service-log-body">{highlightInline(filteredLog, effectiveFilter)}</pre>
      )}
      {activeTab && !filteredLog && effectiveFilter && (
        <div className="log-empty">No lines match "{effectiveFilter}" in {activeTab}</div>
      )}
    </div>
  );
}

export const LogPanel: React.FC<Props> = ({ logs, onClear }) => {
  const endRef = useRef<HTMLDivElement>(null);
  const [search, setSearch] = useState('');

  useEffect(() => {
    endRef.current?.scrollIntoView({ behavior: 'smooth' });
  }, [logs]);

  const filtered = useMemo(() => {
    if (!search) return logs;
    const q = search.toLowerCase();
    return logs.filter(
      (e) =>
        e.message.toLowerCase().includes(q) ||
        e.request?.url?.toLowerCase().includes(q) ||
        e.request?.method?.toLowerCase().includes(q) ||
        JSON.stringify(e.request?.body || '').toLowerCase().includes(q) ||
        JSON.stringify(e.response?.body || '').toLowerCase().includes(q) ||
        (e.serviceLogs && Object.values(e.serviceLogs).some((l) => l.toLowerCase().includes(q)))
    );
  }, [logs, search]);

  // Rough overall match count: per-entry hits counted across message +
  // url + request/response bodies + every service-log text.
  const totalMatches = useMemo(() => {
    if (!search) return 0;
    return filtered.reduce((acc, e) => {
      let n = countMatches(e.message, search);
      if (e.request?.url) n += countMatches(e.request.url, search);
      if (e.request?.body) n += countMatches(JSON.stringify(e.request.body), search);
      if (e.response?.body) n += countMatches(JSON.stringify(e.response.body), search);
      if (e.serviceLogs) {
        for (const txt of Object.values(e.serviceLogs)) n += countMatches(txt, search);
      }
      return acc + n;
    }, 0);
  }, [filtered, search]);

  return (
    <div className="log-panel">
      <div className="log-panel-header">
        <span className="log-panel-title">
          Logs ({filtered.length}
          {search && <span className="log-match-count"> · {totalMatches} match{totalMatches === 1 ? '' : 'es'}</span>}
          )
        </span>
        <div className="log-panel-actions">
          <input
            className="log-search"
            type="text"
            placeholder="Search logs (message / URL / body / service logs)..."
            value={search}
            onChange={(e) => setSearch(e.target.value)}
          />
          {search && (
            <button
              className="log-search-clear"
              onClick={() => setSearch('')}
              title="Clear search"
            >
              {'\u2715'}
            </button>
          )}
          <button className="log-clear-btn" onClick={onClear} title="Clear logs">
            Clear
          </button>
        </div>
      </div>
      <div className="log-panel-body">
        {filtered.length === 0 && (
          <div className="log-empty">{logs.length === 0 ? 'Logs will appear here...' : 'No matching logs'}</div>
        )}
        {filtered.map((entry, i) => (
          <LogEntryRow key={i} entry={entry} highlight={search} />
        ))}
        <div ref={endRef} />
      </div>
    </div>
  );
};
