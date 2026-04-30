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

function HeaderTable({ headers, highlight }: { headers: Record<string, string>; highlight?: string }) {
  const keys = Object.keys(headers);
  if (keys.length === 0) return <div className="log-detail-empty">(none)</div>;
  return (
    <table className="log-detail-headers">
      <tbody>
        {keys.map((k) => (
          <tr key={k}>
            <td className="log-header-key">{highlightInline(k, highlight)}</td>
            <td className="log-header-val">{highlightInline(headers[k], highlight)}</td>
          </tr>
        ))}
      </tbody>
    </table>
  );
}

function LogEntryRow({ entry, highlight }: { entry: LogEntry; highlight?: string }) {
  const [expanded, setExpanded] = useState(false);
  const [showReqHeaders, setShowReqHeaders] = useState(false);
  const [showRespHeaders, setShowRespHeaders] = useState(false);
  const [localFilter, setLocalFilter] = useState('');
  const hasDetails = entry.request || entry.response;

  // Per-request filter overrides the global search highlight for body/header rendering.
  const effective = localFilter || highlight || '';

  useEffect(() => {
    if (!highlight) return;
    const q = highlight.toLowerCase();
    const matchesInside =
      (entry.request?.url ?? '').toLowerCase().includes(q) ||
      JSON.stringify(entry.request?.body ?? '').toLowerCase().includes(q) ||
      JSON.stringify(entry.response?.body ?? '').toLowerCase().includes(q) ||
      JSON.stringify(entry.request?.headers ?? '').toLowerCase().includes(q) ||
      JSON.stringify(entry.response?.headers ?? '').toLowerCase().includes(q) ||
      (entry.serviceLogs && Object.values(entry.serviceLogs).some((l) => l.toLowerCase().includes(q)));
    if (matchesInside) setExpanded(true);
  }, [highlight, entry]);

  const filterBody = (body: any): string => {
    const s = typeof body === 'string' ? body : JSON.stringify(body, null, 2);
    if (!effective) return s;
    const q = effective.toLowerCase();
    const lines = s.split('\n').filter((l) => l.toLowerCase().includes(q));
    return lines.length ? lines.join('\n') : s;
  };

  return (
    <div className="log-entry">
      <div className={`log-entry-header ${hasDetails ? 'clickable' : ''}`} onClick={() => hasDetails && setExpanded(!expanded)}>
        <span className="log-time">{entry.time}</span>
        <span className={`log-msg log-${entry.level}`}>{highlightInline(entry.message, highlight)}</span>
        {hasDetails && <span className="log-toggle">{expanded ? '\u25BC' : '\u25B6'}</span>}
      </div>
      {expanded && (
        <div className="log-details">
          {hasDetails && (
            <div className="log-detail-toolbar" onClick={(e) => e.stopPropagation()}>
              <input
                className="log-detail-filter"
                type="text"
                placeholder={highlight ? `Filter (override global: "${highlight}")` : 'Filter this request...'}
                value={localFilter}
                onChange={(e) => setLocalFilter(e.target.value)}
              />
              {localFilter && (
                <button className="log-detail-filter-clear" onClick={() => setLocalFilter('')} title="Clear filter">{'\u2715'}</button>
              )}
            </div>
          )}
          {entry.request && (
            <div className="log-detail-section">
              <span className="log-detail-label">Request</span>
              <span className="log-detail-method">{entry.request.method}</span>
              <span className="log-detail-url">{highlightInline(entry.request.url, effective)}</span>
              {entry.request.headers && Object.keys(entry.request.headers).length > 0 && (
                <button
                  className="log-detail-toggle"
                  onClick={(e) => { e.stopPropagation(); setShowReqHeaders((v) => !v); }}
                  title="Toggle request headers"
                >
                  {showReqHeaders ? '\u25BE' : '\u25B8'} Headers ({Object.keys(entry.request.headers).length})
                </button>
              )}
              {showReqHeaders && entry.request.headers && (
                <HeaderTable headers={entry.request.headers} highlight={effective} />
              )}
              {entry.request.body !== undefined && entry.request.body !== null && entry.request.body !== '' && (
                <pre className="log-detail-body">{highlightInline(filterBody(entry.request.body), effective)}</pre>
              )}
            </div>
          )}
          {entry.response && (
            <div className="log-detail-section">
              <span className="log-detail-label">Response</span>
              <span className={`log-detail-status ${entry.response.status >= 400 ? 'status-error' : 'status-ok'}`}>
                {entry.response.status}
              </span>
              {entry.response.headers && Object.keys(entry.response.headers).length > 0 && (
                <button
                  className="log-detail-toggle"
                  onClick={(e) => { e.stopPropagation(); setShowRespHeaders((v) => !v); }}
                  title="Toggle response headers"
                >
                  {showRespHeaders ? '\u25BE' : '\u25B8'} Headers ({Object.keys(entry.response.headers).length})
                </button>
              )}
              {showRespHeaders && entry.response.headers && (
                <HeaderTable headers={entry.response.headers} highlight={effective} />
              )}
              {entry.response.body !== undefined && entry.response.body !== null && entry.response.body !== '' && (
                <pre className="log-detail-body">{highlightInline(filterBody(entry.response.body), effective)}</pre>
              )}
            </div>
          )}
          {entry.serviceLogs && Object.keys(entry.serviceLogs).length > 0 && (
            <ServiceLogsSection logs={entry.serviceLogs} globalHighlight={effective} />
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
        JSON.stringify(e.request?.headers || '').toLowerCase().includes(q) ||
        JSON.stringify(e.response?.headers || '').toLowerCase().includes(q) ||
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
