import React, { useEffect, useRef, useState, useMemo } from 'react';
import { LogEntry } from '../types';
import './LogPanel.css';

interface Props {
  logs: LogEntry[];
  onClear: () => void;
}

function LogEntryRow({ entry, highlight }: { entry: LogEntry; highlight?: string }) {
  const [expanded, setExpanded] = useState(false);
  const hasDetails = entry.request || entry.response;

  const highlightText = (text: string) => {
    if (!highlight) return text;
    const idx = text.toLowerCase().indexOf(highlight.toLowerCase());
    if (idx === -1) return text;
    return <>{text.slice(0, idx)}<mark className="log-highlight">{text.slice(idx, idx + highlight.length)}</mark>{text.slice(idx + highlight.length)}</>;
  };

  return (
    <div className="log-entry">
      <div className={`log-entry-header ${hasDetails ? 'clickable' : ''}`} onClick={() => hasDetails && setExpanded(!expanded)}>
        <span className="log-time">{entry.time}</span>
        <span className={`log-msg log-${entry.level}`}>{highlightText(entry.message)}</span>
        {hasDetails && <span className="log-toggle">{expanded ? '\u25BC' : '\u25B6'}</span>}
      </div>
      {expanded && (
        <div className="log-details">
          {entry.request && (
            <div className="log-detail-section">
              <span className="log-detail-label">Request</span>
              <span className="log-detail-method">{entry.request.method}</span>
              <span className="log-detail-url">{entry.request.url}</span>
              {entry.request.body && (
                <pre className="log-detail-body">{JSON.stringify(entry.request.body, null, 2)}</pre>
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
                <pre className="log-detail-body">{JSON.stringify(entry.response.body, null, 2)}</pre>
              )}
            </div>
          )}
        </div>
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
    return logs.filter(e =>
      e.message.toLowerCase().includes(q) ||
      e.request?.url?.toLowerCase().includes(q) ||
      e.request?.method?.toLowerCase().includes(q) ||
      JSON.stringify(e.response?.body || '').toLowerCase().includes(q)
    );
  }, [logs, search]);

  return (
    <div className="log-panel">
      <div className="log-panel-header">
        <span className="log-panel-title">Logs ({filtered.length})</span>
        <div className="log-panel-actions">
          <input
            className="log-search"
            type="text"
            placeholder="Search..."
            value={search}
            onChange={e => setSearch(e.target.value)}
          />
          <button className="log-clear-btn" onClick={onClear} title="Clear logs">Clear</button>
        </div>
      </div>
      <div className="log-panel-body">
        {filtered.length === 0 && <div className="log-empty">{logs.length === 0 ? 'Logs will appear here...' : 'No matching logs'}</div>}
        {filtered.map((entry, i) => (
          <LogEntryRow key={i} entry={entry} highlight={search} />
        ))}
        <div ref={endRef} />
      </div>
    </div>
  );
};
