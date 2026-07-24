import React, { useCallback, useEffect, useRef, useState } from 'react';
import { remoteLogList, remoteLogTail, remoteLogClear, RemoteTarget } from '../services/remote';
import './RemoteLogsModal.css';

const LAYOUT_KEY = 'ny.remoteStack.logLayout';

type SplitDir = 'row' | 'column';

interface PaneSpec {
  id: string;
  file: string;
  follow: boolean;
  sort: 'none' | 'asc' | 'desc';
}

interface Layout {
  dir: SplitDir;
  panes: PaneSpec[];
}

const newId = () => Math.random().toString(36).slice(2, 9);

const loadLayout = (): Layout | null => {
  try {
    const raw = window.localStorage.getItem(LAYOUT_KEY);
    if (!raw) return null;
    const parsed = JSON.parse(raw);
    if (!parsed || !Array.isArray(parsed.panes) || !parsed.panes.length) return null;
    return {
      dir: parsed.dir === 'column' ? 'column' : 'row',
      panes: parsed.panes.map((p: any) => ({
        id: newId(),
        file: String(p.file || ''),
        follow: p.follow !== false,
        sort: p.sort === 'asc' || p.sort === 'desc' ? p.sort : 'none',
      })),
    };
  } catch {
    return null;
  }
};

const saveLayout = (layout: Layout): void => {
  try {
    window.localStorage.setItem(LAYOUT_KEY, JSON.stringify({
      dir: layout.dir,
      panes: layout.panes.map(p => ({ file: p.file, follow: p.follow, sort: p.sort })),
    }));
  } catch {
    /* ignore */
  }
};

// Matches "2026-07-21 12:53:09.269707939" (space or T separator, optional frac).
// Fixed-width + zero-padded, so lexicographic order == time order.
const TS_RE = /(\d{4}-\d{2}-\d{2}[ T]\d{2}:\d{2}:\d{2}(?:\.\d+)?)/;

const applyView = (content: string, search: string, sort: PaneSpec['sort']): string => {
  let lines = content.split('\n');
  const q = search.trim().toLowerCase();
  if (q) lines = lines.filter(l => l.toLowerCase().includes(q));
  if (sort !== 'none') {
    // Each line's key is its own timestamp, else the previous line's, so a
    // multi-line entry's continuation stays grouped with its parent.
    let last = '';
    const keyed = lines.map((line, i) => {
      const m = line.match(TS_RE);
      if (m) last = m[1];
      return { line, i, ts: last };
    });
    keyed.sort((a, b) => {
      if (a.ts !== b.ts) {
        if (!a.ts) return 1;
        if (!b.ts) return -1;
        const cmp = a.ts < b.ts ? -1 : 1;
        return sort === 'asc' ? cmp : -cmp;
      }
      return a.i - b.i;
    });
    lines = keyed.map(k => k.line);
  }
  return lines.join('\n');
};

interface PaneProps {
  spec: PaneSpec;
  files: string[];
  target: RemoteTarget;
  isLocalhost: boolean;
  canClose: boolean;
  onChange: (patch: Partial<PaneSpec>) => void;
  onSplit: (dir: SplitDir) => void;
  onClose: () => void;
}

const LogPane: React.FC<PaneProps> = ({
  spec, files, target, isLocalhost, canClose, onChange, onSplit, onClose,
}) => {
  const [content, setContent] = useState('');
  const [error, setError] = useState<string | null>(null);
  const [truncated, setTruncated] = useState(false);
  const [search, setSearch] = useState('');
  const [nonce, setNonce] = useState(0);
  const [clearing, setClearing] = useState(false);
  const bodyRef = useRef<HTMLPreElement | null>(null);
  const sortRef = useRef(spec.sort);
  sortRef.current = spec.sort;

  // follow ON → live tail (auto-refresh 2s, auto-scroll);
  // follow OFF → the FULL log, fetched once (refresh via ↻).
  useEffect(() => {
    if (!spec.file) return;
    let cancelled = false;
    let iv: number | undefined;
    const fetchLog = async () => {
      try {
        const res = await remoteLogTail({ ...target, file: spec.file, full: !spec.follow, lines: 2000 });
        if (cancelled) return;
        if (res.error) {
          setError(res.error);
        } else {
          setError(null);
          setTruncated(!!res.truncated);
          setContent(res.content || '(empty)');
          if (spec.follow && sortRef.current === 'none' && bodyRef.current) {
            const el = bodyRef.current;
            requestAnimationFrame(() => { el.scrollTop = el.scrollHeight; });
          }
        }
      } catch (e) {
        if (!cancelled) setError(`Tail failed: ${e}`);
      }
    };
    fetchLog();
    if (spec.follow) iv = window.setInterval(fetchLog, 2000);
    return () => { cancelled = true; if (iv != null) window.clearInterval(iv); };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [spec.file, spec.follow, nonce]);

  const onClear = async () => {
    if (!spec.file) return;
    if (!window.confirm(`Clear ${spec.file}? This empties the file on ${isLocalhost ? 'this machine' : target.host}.`)) return;
    setClearing(true);
    try {
      const res = await remoteLogClear({ ...target, file: spec.file });
      if (res.error) setError(res.error);
      else { setContent(''); setError(null); setNonce(n => n + 1); }
    } catch (e) {
      setError(`Clear failed: ${e}`);
    } finally {
      setClearing(false);
    }
  };

  const view = applyView(content, search, spec.sort);
  const matches = search.trim() ? view.split('\n').filter(Boolean).length : 0;

  return (
    <div className="rlm-pane">
      <div className="rlm-pane-head">
        <select
          className="rlm-select"
          value={spec.file}
          onChange={e => { setContent(''); onChange({ file: e.target.value }); }}
          disabled={!files.length}
          title="Log file shown in this pane"
        >
          {!files.length && <option value="">(no logs)</option>}
          {files.map(f => <option key={f} value={f}>{f}</option>)}
        </select>

        <button
          className="rlm-btn rlm-btn-icon"
          onClick={() => onSplit('row')}
          title="Split horizontally — add a pane beside this one"
          aria-label="Split horizontally"
        >
          ▥
        </button>
        <button
          className="rlm-btn rlm-btn-icon"
          onClick={() => onSplit('column')}
          title="Split vertically — add a pane below this one"
          aria-label="Split vertically"
        >
          ▤
        </button>

        <button
          className="rlm-btn rlm-btn-icon"
          onClick={() => setNonce(n => n + 1)}
          title="Refresh now"
          aria-label="Refresh"
        >
          ↻
        </button>
        <label
          className={`rlm-follow${spec.follow ? ' is-on' : ''}`}
          title="On: live tail (auto-refresh 2s). Off: load the FULL log."
        >
          <input
            type="checkbox"
            checked={spec.follow}
            onChange={e => onChange({ follow: e.target.checked })}
          />
          <span className="rlm-dot" />
          follow
        </label>
        <select
          className="rlm-select rlm-sort"
          value={spec.sort}
          onChange={e => onChange({ sort: e.target.value as PaneSpec['sort'] })}
          title="Sort lines by their timestamp"
        >
          <option value="none">file order</option>
          <option value="asc">time ↑</option>
          <option value="desc">time ↓</option>
        </select>
        <button
          className="rlm-btn rlm-btn-danger"
          onClick={onClear}
          disabled={clearing || !spec.file}
          title="Empty this log file on the box"
        >
          {clearing ? 'Clearing…' : 'Clear'}
        </button>
        <span className="rlm-spacer" />
        {canClose && (
          <button
            className="rlm-btn rlm-btn-icon"
            onClick={onClose}
            title="Close this pane"
            aria-label="Close pane"
          >
            ✕
          </button>
        )}
      </div>

      <div className="rlm-searchbar">
        <input
          className="rlm-search"
          type="text"
          placeholder="Search this log (case-insensitive, filters matching lines)…"
          value={search}
          onChange={e => setSearch(e.target.value)}
        />
        {search.trim() && (
          <span className="rlm-matches">
            {matches} match{matches === 1 ? '' : 'es'}
            <button className="rlm-btn" onClick={() => setSearch('')} title="Clear search">clear</button>
          </span>
        )}
      </div>

      {error && <div className="rlm-error">{error}</div>}
      <pre className="rlm-body" ref={bodyRef}>
        {view || (search.trim() ? '(no matching lines)' : '(waiting for output…)')}
      </pre>
      <div className="rlm-foot">
        {spec.follow ? 'live tail · auto-refresh 2s' : 'full log'}
        {truncated && ' (showing last 25 MB)'}
        {spec.file && !isLocalhost && <> · <code>{target.remoteDir}/{spec.file}</code></>}
      </div>
    </div>
  );
};

export const RemoteLogsModal: React.FC<{
  target: RemoteTarget;
  isLocalhost: boolean;
  onClose: () => void;
}> = ({ target, isLocalhost, onClose }) => {
  const [files, setFiles] = useState<string[]>([]);
  const [listError, setListError] = useState<string | null>(null);
  const [loading, setLoading] = useState(true);
  const [layout, setLayout] = useState<Layout>(() => loadLayout() || { dir: 'row', panes: [] });

  // Layout (split direction + which files are open) survives dismiss/reopen.
  useEffect(() => { if (layout.panes.length) saveLayout(layout); }, [layout]);

  useEffect(() => {
    const onKey = (e: KeyboardEvent) => { if (e.key === 'Escape') onClose(); };
    window.addEventListener('keydown', onKey);
    return () => window.removeEventListener('keydown', onKey);
  }, [onClose]);

  const refreshFiles = useCallback(async () => {
    setLoading(true);
    try {
      const res = await remoteLogList(target);
      if (res.error) {
        setListError(res.error);
        setFiles([]);
        return;
      }
      const list = res.files || [];
      setFiles(list);
      setListError(list.length ? null : 'No *.log files found — is the stack running?');
      // Drop panes whose file vanished; seed one pane on first open.
      setLayout(prev => {
        const kept = prev.panes.filter(p => list.includes(p.file));
        if (kept.length) return { ...prev, panes: kept };
        const first = list.find(f => f.includes('rider-app-exe')) || list[0] || '';
        return { ...prev, panes: [{ id: newId(), file: first, follow: true, sort: 'none' }] };
      });
    } catch (e) {
      setListError(`Failed to list logs: ${e}`);
    } finally {
      setLoading(false);
    }
  }, [target]);

  useEffect(() => { refreshFiles(); }, [refreshFiles]);

  const patchPane = (id: string, patch: Partial<PaneSpec>) =>
    setLayout(prev => ({ ...prev, panes: prev.panes.map(p => (p.id === id ? { ...p, ...patch } : p)) }));

  const splitPane = (id: string, dir: SplitDir) =>
    setLayout(prev => {
      const idx = prev.panes.findIndex(p => p.id === id);
      if (idx < 0) return prev;
      const src = prev.panes[idx];
      const next = prev.panes.slice();
      next.splice(idx + 1, 0, { ...src, id: newId() });
      return { dir, panes: next };
    });

  const closePane = (id: string) =>
    setLayout(prev => (prev.panes.length <= 1
      ? prev
      : { ...prev, panes: prev.panes.filter(p => p.id !== id) }));

  return (
    <div className="rlm-overlay" onClick={onClose}>
      <div className="rlm-modal" onClick={e => e.stopPropagation()} role="dialog" aria-label="Service logs">
        <div className="rlm-head">
          <span className="rlm-title">Service logs</span>
          <span className="rlm-src">{isLocalhost ? 'local' : `${target.user}@${target.host}`}</span>
          <button className="rlm-btn" onClick={refreshFiles} disabled={loading} title="Re-scan the workspace for *.log files">
            {loading ? 'Scanning…' : 'Rescan files'}
          </button>
          <span className="rlm-spacer" />
          <span className="rlm-hint">{layout.panes.length} pane{layout.panes.length === 1 ? '' : 's'} · Esc to close</span>
          <button className="rlm-btn rlm-btn-icon" onClick={onClose} title="Close (Esc)" aria-label="Close">✕</button>
        </div>

        {listError && <div className="rlm-error rlm-error-top">{listError}</div>}

        <div className={`rlm-panes rlm-panes-${layout.dir}`}>
          {layout.panes.map(p => (
            <LogPane
              key={p.id}
              spec={p}
              files={files}
              target={target}
              isLocalhost={isLocalhost}
              canClose={layout.panes.length > 1}
              onChange={patch => patchPane(p.id, patch)}
              onSplit={dir => splitPane(p.id, dir)}
              onClose={() => closePane(p.id)}
            />
          ))}
        </div>
      </div>
    </div>
  );
};
