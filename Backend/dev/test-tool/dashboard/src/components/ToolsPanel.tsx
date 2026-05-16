import React, { useEffect, useState, useCallback } from 'react';
import {
  listLaunchers, getLauncher, setInputs, setSource, runWorkflow,
  runStage, stopStage, logStreamUrl, fileToBase64,
} from '../services/launcher';
import {
  LauncherSummary, LauncherDetailPayload, SpecInput, StageStatus, SpecLog,
} from '../types/launcher';
import './ToolsPanel.css';

export interface ToolsPanelProps {
  /** Heading shown above the grid. Defaults to "Systems". */
  title?: string;
  /** Only show launchers whose category is in this list. Defaults to all categories. */
  categories?: string[];
}

export const ToolsPanel: React.FC<ToolsPanelProps> = ({ title = 'Systems', categories }) => {
  const [launchers, setLaunchers] = useState<LauncherSummary[]>([]);
  const [selected, setSelected] = useState<string | null>(null);
  const [error, setError] = useState<string | null>(null);

  const refresh = useCallback(async () => {
    try {
      const all = await listLaunchers();
      setLaunchers(categories ? all.filter(l => categories.includes(l.category || 'other')) : all);
      setError(null);
    } catch (e: any) { setError(e.message || String(e)); }
  }, [categories]);

  useEffect(() => { refresh(); }, [refresh]);

  if (selected) {
    return (
      <LauncherDetail
        slug={selected}
        onBack={() => { setSelected(null); refresh(); }}
      />
    );
  }

  return (
    <div className="tools-panel">
      <div className="tools-header">
        <h2>{title}</h2>
        <button className="btn" onClick={refresh}>Refresh</button>
      </div>
      {error && <div className="tools-error">{error}</div>}
      {launchers.length === 0 && !error && (
        <div className="tools-empty">
          No launchers found. Add a YAML spec under{' '}
          <code>Backend/dev/test-tool/specs/</code>.
        </div>
      )}
      <div className="tools-grid">
        {launchers.map(l => (
          <button
            key={l.name}
            className="tool-card"
            onClick={() => setSelected(l.name)}
          >
            <div className="tool-card-title">{l.title}</div>
            <div className="tool-card-category">{l.category || 'other'}</div>
            {l.tags && l.tags.length > 0 && (
              <div className="tool-card-tags">
                {l.tags.map(t => <span key={t} className="tool-card-tag">{t}</span>)}
              </div>
            )}
            {l.ports && l.ports.length > 0 && (
              <div className="tool-card-ports">
                {l.ports.slice(0, 4).map(p => (
                  <span key={p.name} className="tool-card-port">
                    {p.name}:{p.port}
                  </span>
                ))}
              </div>
            )}
          </button>
        ))}
      </div>
    </div>
  );
};

// ── Detail view ─────────────────────────────────────────────────────────────

const LauncherDetail: React.FC<{ slug: string; onBack: () => void }> = ({ slug, onBack }) => {
  const [data, setData] = useState<LauncherDetailPayload | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [busy, setBusy] = useState<string | null>(null);
  const [activeLog, setActiveLog] = useState<string | null>(null);

  const refresh = useCallback(async () => {
    try { setData(await getLauncher(slug)); setError(null); }
    catch (e: any) { setError(e.message || String(e)); }
  }, [slug]);

  useEffect(() => { refresh(); }, [refresh]);
  useEffect(() => {
    const id = window.setInterval(refresh, 3000);
    return () => window.clearInterval(id);
  }, [refresh]);

  if (!data) {
    return (
      <div className="tools-panel">
        <button className="btn" onClick={onBack}>← Back</button>
        {error ? <div className="tools-error">{error}</div> : <div>Loading…</div>}
      </div>
    );
  }

  const spec = data.spec;
  const stageById: Record<string, StageStatus> = {};
  for (const s of data.stages) stageById[s.id] = s;

  const handleStage = async (stageId: string, force = false) => {
    setBusy(stageId);
    try { await runStage(slug, stageId, force); await refresh(); }
    catch (e: any) { setError(e.message || String(e)); }
    finally { setBusy(null); }
  };
  const handleStop = async (stageId: string) => {
    setBusy(stageId);
    try { await stopStage(slug, stageId); await refresh(); }
    catch (e: any) { setError(e.message || String(e)); }
    finally { setBusy(null); }
  };
  const handleWorkflow = async (name: string) => {
    setBusy(`wf:${name}`);
    try { await runWorkflow(slug, name); await refresh(); }
    catch (e: any) { setError(e.message || String(e)); }
    finally { setBusy(null); }
  };

  return (
    <div className="tools-panel">
      <div className="tools-header">
        <button className="btn" onClick={onBack}>← Back</button>
        <h2>{spec.title}</h2>
        <button className="btn" onClick={refresh}>Refresh</button>
      </div>
      {error && <div className="tools-error">{error}</div>}

      <Section title="Source">
        <SourceEditor
          slug={slug}
          url={spec.source?.url}
          ref={spec.source?.ref}
          destDir={spec.source?.destDir}
          onChanged={refresh}
        />
      </Section>

      {spec.inputs && spec.inputs.length > 0 && (
        <Section title="Inputs">
          <InputsForm
            slug={slug}
            inputs={spec.inputs}
            current={data.inputs}
            missing={data.missingRequired}
            onSaved={refresh}
          />
        </Section>
      )}

      {spec.workflows && Object.keys(spec.workflows).length > 0 && (
        <Section title="Workflows">
          <div className="tool-row">
            {Object.entries(spec.workflows).map(([name, stages]) => (
              <button
                key={name}
                className="btn btn-primary"
                disabled={busy !== null}
                onClick={() => handleWorkflow(name)}
                title={stages.join(' → ')}
              >
                ▶ {name}
              </button>
            ))}
          </div>
        </Section>
      )}

      <Section title="Stages">
        <table className="stages-table">
          <thead>
            <tr><th>Stage</th><th>State</th><th>Lifecycle</th><th>Last exit</th><th>Actions</th></tr>
          </thead>
          <tbody>
            {spec.stages.map(st => {
              const s = stageById[st.id];
              const state = s?.state || 'idle';
              return (
                <tr key={st.id} className={`stage-row state-${state}`}>
                  <td><code>{st.id}</code>{st.builtin && <span className="stage-builtin"> (builtin: {st.builtin})</span>}</td>
                  <td><span className={`stage-state state-${state}`}>{state}</span>{s?.stale ? ' · stale' : ''}</td>
                  <td>{st.lifecycle || 'one-shot'}</td>
                  <td>{s?.lastExit === null || s?.lastExit === undefined ? '—' : s.lastExit}</td>
                  <td>
                    <button
                      className="btn btn-sm"
                      disabled={busy !== null}
                      onClick={() => handleStage(st.id, false)}
                    >Run</button>
                    <button
                      className="btn btn-sm"
                      disabled={busy !== null}
                      onClick={() => handleStage(st.id, true)}
                      title="Force re-run"
                    >Force</button>
                    {state === 'running' && (
                      <button
                        className="btn btn-sm btn-danger"
                        disabled={busy !== null}
                        onClick={() => handleStop(st.id)}
                      >Stop</button>
                    )}
                  </td>
                </tr>
              );
            })}
          </tbody>
        </table>
      </Section>

      {spec.logs && spec.logs.length > 0 && (
        <Section title="Logs">
          <div className="log-tabs">
            {spec.logs.map(l => (
              <button
                key={l.name}
                className={`log-tab ${activeLog === l.name ? 'active' : ''}`}
                onClick={() => setActiveLog(activeLog === l.name ? null : l.name)}
              >{l.name} · {l.kind}</button>
            ))}
          </div>
          {activeLog && (
            <LiveLog
              key={activeLog}
              url={logStreamUrl(slug, activeLog)}
            />
          )}
        </Section>
      )}

      {spec.domains && spec.domains.length > 0 && (
        <Section title="Open">
          <div className="tool-row">
            {spec.domains.map(d => (
              <a key={d.name} className="btn" href={d.url} target="_blank" rel="noreferrer">
                {d.name} → {d.url}
              </a>
            ))}
          </div>
        </Section>
      )}
    </div>
  );
};

// ── Sub-components ──────────────────────────────────────────────────────────

const Section: React.FC<{ title: string; children: React.ReactNode }> = ({ title, children }) => (
  <section className="tool-section">
    <h3>{title}</h3>
    {children}
  </section>
);

const SourceEditor: React.FC<{
  slug: string;
  url?: string;
  ref?: string;
  destDir?: string;
  onChanged: () => void;
}> = ({ slug, url, ref: gitRef, destDir, onChanged }) => {
  const [editRef, setEditRef] = useState(gitRef || '');
  const [localPath, setLocalPath] = useState('');
  const [busy, setBusy] = useState(false);

  return (
    <div className="source-editor">
      <div className="source-line"><b>URL:</b> <code>{url || '—'}</code></div>
      <div className="source-line"><b>destDir:</b> <code>{destDir || '—'}</code></div>
      <div className="source-row">
        <label>ref</label>
        <input value={editRef} onChange={e => setEditRef(e.target.value)} placeholder="branch / tag / sha" />
        <button
          className="btn btn-sm"
          disabled={busy || editRef === gitRef}
          onClick={async () => { setBusy(true); try { await setSource(slug, { ref: editRef }); onChanged(); } finally { setBusy(false); } }}
        >Set ref</button>
      </div>
      <div className="source-row">
        <label>local path</label>
        <input value={localPath} onChange={e => setLocalPath(e.target.value)} placeholder="/abs/path/to/local/checkout" />
        <button
          className="btn btn-sm"
          disabled={busy || !localPath.trim()}
          onClick={async () => { setBusy(true); try { await setSource(slug, { localPath: localPath.trim() }); onChanged(); } finally { setBusy(false); } }}
        >Symlink</button>
        <button
          className="btn btn-sm"
          disabled={busy}
          onClick={async () => { setBusy(true); try { await setSource(slug, { localPath: null }); setLocalPath(''); onChanged(); } finally { setBusy(false); } }}
        >Unlink</button>
      </div>
    </div>
  );
};

const InputsForm: React.FC<{
  slug: string;
  inputs: SpecInput[];
  current: Record<string, any>;
  missing: string[];
  onSaved: () => void;
}> = ({ slug, inputs, current, missing, onSaved }) => {
  const [pending, setPending] = useState<Record<string, any>>({});
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const valueOf = (k: string): string => {
    const v = pending[k] !== undefined ? pending[k] : current[k];
    if (v == null) return '';
    if (typeof v === 'object') return ''; // descriptors don't render as text inputs
    return String(v);
  };

  const save = async () => {
    setBusy(true);
    setError(null);
    try {
      const payload: Record<string, any> = {};
      for (const [k, v] of Object.entries(pending)) {
        payload[k] = v;
      }
      if (Object.keys(payload).length > 0) {
        await setInputs(slug, payload);
        setPending({});
      }
      onSaved();
    } catch (e: any) {
      setError(e.message || String(e));
    } finally {
      setBusy(false);
    }
  };

  return (
    <div className="inputs-form">
      {error && <div className="tools-error">{error}</div>}
      {missing.length > 0 && (
        <div className="tools-warn">Required: {missing.join(', ')}</div>
      )}
      {inputs.map(i => {
        const isMissing = missing.includes(i.key);
        const descriptor = current[i.key];
        return (
          <div key={i.key} className={`input-row ${isMissing ? 'missing' : ''}`}>
            <label title={i.key}>
              {i.label}
              {i.required && <span className="req"> *</span>}
            </label>
            {i.type === 'file' ? (
              <div className="file-input-cell">
                <input
                  type="file"
                  accept={(i.accept || []).join(',')}
                  onChange={async e => {
                    const f = e.target.files?.[0];
                    if (!f) return;
                    const content_base64 = await fileToBase64(f);
                    setPending(p => ({ ...p, [i.key]: { filename: f.name, content_base64 } }));
                  }}
                />
                <span className="file-state">
                  {pending[i.key]
                    ? `pending: ${pending[i.key].filename}`
                    : descriptor?.set
                      ? (descriptor.filename ? `set: ${descriptor.filename}` : 'set')
                      : 'none'}
                </span>
              </div>
            ) : i.type === 'select' ? (
              <select
                value={valueOf(i.key) || (i.default as string) || ''}
                onChange={e => setPending(p => ({ ...p, [i.key]: { value: e.target.value } }))}
              >
                <option value="">—</option>
                {(i.options || []).map(o => <option key={o} value={o}>{o}</option>)}
              </select>
            ) : (
              <input
                type={i.type === 'secret-text' ? 'password' : (i.type === 'number' ? 'number' : 'text')}
                value={valueOf(i.key)}
                placeholder={i.default != null ? String(i.default) : ''}
                onChange={e => setPending(p => ({ ...p, [i.key]: { value: e.target.value } }))}
              />
            )}
          </div>
        );
      })}
      <div className="tool-row">
        <button
          className="btn btn-primary"
          disabled={busy || Object.keys(pending).length === 0}
          onClick={save}
        >Save inputs</button>
        {Object.keys(pending).length > 0 && (
          <button className="btn" disabled={busy} onClick={() => setPending({})}>Discard</button>
        )}
      </div>
    </div>
  );
};

const LiveLog: React.FC<{ url: string }> = ({ url }) => {
  const [lines, setLines] = useState<string[]>([]);
  const preRef = React.useRef<HTMLPreElement | null>(null);

  useEffect(() => {
    setLines([]);
    const es = new EventSource(url);
    const decoder = new TextDecoder('utf-8');
    es.onmessage = (e) => {
      try {
        const obj = JSON.parse(e.data);
        if (obj.b64) {
          const bin = atob(obj.b64);
          const bytes = new Uint8Array(bin.length);
          for (let i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i);
          const text = decoder.decode(bytes);
          setLines(prev => {
            const merged = (prev.join('') + text).split('\n');
            const tail = merged.slice(-1000);
            return tail;
          });
        }
      } catch { /* ignore */ }
    };
    es.onerror = () => { /* SSE auto-reconnects */ };
    return () => es.close();
  }, [url]);

  useEffect(() => {
    if (preRef.current) preRef.current.scrollTop = preRef.current.scrollHeight;
  }, [lines]);

  return <pre ref={preRef} className="live-log">{lines.join('\n')}</pre>;
};
