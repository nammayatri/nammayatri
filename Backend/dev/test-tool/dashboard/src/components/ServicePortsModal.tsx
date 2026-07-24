import React, { useCallback, useEffect, useState } from 'react';
import { fetchDevboxPorts, DevboxPortsResponse } from '../services/remote';
import { LOCAL_API_BASE, getContextApiBaseOverride, getContextApiBaseDefault } from '../config';
import './ServicePortsModal.css';

const CopyButton: React.FC<{ value: string }> = ({ value }) => {
  const [done, setDone] = useState(false);
  const copy = async () => {
    try {
      await navigator.clipboard.writeText(value);
    } catch {
      const ta = document.createElement('textarea');
      ta.value = value;
      document.body.appendChild(ta);
      ta.select();
      document.execCommand('copy');
      document.body.removeChild(ta);
    }
    setDone(true);
    window.setTimeout(() => setDone(false), 1200);
  };
  return (
    <button className="spm-copy" onClick={copy} title={`Copy ${value}`}>
      {done ? '✓' : '⧉'}
    </button>
  );
};

const UrlRow: React.FC<{ name: string; url: string; note?: string }> = ({ name, url, note }) => (
  <tr>
    <td className="spm-name">{name}</td>
    <td className="spm-url">
      <a href={url} target="_blank" rel="noopener noreferrer">{url}</a>
      {note && <span className="spm-note">{note}</span>}
    </td>
    <td className="spm-actions"><CopyButton value={url} /></td>
  </tr>
);

export const ServicePortsModal: React.FC<{ onClose: () => void }> = ({ onClose }) => {
  const [data, setData] = useState<DevboxPortsResponse | null>(null);
  const [loading, setLoading] = useState(true);
  const [filter, setFilter] = useState('');

  const load = useCallback(async (refresh = false) => {
    setLoading(true);
    try { setData(await fetchDevboxPorts({ refresh })); }
    catch (e: any) { setData({ error: e?.message || String(e) }); }
    finally { setLoading(false); }
  }, []);

  useEffect(() => { load(); }, [load]);

  useEffect(() => {
    const onKey = (e: KeyboardEvent) => { if (e.key === 'Escape') onClose(); };
    window.addEventListener('keydown', onKey);
    return () => window.removeEventListener('keydown', onKey);
  }, [onClose]);

  const host = data?.host || 'localhost';
  const ports = data?.ports || {};
  const caddyPort = data?.caddyPort ?? null;
  const routes = data?.caddyRoutes || [];
  const match = (s: string) => !filter.trim() || s.toLowerCase().includes(filter.trim().toLowerCase());

  const direct = Object.entries(ports)
    .filter(([name]) => match(name))
    .sort(([a], [b]) => a.localeCompare(b));
  const viaCaddy = routes.filter(match).sort((a, b) => a.localeCompare(b));

  return (
    <div className="spm-backdrop" onClick={onClose}>
      <div className="spm-modal" onClick={e => e.stopPropagation()}>
        <div className="spm-header">
          <h3>Service Ports</h3>
          <span className="spm-host">
            stack host <code>{host}</code>
            {data?.devKey && <> · <code>{data.devKey}</code></>}
          </span>
          <input
            className="spm-filter"
            placeholder="Filter services…"
            value={filter}
            onChange={e => setFilter(e.target.value)}
          />
          <button className="spm-btn" onClick={() => load(true)} disabled={loading}>
            {loading ? 'Loading…' : 'Refresh'}
          </button>
          <button className="spm-btn spm-close" onClick={onClose}>✕</button>
        </div>

        {data?.error && (
          <div className="spm-error">
            {data.error}
            <div className="spm-hint">Ports are published by the stack at <code>&lt;workspace&gt;/data/devbox-ports.json</code> — start the stack, then refresh.</div>
          </div>
        )}

        <div className="spm-body">
          <section>
            <h4>Dashboard endpoints</h4>
            <table className="spm-table">
              <tbody>
                <UrlRow name="local-api" url={LOCAL_API_BASE} />
                <UrlRow
                  name="context-api"
                  url={getContextApiBaseOverride() || getContextApiBaseDefault()}
                  note={getContextApiBaseOverride() ? 'override' : undefined}
                />
              </tbody>
            </table>
          </section>

          {caddyPort != null && (
            <section>
              <h4>
                Via Caddy <code>http://{host}:{caddyPort}</code>
                <CopyButton value={`http://${host}:${caddyPort}`} />
              </h4>
              <table className="spm-table">
                <tbody>
                  {viaCaddy.map(svc => (
                    <UrlRow
                      key={svc}
                      name={svc}
                      url={`http://${host}:${caddyPort}/${svc}/`}
                      note={svc === 'db-manager-frontend' ? 'login admin / admin' : undefined}
                    />
                  ))}
                  {viaCaddy.length === 0 && (
                    <tr><td colSpan={3} className="spm-empty">No caddy routes match.</td></tr>
                  )}
                </tbody>
              </table>
            </section>
          )}

          <section>
            <h4>Direct ports</h4>
            <table className="spm-table">
              <tbody>
                {direct.map(([name, port]) => (
                  <tr key={name}>
                    <td className="spm-name">{name}</td>
                    <td className="spm-port">{port}</td>
                    <td className="spm-url">
                      <a href={`http://${host}:${port}`} target="_blank" rel="noopener noreferrer">
                        http://{host}:{port}
                      </a>
                    </td>
                    <td className="spm-actions">
                      <CopyButton value={`http://${host}:${port}`} />
                      <CopyButton value={String(port)} />
                    </td>
                  </tr>
                ))}
                {direct.length === 0 && (
                  <tr><td colSpan={4} className="spm-empty">
                    {loading ? 'Loading…' : 'No ports available.'}
                  </td></tr>
                )}
              </tbody>
            </table>
          </section>
        </div>

        {data?.source && <div className="spm-footer">source: <code>{data.source}</code></div>}
      </div>
    </div>
  );
};
