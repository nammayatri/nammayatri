import React, { useEffect, useRef } from 'react';
import { Terminal as XTerm } from 'xterm';
import { FitAddon } from '@xterm/addon-fit';
import 'xterm/css/xterm.css';
import './Terminal.css';
import { PROXY_BASE } from '../config';

const MONOKAI_THEME = {
  background: '#1e1e1e',
  foreground: '#d4d4d4',
  cursor: '#aeafad',
  cursorAccent: '#1e1e1e',
  selectionBackground: '#264f78',
  black: '#000000',
  red: '#f44747',
  green: '#6a9955',
  yellow: '#d7ba7d',
  blue: '#569cd6',
  magenta: '#c586c0',
  cyan: '#4ec9b0',
  white: '#d4d4d4',
  brightBlack: '#666666',
  brightRed: '#f44747',
  brightGreen: '#b5cea8',
  brightYellow: '#dcdcaa',
  brightBlue: '#9cdcfe',
  brightMagenta: '#c586c0',
  brightCyan: '#4ec9b0',
  brightWhite: '#ffffff',
};

function b64encode(s: string): string {
  const bytes = new TextEncoder().encode(s);
  let bin = '';
  for (let i = 0; i < bytes.length; i++) bin += String.fromCharCode(bytes[i]);
  return btoa(bin);
}

function b64decodeToBytes(b64: string): Uint8Array {
  const bin = atob(b64);
  const out = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) out[i] = bin.charCodeAt(i);
  return out;
}

export interface TerminalProps {
  onClose?: () => void;
  /** Base URL of the server hosting the terminal endpoints. Defaults to PROXY_BASE (test-context-api). */
  baseUrl?: string;
  /** Path prefix on that server. Defaults to "/api/terminal". Use "/api/remote" for local-api SSH/PTY. */
  pathPrefix?: string;
  /** If set, attach to an existing session id (skip POST .../start). */
  attachSessionId?: string;
}

export const Terminal: React.FC<TerminalProps> = ({
  onClose,
  baseUrl,
  pathPrefix = '/api/terminal',
  attachSessionId,
}) => {
  const apiBase = (baseUrl ?? PROXY_BASE) + pathPrefix;
  const containerRef = useRef<HTMLDivElement | null>(null);
  const termRef = useRef<XTerm | null>(null);
  const fitRef = useRef<FitAddon | null>(null);
  const sessionRef = useRef<string | null>(null);
  const esRef = useRef<EventSource | null>(null);
  const resizeTimerRef = useRef<number | null>(null);
  const disposedRef = useRef(false);

  useEffect(() => {
    if (!containerRef.current) return;

    const term = new XTerm({
      cursorBlink: true,
      fontFamily: 'ui-monospace, SFMono-Regular, "SF Mono", Menlo, Consolas, monospace',
      fontSize: 13,
      lineHeight: 1.2,
      theme: MONOKAI_THEME,
      scrollback: 5000,
      convertEol: false,
      allowProposedApi: true,
    });
    const fit = new FitAddon();
    term.loadAddon(fit);
    term.open(containerRef.current);
    try { fit.fit(); } catch { /* ignore */ }
    termRef.current = term;
    fitRef.current = fit;

    const decoder = new TextDecoder('utf-8');

    const start = async () => {
      try {
        let sessionId: string | undefined = attachSessionId;
        if (!sessionId) {
          const cols = term.cols || 80;
          const rows = term.rows || 24;
          const res = await fetch(`${apiBase}/start`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ cols, rows }),
          });
          const data = await res.json();
          if (disposedRef.current) return;
          if (!res.ok || !data.session) {
            term.write(`\r\n\x1b[31mFailed to start terminal: ${data.error || res.status}\x1b[0m\r\n`);
            return;
          }
          sessionId = data.session;
        }
        if (!sessionId) return;
        sessionRef.current = sessionId;

        const es = new EventSource(`${apiBase}/stream?session=${encodeURIComponent(sessionId)}`);
        esRef.current = es;
        es.onmessage = (ev) => {
          try {
            const msg = JSON.parse(ev.data);
            if (msg.b64) {
              const bytes = b64decodeToBytes(msg.b64);
              term.write(decoder.decode(bytes, { stream: true }));
            }
          } catch { /* ignore */ }
        };
        es.addEventListener('end', (ev: MessageEvent) => {
          try {
            const msg = JSON.parse((ev as MessageEvent).data || '{}');
            term.write(`\r\n\x1b[90m[process exited${msg.exit != null ? ' with code ' + msg.exit : ''}]\x1b[0m\r\n`);
          } catch { /* ignore */ }
          es.close();
        });
        es.onerror = () => {
          /* the browser auto-reconnects EventSource; we only react on visible close */
        };
      } catch (e) {
        term.write(`\r\n\x1b[31mError starting terminal: ${String(e)}\x1b[0m\r\n`);
      }
    };

    const onData = term.onData((d) => {
      const sid = sessionRef.current;
      if (!sid) return;
      fetch(`${apiBase}/input`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ session: sid, data: b64encode(d) }),
      }).catch(() => { /* swallow */ });
    });

    const onResize = term.onResize(({ cols, rows }) => {
      const sid = sessionRef.current;
      if (!sid) return;
      if (resizeTimerRef.current) window.clearTimeout(resizeTimerRef.current);
      resizeTimerRef.current = window.setTimeout(() => {
        fetch(`${apiBase}/resize`, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ session: sid, cols, rows }),
        }).catch(() => { /* swallow */ });
      }, 120);
    });

    const onWindowResize = () => {
      try { fit.fit(); } catch { /* ignore */ }
    };
    window.addEventListener('resize', onWindowResize);

    const ro = (typeof ResizeObserver !== 'undefined')
      ? new ResizeObserver(() => { try { fit.fit(); } catch { /* ignore */ } })
      : null;
    if (ro && containerRef.current) ro.observe(containerRef.current);

    term.focus();
    start();

    return () => {
      disposedRef.current = true;
      onData.dispose();
      onResize.dispose();
      window.removeEventListener('resize', onWindowResize);
      if (ro) ro.disconnect();
      if (resizeTimerRef.current) window.clearTimeout(resizeTimerRef.current);
      const sid = sessionRef.current;
      if (esRef.current) { try { esRef.current.close(); } catch { /* ignore */ } }
      if (sid) {
        fetch(`${apiBase}/kill`, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ session: sid }),
          keepalive: true,
        }).catch(() => { /* swallow */ });
      }
      try { term.dispose(); } catch { /* ignore */ }
      termRef.current = null;
      fitRef.current = null;
      sessionRef.current = null;
      esRef.current = null;
    };
    // Re-create the terminal when the target session changes.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [apiBase, attachSessionId]);

  return (
    <div className="tb-term-root">
      <div className="tb-term-host" ref={containerRef} />
    </div>
  );
};
