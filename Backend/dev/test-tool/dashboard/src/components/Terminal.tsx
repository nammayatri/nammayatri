import React, { useEffect, useRef } from 'react';
import { Terminal as XTerm } from '@xterm/xterm';
import { FitAddon } from '@xterm/addon-fit';
import '@xterm/xterm/css/xterm.css';
import './Terminal.css';
import { PROXY_BASE } from '../config';

// Background is pure black on purpose. Full-screen TUIs (process-compose)
// paint every cell with an explicit black background on their first draw, but
// after a re-attach only *changed* cells are repainted — every untouched cell
// falls back to this default. Anything other than #000 makes the screen look
// washed out until the TUI happens to redraw those cells.
const MONOKAI_THEME = {
  background: '#000000',
  foreground: '#d4d4d4',
  cursor: '#aeafad',
  cursorAccent: '#000000',
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
  const serverRepaintTimerRef = useRef<number | null>(null);
  const disposedRef = useRef(false);

  useEffect(() => {
    // Reset disposed flag for this effect run (important when effect re-runs).
    disposedRef.current = false;

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

    // React StrictMode double-invokes effects (mount → unmount → remount).
    // The Viewport constructor schedules setTimeout(()=>this.syncScrollArea()).
    // On StrictMode unmount, term.dispose() clears _renderer.value to undefined.
    // The old Viewport's setTimeout then fires after dispose and calls
    // syncScrollArea → get dimensions() → _renderer.value.dimensions → crash
    // ("Cannot read properties of undefined (reading 'dimensions')").
    //
    // Fix: open(), then immediately wrap syncScrollArea on the Viewport instance
    // so any post-dispose calls are silently swallowed before they propagate.
    try { term.open(containerRef.current); } catch { /* ignore sync throw */ }
    try {
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      const vp = (term as any)._core?.viewport;
      if (vp && typeof vp.syncScrollArea === 'function') {
        const _orig = vp.syncScrollArea.bind(vp);
        vp.syncScrollArea = (...args: unknown[]) => { try { _orig(...args); } catch { /* suppress */ } };
      }
    } catch { /* if _core/viewport aren't accessible, fall through */ }

    // ── Sizing ──
    // Tabs are hidden with `display: none`, which makes the container measure
    // 0×0. Fitting against that yields a 1–2 cell geometry, and term.onResize
    // would push it to the PTY — the remote TUI then really does redraw itself
    // at that size, which is what showed up as a mangled screen after a tab
    // switch. So: never fit unless the element has a real layout box, and
    // ignore nonsense proposals. On hidden → visible, re-fit and repaint from
    // xterm's own buffer (it keeps the screen while hidden; no server involved).
    const hasLayout = () => {
      const el = containerRef.current;
      return !!el && el.isConnected && el.offsetWidth > 0 && el.offsetHeight > 0;
    };

    const safeFit = () => {
      if (!hasLayout()) return false;
      try {
        const dims = fit.proposeDimensions();
        if (!dims || !Number.isFinite(dims.cols) || !Number.isFinite(dims.rows)) return false;
        if (dims.cols < 20 || dims.rows < 5) return false;
        if (dims.cols !== term.cols || dims.rows !== term.rows) fit.fit();
        return true;
      } catch {
        return false;
      }
    };

    const repaint = () => {
      try { term.refresh(0, term.rows - 1); } catch { /* ignore */ }
    };

    // Server-side repaint: ask the TUI itself to redraw every cell. term.refresh
    // only re-renders what xterm already holds, which cannot remove junk the
    // TUI does not know it left behind (the stray border column after a
    // re-attach). Debounced, because it is only correct once the geometry has
    // stopped moving — a pulse at an intermediate fit width recreates the very
    // artefact it is meant to clear.
    const requestServerRepaint = () => {
      const sid = sessionRef.current;
      if (!sid) return;
      if (serverRepaintTimerRef.current) window.clearTimeout(serverRepaintTimerRef.current);
      serverRepaintTimerRef.current = window.setTimeout(() => {
        if (disposedRef.current) return;
        fetch(`${apiBase}/repaint`, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ session: sessionRef.current }),
        }).catch(() => { /* endpoint may not exist for this prefix — harmless */ });
      }, 450);
    };

    // Defer the first fit to the next animation frame so the flex container
    // has settled its dimensions before FitAddon measures it.
    requestAnimationFrame(() => {
      if (!disposedRef.current) safeFit();
    });

    termRef.current = term;
    fitRef.current = fit;

    if (attachSessionId) {
      sessionRef.current = attachSessionId;
    }

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

        // Re-fit now that we have a session so the PTY gets the real geometry.
        safeFit();
        try {
          await fetch(`${apiBase}/resize`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ session: sessionId, cols: term.cols, rows: term.rows }),
          });
        } catch { /* swallow */ }

        const es = new EventSource(`${apiBase}/stream?session=${encodeURIComponent(sessionId)}`);
        esRef.current = es;
        // Re-attach: the stream's own pulse fires at whatever geometry the PTY
        // had when it opened. Follow up once this terminal's fit has settled.
        if (attachSessionId) requestServerRepaint();
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
        requestServerRepaint();
      }, 120);
    });

    const onWindowResize = () => { safeFit(); };
    window.addEventListener('resize', onWindowResize);

    // Browser-level tab switches don't resize anything, but the renderer can
    // come back with a stale texture atlas — repaint from the buffer.
    const onVisibility = () => {
      if (document.visibilityState === 'visible' && hasLayout()) {
        safeFit();
        requestAnimationFrame(repaint);
      }
    };
    document.addEventListener('visibilitychange', onVisibility);

    const hiddenRef = { current: !hasLayout() };
    const ro = (typeof ResizeObserver !== 'undefined')
      ? new ResizeObserver(() => {
        if (!hasLayout()) { hiddenRef.current = true; return; }
        const wasHidden = hiddenRef.current;
        hiddenRef.current = false;
        safeFit();
        if (wasHidden) {
          requestAnimationFrame(repaint);
          requestServerRepaint();
        }
      })
      : null;
    if (ro && containerRef.current) ro.observe(containerRef.current);

    term.focus();
    start();

    return () => {
      disposedRef.current = true;
      onData.dispose();
      onResize.dispose();
      window.removeEventListener('resize', onWindowResize);
      document.removeEventListener('visibilitychange', onVisibility);
      if (ro) ro.disconnect();
      if (resizeTimerRef.current) window.clearTimeout(resizeTimerRef.current);
      if (serverRepaintTimerRef.current) window.clearTimeout(serverRepaintTimerRef.current);
      const sid = sessionRef.current;
      if (esRef.current) { try { esRef.current.close(); } catch { /* ignore */ } }
      // Only kill sessions we created. When `attachSessionId` was provided,
      // the parent (e.g. RemoteStackPanel reattaching after a page refresh /
      // tab switch) owns the lifecycle — killing it here would tear down a
      // long-running stack the user wants to keep alive.
      if (sid && !attachSessionId) {
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

  const refocus = () => {
    try { termRef.current?.focus(); } catch { /* ignore */ }
  };

  return (
    <div className="tb-term-root" onPointerDown={refocus}>
      <div className="tb-term-host" ref={containerRef} />
    </div>
  );
};
