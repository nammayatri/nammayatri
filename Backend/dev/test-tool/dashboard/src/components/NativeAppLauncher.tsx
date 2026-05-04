import React, { useEffect, useRef, useState } from 'react';
import './TopBarActions.css';
import { showAlert, showConfirm } from './Dialogs';
import { RefPicker } from './RefPicker';
import { loadUiState, saveUiState } from './uiState';

const PROXY_BASE = 'http://localhost:7082';

export interface NyRnStatus {
  running: boolean;
  ready: boolean;
  app?: string | null;
  platform?: string | null;
  variant?: string | null;
  apps_built: string[];
  started_at?: number | null;
  finished_at?: number | null;
  exit_code?: number | null;
  error?: string | null;
  log: string[];
  pid?: number | null;
}

interface NyRnOptions {
  apps: string[];
  platforms: string[];
  variants_by_app: { [app: string]: string[] };
  defaults: { platform: string; variant_by_app: { [app: string]: string } };
}

type DeviceLogTab = 'build' | 'metro' | 'console' | 'network' | 'all';

interface Props {
  /** "customer" or "driver" — the launcher's app slot */
  app: 'customer' | 'driver';
  /** Display label on the top-bar button (e.g. "Customer App") */
  label: string;
  /** Emoji prefix for the top-bar button */
  icon?: string;
}

/**
 * Self-contained per-app launcher: top-bar button + modal with
 * Platform/Brand selectors, Start/Stop/Re-Launch actions, and Build /
 * Console / Network log tabs. Two of these mount independently for
 * customer + driver — they share nothing except the underlying repo
 * clone and the emulator.
 */
export const NativeAppLauncher: React.FC<Props> = ({ app, label, icon = '📱' }) => {
  const [data, setData] = useState<NyRnStatus | null>(null);
  const [opts, setOpts] = useState<NyRnOptions | null>(null);
  const [modalOpen, setModalOpen] = useState(false);
  const [selPlatform, setSelPlatform] = useState<string>('android');
  const [selVariant, setSelVariant] = useState<string>('');
  const [selRef, setSelRef] = useState<string>('');
  // True from the moment Start is clicked until either the POST returns
  // OR the next status poll lands `data.running=true`. Bridges the
  // dead window where the POST is in flight and the button would
  // otherwise look idle.
  const [isStarting, setIsStarting] = useState(false);
  // Same idea but for the in-modal "Clear MMKV cache" action.
  const [isClearing, setIsClearing] = useState(false);
  const [tab, setTab] = useState<DeviceLogTab>('build');
  const [deviceLogs, setDeviceLogs] = useState<{ console: string[]; network: string[]; all: string[] }>({ console: [], network: [], all: [] });
  const [metroLog, setMetroLog] = useState<string[]>([]);
  const [logcatFilter, setLogcatFilter] = useState<string>('');
  const [logcatDiag, setLogcatDiag] = useState<{ adb_bin?: string; devices?: string; devices_err?: string; logcat_stderr?: string; logcat_rc?: number; error?: string } | null>(null);
  const logBoxRef = useRef<HTMLPreElement | null>(null);

  // One-time options fetch: variant lists + platform list + defaults.
  // Also pulls the persisted UI selection for THIS launcher slot ($app)
  // so a dashboard reload restores the user's last-picked platform /
  // variant / git ref instead of jumping back to the launcher's defaults.
  // Per-app key prefix keeps customer + driver independent.
  useEffect(() => {
    let cancelled = false;
    (async () => {
      const [optsRes, ui] = await Promise.all([
        fetch(`${PROXY_BASE}/api/ny-react-native/options`).then(r => r.ok ? r.json() : null).catch(() => null),
        loadUiState(),
      ]);
      if (cancelled) return;
      if (optsRes) {
        setOpts(optsRes as NyRnOptions);
        const d = optsRes as NyRnOptions;
        const uiPlatform = typeof ui[`nativeApp.${app}.platform`] === 'string'
          ? ui[`nativeApp.${app}.platform`] as string : null;
        const uiVariant = typeof ui[`nativeApp.${app}.variant`] === 'string'
          ? ui[`nativeApp.${app}.variant`] as string : null;
        const uiRef = typeof ui[`nativeApp.${app}.ref`] === 'string'
          ? ui[`nativeApp.${app}.ref`] as string : null;
        if (uiPlatform && (d.platforms ?? []).includes(uiPlatform)) {
          setSelPlatform(uiPlatform);
        } else if (d.defaults) {
          setSelPlatform(d.defaults.platform);
        }
        const allowedVariants = d.variants_by_app[app] ?? [];
        if (uiVariant && allowedVariants.includes(uiVariant)) {
          setSelVariant(uiVariant);
        } else if (d.defaults) {
          setSelVariant(d.defaults.variant_by_app?.[app] ?? (allowedVariants[0] ?? ''));
        }
        if (uiRef) setSelRef(uiRef);
      }
    })();
    return () => { cancelled = true; };
  }, [app]);

  // When variant list for this app changes (auto-detect after clone),
  // snap selection to a valid one.
  useEffect(() => {
    if (!opts) return;
    const allowed = opts.variants_by_app[app] ?? [];
    if (allowed.length && !allowed.includes(selVariant)) {
      setSelVariant(allowed[0]);
    }
  }, [opts, app, selVariant]);

  // Initial status fetch — recover state across page reloads.
  useEffect(() => {
    fetch(`${PROXY_BASE}/api/ny-react-native/status?app=${app}`)
      .then(r => r.json())
      .then((d: NyRnStatus) => setData(d))
      .catch(() => { });
  }, [app]);

  // Poll this app's status while running OR while modal open.
  useEffect(() => {
    const polling = data?.running || modalOpen;
    if (!polling) return;
    const t = setInterval(async () => {
      try {
        const r = await fetch(`${PROXY_BASE}/api/ny-react-native/status?app=${app}`);
        setData(await r.json());
      } catch { /* keep polling */ }
    }, 2000);
    return () => clearInterval(t);
  }, [app, data?.running, modalOpen]);

  // Auto-scroll active log box.
  useEffect(() => {
    if (modalOpen && logBoxRef.current) {
      logBoxRef.current.scrollTop = logBoxRef.current.scrollHeight;
    }
  }, [data?.log, deviceLogs, metroLog, tab, modalOpen, logcatFilter]);

  // Tail device logcat (console / network / all) while modal is open
  // and a logcat tab is active. NOTE: logcat is device-wide, not per-
  // app — both launchers see the same log stream.
  useEffect(() => {
    if (!modalOpen) return;
    if (tab !== 'console' && tab !== 'network' && tab !== 'all') return;
    let cancelled = false;
    const fetchLogs = async () => {
      try {
        // Send ?app=… so the server can resolve the platform (ios vs
        // android) and route to xcrun simctl log show vs adb logcat.
        const r = await fetch(`${PROXY_BASE}/api/ny-react-native/logcat?type=${tab}&app=${app}&lines=600`);
        const d = await r.json();
        if (cancelled) return;
        setDeviceLogs(prev => ({ ...prev, [tab]: Array.isArray(d.lines) ? d.lines : [] }));
        setLogcatDiag(d.diag ?? null);
      } catch { /* keep trying */ }
    };
    fetchLogs();
    const t = setInterval(fetchLogs, 2000);
    return () => { cancelled = true; clearInterval(t); };
  }, [modalOpen, tab]);

  // Tail Metro's own log (compile progress, bundle requests, transform
  // errors). Build log stops streaming after `ready`; Metro keeps going.
  useEffect(() => {
    if (!modalOpen || tab !== 'metro') return;
    let cancelled = false;
    const fetchMetro = async () => {
      try {
        const r = await fetch(`${PROXY_BASE}/api/ny-react-native/metro-log?app=${app}&lines=400`);
        const d = await r.json();
        if (cancelled) return;
        setMetroLog(Array.isArray(d.lines) ? d.lines : []);
      } catch { /* keep trying */ }
    };
    fetchMetro();
    const t = setInterval(fetchMetro, 2000);
    return () => { cancelled = true; clearInterval(t); };
  }, [modalOpen, tab, app]);

  const handleStart = async () => {
    if (isStarting) return;
    setIsStarting(true);
    try {
      const r = await fetch(`${PROXY_BASE}/api/ny-react-native/start`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          app,
          platform: selPlatform,
          variant: selVariant,
          ref: selRef.trim() || undefined,
        }),
      });
      if (r.ok) {
        setData(prev => ({
          ...(prev ?? { apps_built: [], log: [], ready: false }),
          running: true, ready: false,
          app, platform: selPlatform, variant: selVariant,
        } as NyRnStatus));
      } else {
        const err = await r.json().catch(() => ({}));
        if (!err?.error?.includes('already running')) {
          showAlert(`${err?.error ?? `HTTP ${r.status}`}`, { title: `Could not start ${label}`, variant: 'danger' });
        }
      }
    } catch (e) {
      showAlert(String(e), { title: `Could not start ${label}`, variant: 'danger' });
    } finally {
      setIsStarting(false);
    }
  };

  const handleOpenDebugger = async () => {
    try {
      const r = await fetch(`${PROXY_BASE}/api/ny-react-native/open-debugger`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ app }),
      });
      const d = await r.json().catch(() => ({}));
      if (r.ok && d.opened) {
        // Metro launched Chrome itself — nothing to do here.
        return;
      }
      // Fallback: surface the discovery URL so the user can pick the
      // Hermes target manually.
      if (d.fallback_url) {
        window.open(d.fallback_url, '_blank', 'noopener,noreferrer');
        showAlert(
          `${d.error ?? 'open-debugger failed'}\n\nOpened ${d.fallback_url} — click the devtoolsFrontendUrl for the Hermes target there.`,
          { title: 'DevTools fallback', variant: 'info' },
        );
      } else {
        showAlert(`${d?.error ?? `HTTP ${r.status}`}`, { title: 'Could not open DevTools', variant: 'danger' });
      }
    } catch (e) {
      showAlert(String(e), { title: 'Could not open DevTools', variant: 'danger' });
    }
  };

  const handleClearCache = async () => {
    if (isClearing) return;
    const ok = await showConfirm(
      `Clear ${label} app cache?\n\n` +
      `Wipes MMKV / Documents / Preferences. Auth tokens and any stored ` +
      `BASE_URL get reset, so the next launch starts fresh.`,
      { title: `Clear ${label} cache`, confirmLabel: 'Clear cache', variant: 'danger' },
    );
    if (!ok) return;
    setIsClearing(true);
    try {
      const r = await fetch(`${PROXY_BASE}/api/ny-react-native/clear-cache`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ app }),
      });
      const d = await r.json().catch(() => ({}));
      if (r.ok && d.cleared) {
        showAlert(
          `${label} app cache cleared.\n\n` +
          (d.platform === 'android'
            ? `pm clear ${d.package} ✓`
            : `Wiped: ${(d.wiped ?? []).join(', ') || '(nothing)'}\n` +
              `Bundle: ${d.bundle_id}`) +
          `\n\nTap the app icon on the device to relaunch with empty MMKV.`,
          { title: 'Cache cleared', variant: 'success' },
        );
      } else {
        showAlert(
          `${d?.error ?? `HTTP ${r.status}`}${d?.hint ? `\n\n${d.hint}` : ''}`,
          { title: `Could not clear cache`, variant: 'danger' },
        );
      }
    } catch (e) {
      showAlert(String(e), { title: `Could not clear cache`, variant: 'danger' });
    } finally {
      setIsClearing(false);
    }
  };

  const handleStop = async () => {
    const ok = await showConfirm(
      `Stop the running ${label} launcher?\n\nMetro and any in-flight gradle build will be killed.`,
      { title: `Stop ${label}`, confirmLabel: 'Stop', variant: 'danger' },
    );
    if (!ok) return;
    try {
      await fetch(`${PROXY_BASE}/api/ny-react-native/stop`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ app }),
      });
      setData(prev => prev ? { ...prev, running: false, ready: false } : prev);
    } catch (e) {
      showAlert(String(e), { title: `Could not stop ${label}`, variant: 'danger' });
    }
  };

  const elapsed = data?.started_at
    ? Math.floor(((data.finished_at ?? Date.now() / 1000) - data.started_at))
    : 0;

  const summary = data?.platform && data?.variant
    ? `${data.platform} · ${data.variant}`
    : `${selPlatform} · ${selVariant}`;

  return (
    <>
      <button
        className={`tb-btn tb-nyrn${data?.ready ? ' tb-ok' : data?.error ? ' tb-err' : ''}${data?.running ? ' tb-running' : ''}`}
        onClick={() => setModalOpen(true)}
        title={`Open ${label} launcher (configure + start / stop / re-launch). Current: ${summary}`}
      >
        {icon} {label}
        {data && (data.ready || data.running || data.error) && (
          <span style={{ marginLeft: 6, opacity: 0.85 }}>· {summary}</span>
        )}
        {data?.ready && <span className="tb-status-chip is-ready" style={{ marginLeft: 6 }}><span className="tb-status-chip-dot" />ready</span>}
        {data?.running && <span className="tb-status-chip is-running" style={{ marginLeft: 6 }}><span className="tb-status-chip-dot" />{elapsed}s</span>}
        {data?.error && <span className="tb-status-chip is-error" style={{ marginLeft: 6 }}><span className="tb-status-chip-dot" />error</span>}
      </button>

      {modalOpen && (
        <div className="tb-modal-backdrop" onClick={() => setModalOpen(false)}>
          <div className="tb-modal" onClick={e => e.stopPropagation()}>
            <div className="tb-modal-header">
              <span className="tb-modal-title">
                {label} — {data?.platform ?? selPlatform} · {data?.variant ?? selVariant}
                <span className={`tb-pill ${data?.ready ? 'tb-pill-ok' : data?.error ? 'tb-pill-err' : data?.running ? 'tb-pill-run' : ''}`}>
                  {data?.ready ? `ready · ${elapsed}s`
                    : data?.running ? `building · ${elapsed}s`
                      : data?.error ? 'error'
                        : data?.exit_code != null ? `exit ${data.exit_code}`
                          : 'idle'}
                </span>
              </span>
              <button className="tb-modal-close" onClick={() => setModalOpen(false)} title="Close">✕</button>
            </div>

            {opts && (
              <div className="tb-modal-form">
                <div className="tb-modal-form-field">
                  <label htmlFor={`${app}-platform`}>Platform</label>
                  <select
                    id={`${app}-platform`}
                    value={selPlatform}
                    onChange={e => { setSelPlatform(e.target.value); saveUiState({ [`nativeApp.${app}.platform`]: e.target.value }); }}
                    disabled={!!data?.running}
                  >
                    {opts.platforms.map(p => (<option key={p} value={p}>{p}</option>))}
                  </select>
                </div>
                <div className="tb-modal-form-field">
                  <label htmlFor={`${app}-variant`}>Brand / variant</label>
                  <select
                    id={`${app}-variant`}
                    value={selVariant}
                    onChange={e => { setSelVariant(e.target.value); saveUiState({ [`nativeApp.${app}.variant`]: e.target.value }); }}
                    disabled={!!data?.running}
                    title="Brand / product flavor (parsed from android/app/build.gradle once the repo is cloned)"
                  >
                    {(opts.variants_by_app[app] ?? []).map(v => (<option key={v} value={v}>{v}</option>))}
                  </select>
                </div>
                <div className="tb-modal-form-field">
                  <RefPicker
                    repo="nammayatri/ny-react-native"
                    value={selRef}
                    onChange={(v) => { setSelRef(v); saveUiState({ [`nativeApp.${app}.ref`]: v || null }); }}
                    disabled={!!data?.running}
                  />
                </div>
                <div className="tb-modal-form-actions">
                  {!data?.running && (
                    <button
                      className={`tb-btn tb-modal-form-primary${isStarting ? ' tb-busy' : ''}`}
                      onClick={handleStart}
                      disabled={isStarting}
                      title={`Build, install, and launch the ${label.toLowerCase()} app on the emulator`}
                    >
                      {isStarting ? (
                        <>
                          <span className="tb-spinner" aria-hidden="true" />
                          {' '}Starting…
                        </>
                      ) : (
                        <>▶ {data?.ready ? 'Re-Launch' : data?.error ? 'Retry' : 'Start'}</>
                      )}
                    </button>
                  )}
                  {data?.running && (
                    <button
                      className="tb-btn tb-err"
                      onClick={handleStop}
                      title={`Kill the ${label} launcher (Metro + gradle + adb)`}
                    >
                      ⏹ Stop
                    </button>
                  )}
                  {data?.running && (
                    <button
                      className={`tb-btn${isClearing ? ' tb-busy' : ''}`}
                      onClick={handleClearCache}
                      disabled={isClearing}
                      title={
                        `Wipe ${label.toLowerCase()} app's MMKV / Documents / ` +
                        `Preferences on the device. Android: pm clear; iOS: ` +
                        `delete Library/MMKV + terminate. App is not rebuilt — ` +
                        `tap the icon to relaunch with empty storage.`
                      }
                    >
                      {isClearing ? (
                        <>
                          <span className="tb-spinner" aria-hidden="true" />
                          {' '}Clearing…
                        </>
                      ) : (
                        <>🧹 Clear cache</>
                      )}
                    </button>
                  )}
                  {data?.ready && (
                    <button
                      className="tb-btn"
                      onClick={handleOpenDebugger}
                      title="Open React Native DevTools (Hermes inspector) in Chrome. Bypasses iOS NSLog ~1024-byte truncation — full untruncated console.log from the JS runtime."
                    >
                      🐛 Open DevTools
                    </button>
                  )}
                </div>
              </div>
            )}

            {data?.error && <div className="tb-modal-error">⚠ {data.error}</div>}

            <div className="tb-modal-tabs">
              <button
                className={`tb-modal-tab ${tab === 'build' ? 'active' : ''}`}
                onClick={() => setTab('build')}
                title="Setup script log: clone, install, prebuild, gradle, install, am start. Stops growing once the app is launched (ready)."
              >
                Build <span className="tb-modal-tab-count">{data?.log?.length ?? 0}</span>
              </button>
              <button
                className={`tb-modal-tab ${tab === 'metro' ? 'active' : ''}`}
                onClick={() => setTab('metro')}
                title="Metro server log: bundle compile progress, bundle requests, transform errors. Continues streaming after 'ready'."
              >
                Metro <span className="tb-modal-tab-count">{metroLog.length}</span>
              </button>
              <button
                className={`tb-modal-tab ${tab === 'console' ? 'active' : ''}`}
                onClick={() => setTab('console')}
                title="JS console.log + RN runtime errors (filtered logcat). Device-wide — both apps share these tags."
              >
                Console <span className="tb-modal-tab-count">{deviceLogs.console.length}</span>
              </button>
              <button
                className={`tb-modal-tab ${tab === 'network' ? 'active' : ''}`}
                onClick={() => setTab('network')}
                title="HTTP traffic (filtered logcat: OkHttp / Retrofit / Volley / WebSocket / URL strings). Device-wide."
              >
                Network <span className="tb-modal-tab-count">{deviceLogs.network.length}</span>
              </button>
              <button
                className={`tb-modal-tab ${tab === 'all' ? 'active' : ''}`}
                onClick={() => setTab('all')}
                title="Full device logcat (unfiltered). Use the filter box below to grep for any substring."
              >
                All <span className="tb-modal-tab-count">{deviceLogs.all.length}</span>
              </button>
            </div>

            {/* Filter input — shown for the All tab AND any logcat tab so you
                can narrow further (e.g. to a specific package or tag). */}
            {(tab === 'all' || tab === 'console' || tab === 'network') && (
              <div style={{ padding: '8px 18px', background: '#0d1117', borderBottom: '1px solid #30363d' }}>
                <input
                  type="text"
                  placeholder={tab === 'all'
                    ? 'Filter (substring or /regex/i) — try: OkHttp, ReactNativeJS, in.juspay, FATAL'
                    : 'Refine filter (substring or /regex/i)'}
                  value={logcatFilter}
                  onChange={e => setLogcatFilter(e.target.value)}
                  style={{
                    width: '100%',
                    background: '#161b22',
                    color: '#c9d1d9',
                    border: '1px solid #30363d',
                    borderRadius: 4,
                    padding: '6px 10px',
                    fontSize: 12,
                    fontFamily: 'ui-monospace, SFMono-Regular, monospace',
                  }}
                />
              </div>
            )}

            <pre ref={logBoxRef} className="tb-modal-log">
              {(() => {
                let lines: string[];
                let emptyHint: string;
                if (tab === 'build') {
                  lines = data?.log ?? [];
                  emptyHint = '(no build log yet — pick a config above and click Start)';
                } else if (tab === 'metro') {
                  lines = metroLog;
                  emptyHint = '(no Metro output yet — Metro starts after the build phase. Note: build log stops streaming after `ready`; Metro keeps going.)';
                } else if (tab === 'console') {
                  lines = deviceLogs.console;
                  emptyHint = '(no console output yet — JS console.log appears here once the app is running. If empty after a network call, switch to All and filter for ReactNativeJS / Hermes.)';
                } else if (tab === 'network') {
                  lines = deviceLogs.network;
                  emptyHint = '(no HTTP traffic captured yet — trigger a network call in the app. If empty, the app may use a network library not in our pattern list; switch to All and filter for the URL.)';
                } else {
                  lines = deviceLogs.all;
                  // When raw logcat is empty, server returns diagnostics:
                  // which adb binary it tried + what `adb devices` showed.
                  // Surface that in the empty-state so the user can fix
                  // the daemon mismatch without having to dig elsewhere.
                  if (logcatDiag) {
                    const devsLine = logcatDiag.devices?.split('\n').filter(l => l.trim()).slice(1).join('\n') || '(none)';
                    emptyHint =
                      `(no logcat output — adb returned 0 lines)\n\n` +
                      `adb binary:\n  ${logcatDiag.adb_bin || '(unknown)'}\n\n` +
                      `adb devices -l:\n  ${devsLine}\n\n` +
                      (logcatDiag.devices_err ? `devices stderr:\n  ${logcatDiag.devices_err}\n\n` : '') +
                      (logcatDiag.logcat_stderr ? `logcat stderr:\n  ${logcatDiag.logcat_stderr}\n\n` : '') +
                      `Likely causes:\n` +
                      `  • The above adb is talking to a daemon that has no devices.\n` +
                      `    Run \`adb kill-server\` from the terminal that launched the\n` +
                      `    emulator, then re-launch the app.\n` +
                      `  • test-context-api was started before ANDROID_HOME was set.\n` +
                      `    Restart it with: lsof -ti :7082 | xargs kill\n` +
                      `  • Multiple adb binaries on PATH — server resolved \`${logcatDiag.adb_bin}\`\n` +
                      `    but the emulator was launched with a different one.`;
                  } else {
                    emptyHint = '(no logcat output yet — adb logcat -d returned no lines. Make sure the emulator is connected.)';
                  }
                }

                // Apply client-side filter. Trailing /regex/flags syntax
                // means treat as a regex; otherwise plain substring (case-insensitive).
                const f = logcatFilter.trim();
                const filterApplies = f.length > 0 && (tab === 'all' || tab === 'console' || tab === 'network');
                if (filterApplies) {
                  let predicate: (s: string) => boolean;
                  const m = /^\/(.+)\/([a-z]*)$/.exec(f);
                  if (m) {
                    try {
                      const re = new RegExp(m[1], m[2] || 'i');
                      predicate = s => re.test(s);
                    } catch {
                      const lower = f.toLowerCase();
                      predicate = s => s.toLowerCase().includes(lower);
                    }
                  } else {
                    const lower = f.toLowerCase();
                    predicate = s => s.toLowerCase().includes(lower);
                  }
                  lines = lines.filter(predicate);
                }

                return lines.length ? lines.join('\n') : emptyHint;
              })()}
            </pre>

            <div className="tb-modal-footer">
              <span className="tb-modal-meta">
                {tab === 'build'   && <>{data?.log?.length ?? 0} lines · build log</>}
                {tab === 'metro'   && <>{metroLog.length} lines · /tmp/ny-rn-metro-{app === 'customer' ? 'consumer' : 'provider'}.log</>}
                {tab === 'console' && <>{deviceLogs.console.length} lines · filtered logcat (RN / Hermes)</>}
                {tab === 'network' && <>{deviceLogs.network.length} lines · filtered logcat (HTTP)</>}
                {tab === 'all'     && <>{deviceLogs.all.length} lines · full logcat · {logcatFilter ? `filter: ${logcatFilter}` : 'no filter'}</>}
                {data?.apps_built?.length ? ` · ${data.apps_built.join(', ')}` : ''}
              </span>
              <button
                className="tb-btn"
                onClick={() => {
                  const src = tab === 'build' ? (data?.log ?? [])
                    : tab === 'metro' ? metroLog
                      : tab === 'console' ? deviceLogs.console
                        : tab === 'network' ? deviceLogs.network
                          : deviceLogs.all;
                  if (!src.length) return;
                  navigator.clipboard?.writeText(src.join('\n')).catch(() => { });
                }}
                title={`Copy ${tab} log to clipboard`}
              >
                📋 Copy
              </button>
            </div>
          </div>
        </div>
      )}
    </>
  );
};
