/**
 * CollectionRunner — Runs Postman collection steps from integration-tests
 *
 * Replaces hardcoded flows with collections sourced from integration-tests/collections/
 * Each step executes with variable substitution, service log capture, and pm.* script runtime.
 */

import React, { useState, useEffect, useRef, useCallback } from 'react';
import './CollectionRunner.css';
import {
  CollectionGroup, CollectionEnvironment, CollectionSuite,
  fetchCollections, fetchCollection,
  fetchConfigSyncStatus, triggerConfigSync,
  isTollCollectionDir, seedTollDashboardAccess,
  mockServerBase, MockHit,
  startLogCapture, stopLogCapture
} from '../services/context';
import { parseCollection, ParsedStep, ParsedTreeNode, PostmanCollection } from '../services/postman-parser';
import { VariableStores, executePrereqScript } from '../services/postman-runtime';
import { PROXY_BASE } from '../config';
import { callPostmanStep, PostmanStepResult, startNewCoverageRun } from '../services/api';
import { RunAllDashboard, RunAllSuiteResult } from './RunAllDashboard';
import { StepResult, LogEntry } from '../types';

// Cities whose backing data lives in the EU prod cluster. Any environment
// matching one of these (by city or envName, case-insensitive) defaults its
// Sync-From options to ['prod_international', 'master'] instead of the
// India-default ['prod', 'master']. Append new entries here as more
// international cities come online.
const INTERNATIONAL_CITIES = ['Helsinki', 'Amsterdam'];

// Unified post-step grace. Logs and mock-hits captures start together before
// the step runs and close together after `done()` + this many milliseconds.
// Long enough to catch BPP-side async fan-out (confirm → juspay session,
// allocator scheduling, rider-app forks); short enough not to dominate step
// latency.
const STEP_CAPTURE_GRACE_MS = 1000;

interface StepCaptureResult {
  mockHits: MockHit[];
  serviceLogs: Record<string, string>;
}

// Safety timeout for waiting on EventSource.open — if mock-server doesn't
// respond in this long we proceed anyway with `mockEs` already nulled.
const MOCK_HITS_HANDSHAKE_TIMEOUT_MS = 500;

// Default inter-step wait. Even with per-step capture grace, fast steps can
// finish before the backend's async fan-out (beckn callbacks, allocator,
// juspay session creation triggered by confirm) settles. This wait gives the
// previous step's tail / SSE one extra beat to drain before the next step
// tears the tail down. Visible in collection logs as `[wait] 2s …`.
const INTER_STEP_WAIT_MS = 2000;

async function interStepWait(abortRef: React.MutableRefObject<boolean>): Promise<void> {
  const deadline = Date.now() + INTER_STEP_WAIT_MS;
  while (Date.now() < deadline) {
    if (abortRef.current) return;
    await new Promise(r => setTimeout(r, Math.min(100, deadline - Date.now())));
  }
}

// Open both the service-log tail (HTTP /api/logs/start) and the mock-hits
// SSE stream BEFORE the step's HTTP call, AND await each subscription's
// confirmation so the step doesn't fire before the tail subprocess is
// running / the SSE subscriber queue is registered. Without awaiting, fast
// Local steps complete before either capture is active and the per-step
// panels come back empty. Caller invokes `done()` once the postman step has
// fully returned (HTTP + test script). After STEP_CAPTURE_GRACE_MS, both are
// closed in parallel; `result` resolves with whatever each captured during
// that window. UI keeps logs / mock-hits in separate panels — they just
// share a lifecycle.
async function startStepCapture(envType: string): Promise<{
  done: () => void;
  result: Promise<StepCaptureResult>;
}> {
  // ── Logs side ── Await the HTTP round-trip so the `tail -n 0 -f` is
  // actually spawned before the step's HTTP call fires. `-n 0` skips
  // backfill, so anything written before tail starts is lost.
  const logToken: string | null = await startLogCapture().catch(() => null);

  // ── Mock-hits side ── Open the SSE and wait for `open` (handshake done →
  // server-side subscriber queue is registered) or `error` (mock-server
  // unreachable → give up cleanly). Either way we're past the handshake race
  // before the step fires its first request.
  let mockHits: MockHit[] = [];
  let mockEs: EventSource | null = null;
  if (envType === 'Local') {
    const es = new EventSource(`${mockServerBase()}/mock/hits/stream`);
    es.addEventListener('message', e => {
      try { mockHits.push(JSON.parse((e as MessageEvent).data) as MockHit); }
      catch { /* ignore malformed frame */ }
    });
    await new Promise<void>(resolve => {
      const safety = setTimeout(resolve, MOCK_HITS_HANDSHAKE_TIMEOUT_MS);
      es.addEventListener('open', () => { clearTimeout(safety); resolve(); }, { once: true });
      es.addEventListener('error', () => {
        // Mock-server down / endpoint missing — null `mockEs` so close() is a
        // no-op. Don't resolveHits here; close() still resolves the outer
        // promise with whatever we have (empty array).
        clearTimeout(safety);
        try { es.close(); } catch { /* noop */ }
        mockEs = null;
        resolve();
      }, { once: true });
    });
    // If the error handler already nulled mockEs above, leave it null.
    if (mockEs !== null || es.readyState !== EventSource.CLOSED) {
      mockEs = es;
    }
  }

  let doneCalled = false;
  let resolveResult!: (r: StepCaptureResult) => void;
  const result = new Promise<StepCaptureResult>(res => { resolveResult = res; });

  const close = async () => {
    const [serviceLogs] = await Promise.all([
      logToken ? stopLogCapture(logToken).catch(() => ({})) : Promise.resolve({}),
      Promise.resolve().then(() => { try { mockEs?.close(); } catch { /* noop */ } }),
    ]);
    resolveResult({ mockHits, serviceLogs });
  };

  return {
    done: () => {
      if (doneCalled) return;
      doneCalled = true;
      setTimeout(close, STEP_CAPTURE_GRACE_MS);
    },
    result,
  };
}


interface ServicesReady {
  ready: boolean;
  services: { name: string; port: number; path: string; up: boolean; status: number | null; error?: string }[];
}

// Poll context-api's /api/services-ready until rider/driver/mock-registry health
// endpoints all return 2xx. Used after a config import — services restart and
// their DB pools spend a while draining locks, during which /v2 returns 408.
async function waitForServicesReady(
  onLog: (level: LogEntry['level'], message: string, extra?: Partial<LogEntry>) => void,
  timeoutMs = 10 * 60 * 1000,
): Promise<boolean> {
  const deadline = Date.now() + timeoutMs;
  onLog('info', '[services] waiting for rider-app, driver-app, mock-registry to be healthy…');
  let lastSummary = '';
  while (Date.now() < deadline) {
    try {
      const r = await fetch(`${PROXY_BASE}/api/services-ready`);
      if (r.ok) {
        const j: ServicesReady = await r.json();
        if (j.ready) {
          onLog('success', '[services] all healthy');
          return true;
        }
        const summary = j.services
          .filter(s => !s.up)
          .map(s => `${s.name} (${s.error || `HTTP ${s.status ?? '?'}`})`)
          .join(', ');
        if (summary && summary !== lastSummary) {
          onLog('info', `[services] still waiting: ${summary}`);
          lastSummary = summary;
        }
      }
    } catch {
      // context-api itself may be momentarily flapping during stack restart
    }
    await new Promise(res => setTimeout(res, 2000));
  }
  onLog('error', `[services] timed out after ${Math.round(timeoutMs / 1000)}s — services never became healthy`);
  return false;
}

interface Props {
  onLog: (level: LogEntry['level'], message: string, extra?: Partial<LogEntry>) => void;
}

interface StepState {
  status: 'pending' | 'running' | 'pass' | 'fail' | 'skip';
  result?: PostmanStepResult;
  durationMs?: number;
  mockHits?: MockHit[];
}

// ── JSON Viewer Modal ──────────────────────────────────────────────
function JsonViewerModal({ data, onClose }: { data: any; onClose: () => void }) {
  const [search, setSearch] = useState('');
  const [copied, setCopied] = useState(false);
  const raw = JSON.stringify(data, null, 2);

  const copyJson = () => {
    navigator.clipboard.writeText(raw).then(() => {
      setCopied(true);
      setTimeout(() => setCopied(false), 1500);
    });
  };

  return (
    <div className="jv-overlay" onClick={onClose}>
      <div className="jv-modal" onClick={e => e.stopPropagation()}>
        <div className="jv-toolbar">
          <input
            className="jv-search"
            type="text"
            placeholder="Search keys or values..."
            value={search}
            onChange={e => setSearch(e.target.value)}
            autoFocus
          />
          <button className="jv-copy-btn" onClick={copyJson}>
            {copied ? '\u2713 Copied' : '\u2398 Copy JSON'}
          </button>
          <button className="jv-close-btn" onClick={onClose}>\u2715</button>
        </div>
        <div className="jv-tree">
          <JsonNode data={data} path="" search={search.toLowerCase()} depth={0} />
        </div>
      </div>
    </div>
  );
}

function JsonNode({ data, path, search, depth }: { data: any; path: string; search: string; depth: number }) {
  const [collapsed, setCollapsed] = useState(depth > 2);

  if (data === null) return <span className="jv-null">null</span>;
  if (typeof data === 'boolean') return <span className="jv-bool">{String(data)}</span>;
  if (typeof data === 'number') return <span className="jv-num">{data}</span>;
  if (typeof data === 'string') {
    const highlighted = search && data.toLowerCase().includes(search);
    return <span className={`jv-str ${highlighted ? 'jv-highlight' : ''}`}>"{data}"</span>;
  }

  const isArray = Array.isArray(data);
  const entries: [string, any][] = isArray ? data.map((v: any, i: number) => [String(i), v] as [string, any]) : Object.entries(data);
  const bracketOpen = isArray ? '[' : '{';
  const bracketClose = isArray ? ']' : '}';

  // Auto-expand if search matches a child
  const matchesSearch = search && JSON.stringify(data).toLowerCase().includes(search);
  const isOpen = matchesSearch ? true : !collapsed;

  return (
    <span>
      <span className="jv-bracket" onClick={() => setCollapsed(!collapsed)} style={{ cursor: 'pointer' }}>
        {isOpen ? '\u25BE' : '\u25B8'} {bracketOpen}
      </span>
      {!isOpen && <span className="jv-collapsed" onClick={() => setCollapsed(false)}> ...{entries.length} items {bracketClose}</span>}
      {isOpen && (
        <>
          <div className="jv-indent">
            {entries.map((entry, i) => {
              const key = entry[0];
              const val = entry[1];
              const keyHighlighted = search && key.toLowerCase().includes(search);
              return (
                <div key={key + i} className="jv-entry">
                  {!isArray && <span className={`jv-key ${keyHighlighted ? 'jv-highlight' : ''}`}>"{key}"</span>}
                  {!isArray && <span className="jv-colon">: </span>}
                  <JsonNode data={val} path={`${path}.${key}`} search={search} depth={depth + 1} />
                  {i < entries.length - 1 && <span className="jv-comma">,</span>}
                </div>
              );
            })}
          </div>
          <span className="jv-bracket">{bracketClose}</span>
        </>
      )}
    </span>
  );
}

export const CollectionRunner: React.FC<Props> = ({ onLog }) => {
  // Selection state
  const [groups, setGroups] = useState<CollectionGroup[]>([]);
  const [selectedDir, setSelectedDir] = useState('');
  const [selectedEnvType, setSelectedEnvType] = useState('Local');
  const [selectedSyncEnv, setSelectedSyncEnv] = useState<string>('master');
  const [selectedEnv, setSelectedEnv] = useState('');
  const [selectedSuite, setSelectedSuite] = useState('');

  // Parsed collection state
  const [steps, setSteps] = useState<ParsedStep[]>([]);
  const [nodes, setNodes] = useState<ParsedTreeNode[]>([]);
  const [stepStates, setStepStates] = useState<Record<string, StepState>>({});

  // Execution state
  const [isRunning, setIsRunning] = useState(false);
  const [runningStepId, setRunningStepId] = useState<string | null>(null);
  const [runAllProgress, setRunAllProgress] = useState<{ current: number; total: number; currentSuite: string } | null>(null);
  const [runAllResults, setRunAllResults] = useState<RunAllSuiteResult[]>([]);
  const [runAllStartedAt, setRunAllStartedAt] = useState<number | null>(null);
  const [, setRunAllTick] = useState(0);
  const [manualMode, setManualMode] = useState(false);
  const [versionId, setVersionId] = useState<string>('');
  const abortRef = useRef(false);
  const storesRef = useRef<VariableStores>({ environment: {}, collection: {} });

  // Expanded steps for viewing details
  const [expandedSteps, setExpandedSteps] = useState<Set<string>>(new Set());
  const [expandedLogs, setExpandedLogs] = useState<Set<string>>(new Set());

  // JSON viewer modal state
  const [modalData, setModalData] = useState<any>(null);

  // Load collection groups on mount
  useEffect(() => {
    fetchCollections().then(g => {
      setGroups(g);
      if (g.length > 0) setSelectedDir(g[0].directory);
    });
  }, []);

  const currentGroup = groups.find(g => g.directory === selectedDir);
  const envTypes = currentGroup?.envTypes ?? ['Local', 'Master'];
  const filteredEnvs = (currentGroup?.environments ?? []).filter(e => e.envType === selectedEnvType);
  const currentEnv = filteredEnvs.find(e => e.envName === selectedEnv);
  const currentSuite = currentGroup?.suites.find(s => s.filename === selectedSuite);
  // Default Sync-From options: India envs run against `prod`+`master`; cities
  // in INTERNATIONAL_CITIES route to the EU cluster instead, so they get
  // `prod_international`+`master`. Extend the list as new international
  // deployments come online.
  // Server-side compatibleEnvs (if ever populated) still wins.
  const _isInternational = INTERNATIONAL_CITIES.some(c => {
    const re = new RegExp(c, 'i');
    return re.test(currentEnv?.city ?? '') || re.test(currentEnv?.envName ?? '');
  });
  const _defaultSyncEnvs = _isInternational
    ? ['prod_international', 'master']
    : ['prod', 'master'];
  const syncEnvOptions =
    (currentEnv?.compatibleEnvs && currentEnv.compatibleEnvs.length > 0
      ? currentEnv.compatibleEnvs
      : currentGroup?.compatibleEnvs && currentGroup.compatibleEnvs.length > 0
        ? currentGroup.compatibleEnvs
        : _defaultSyncEnvs);
  const syncEnvDisabled = selectedEnvType !== 'Local';

  // Mock-server steps are only meaningful on Local env. On any other env type
  // (e.g. Master) the local mock processes aren't running, so we hide those
  // steps from the runner — mirrors the collection-level prerequest that
  // calls pm.execution.skipRequest() for Newman.
  const isMockOnlyStep = useCallback((s: ParsedStep) => (
    s.service === 'mock-server' ||
    s.rawUrl.includes('{{mockServerUrl}}') ||
    s.rawUrl.includes('{{mock_fcm_url}}')
  ), []);
  const visibleSteps = selectedEnvType === 'Local' ? steps : steps.filter(s => !isMockOnlyStep(s));
  const visibleStepIds = new Set(visibleSteps.map(s => s.id));
  const visibleNodes = nodes
    .map(n => ({ ...n, stepIds: n.stepIds.filter(id => visibleStepIds.has(id)) }))
    .filter(n => n.stepIds.length > 0);

  // Auto-select first env and suite when group / env-type changes
  useEffect(() => {
    if (currentGroup) {
      if (filteredEnvs.length > 0 && !filteredEnvs.find(e => e.envName === selectedEnv)) {
        setSelectedEnv(filteredEnvs[0].envName);
      } else if (filteredEnvs.length === 0) {
        setSelectedEnv('');
      }
      if (currentGroup.suites.length > 0 && !currentGroup.suites.find(s => s.filename === selectedSuite)) {
        setSelectedSuite(currentGroup.suites[0].filename);
      }
    }
  }, [selectedDir, selectedEnvType, currentGroup]);

  // Keep selectedSyncEnv valid for the current env/group's compatibleEnvs.
  useEffect(() => {
    if (syncEnvOptions.length > 0 && !syncEnvOptions.includes(selectedSyncEnv)) {
      setSelectedSyncEnv(syncEnvOptions[0]);
    }
  }, [syncEnvOptions, selectedSyncEnv]);

  // Tick once per second while a Run-All is in flight, so the dashboard's
  // "Elapsed" stat updates without needing extra state writes.
  useEffect(() => {
    if (!runAllProgress) return;
    const id = setInterval(() => setRunAllTick(t => t + 1), 1000);
    return () => clearInterval(id);
  }, [runAllProgress]);

  // Load and parse collection when suite changes
  useEffect(() => {
    if (!selectedDir || !selectedSuite || !currentEnv) {
      setSteps([]);
      setNodes([]);
      return;
    }
    fetchCollection(selectedDir, selectedSuite).then(async raw => {
      if (!raw) return;
      const parsed = parseCollection(raw as PostmanCollection, currentEnv.variables);

      // Run collection-level prerequest script for variable init
      if (raw.event) {
        for (const ev of raw.event) {
          if (ev.listen === 'prerequest' && ev.script?.exec) {
            const stores: VariableStores = {
              environment: { ...currentEnv.variables },
              collection: { ...parsed.collectionVars },
            };
            await executePrereqScript(ev.script.exec.join('\n'), stores);
            storesRef.current = stores;
          }
        }
      } else {
        storesRef.current = {
          environment: { ...currentEnv.variables },
          collection: { ...parsed.collectionVars },
        };
      }

      setSteps(parsed.steps);
      setNodes(parsed.nodes);
      setStepStates({});
      setExpandedSteps(new Set());
    });
  }, [selectedDir, selectedSuite, selectedEnv, currentEnv]);

  // Re-run collection-level prerequest script (generates random phone numbers, reg nos, etc.)
  const reinitStores = useCallback(() => {
    if (!currentEnv || !selectedDir || !selectedSuite) return;
    // Reset stores to fresh env (discard prior run's random values)
    const freshStores: VariableStores = {
      environment: { ...currentEnv.variables },
      collection: {},
    };
    // Re-run collection prerequest script to regenerate random vars
    fetchCollection(selectedDir, selectedSuite).then(async raw => {
      if (!raw) return;
      const parsed = parseCollection(raw as PostmanCollection, currentEnv.variables);
      freshStores.collection = { ...parsed.collectionVars };
      if (raw.event) {
        for (const ev of raw.event) {
          if (ev.listen === 'prerequest' && ev.script?.exec) {
            await executePrereqScript(ev.script.exec.join('\n'), freshStores);
          }
        }
      }
      storesRef.current = freshStores;
    });
  }, [currentEnv, selectedDir, selectedSuite]);

  // Ensure local DB config is synced from the chosen upstream environment.
  // Returns true to proceed, false if the user should abort.
  const ensureSyncedFrom = useCallback(async (
    targetEnv: string,
    envType: string = selectedEnvType,
  ): Promise<{ ok: boolean; configSyncApplied: boolean }> => {
    if (envType !== 'Local') return { ok: true, configSyncApplied: false };
    const status = await fetchConfigSyncStatus();
    if (status?.last_synced?.from === targetEnv && !status?.running) {
      onLog('info', `[config-sync] local already synced from '${targetEnv}' — skipping sync`);
      return { ok: true, configSyncApplied: false };
    }
    if (status?.running) {
      onLog('info', `[config-sync] sync already running (from '${status.from}') — waiting for it to finish`);
    } else {
      onLog('info', `[config-sync] local is ${status?.last_synced?.from ? `synced from '${status.last_synced.from}'` : 'not yet synced'} — starting sync from '${targetEnv}'`);
      const trig = await triggerConfigSync(targetEnv, false);
      if (!trig.started) {
        onLog('error', `[config-sync] failed to start: ${trig.error ?? 'unknown error'}`);
        return { ok: false, configSyncApplied: false };
      }
    }
    for (;;) {
      await new Promise(r => setTimeout(r, 2000));
      const st = await fetchConfigSyncStatus();
      if (!st) continue;
      if (!st.running) {
        if (st.exit_code === 0 && !st.error) {
          onLog('success', `[config-sync] done — synced from '${targetEnv}'`);
          const healthy = await waitForServicesReady(onLog);
          return { ok: healthy, configSyncApplied: true };
        }
        onLog('error', `[config-sync] failed (exit=${st.exit_code}, error=${st.error ?? 'n/a'})`);
        return { ok: false, configSyncApplied: false };
      }
    }
  }, [selectedEnvType, onLog]);

  const ensureTollDashboardSeed = useCallback(async (configSyncApplied: boolean): Promise<boolean> => {
    if (!isTollCollectionDir(selectedDir)) return true;
    onLog('info', '[toll-setup] seeding dashboard access_matrix + provider-dashboard token');
    const seed = await seedTollDashboardAccess(selectedSyncEnv, { skipConfigSync: configSyncApplied });
    if (seed.ok) {
      onLog('success', '[toll-setup] done');
      return true;
    }
    if (configSyncApplied) {
      onLog('info', `[toll-setup] seed API unavailable (${seed.error ?? 'unknown'}) — continuing (config-sync applied local-testing-data)`);
      return true;
    }
    onLog('error', `[toll-setup] failed: ${seed.error ?? 'unknown'} — run: cd Backend/dev/integration-tests && ./run-tests.sh --setup`);
    return false;
  }, [selectedDir, selectedSyncEnv, onLog]);

  // Run all steps sequentially
  const runAll = useCallback(async () => {
    if (isRunning || visibleSteps.length === 0) return;
    const sync = await ensureSyncedFrom(selectedSyncEnv);
    if (!sync.ok) return;
    const tollReady = await ensureTollDashboardSeed(sync.configSyncApplied);
    if (!tollReady) return;
    abortRef.current = false;
    setIsRunning(true);
    setStepStates({});

    // Reset stores and re-run collection prerequest to regenerate random vars
    const freshStores: VariableStores = {
      environment: { ...currentEnv!.variables },
      collection: {},
    };
    // Fetch raw collection to re-run prerequest scripts
    const raw = await fetchCollection(selectedDir, selectedSuite);
    if (raw) {
      const parsed = parseCollection(raw as PostmanCollection, currentEnv!.variables);
      freshStores.collection = { ...parsed.collectionVars };
      if (raw.event) {
        for (const ev of raw.event) {
          if (ev.listen === 'prerequest' && ev.script?.exec) {
            await executePrereqScript(ev.script.exec.join('\n'), freshStores);
          }
        }
      }
    }
    storesRef.current = freshStores;

    onLog('info', `-- Running ${currentSuite?.name || selectedSuite} (${currentEnv?.city || selectedEnv}) [${selectedEnvType}] --`);
    startNewCoverageRun(`collection-${selectedDir}-${selectedSuite}-${Date.now()}`);

    for (let stepIdx = 0; stepIdx < visibleSteps.length; stepIdx++) {
      if (abortRef.current) break;
      const step = visibleSteps[stepIdx];

      setRunningStepId(step.id);
      setStepStates(prev => ({ ...prev, [step.id]: { status: 'running' } }));
      onLog('req', `${step.method} ${step.name}`);

      const start = performance.now();
      // Open service-log tail + mock-hits stream together BEFORE the step so
      // they capture async fan-out (beckn callbacks, BPP juspay session,
      // rider-app forks) that lands after the HTTP response.
      const capture = await startStepCapture(selectedEnvType);
      const result = await callPostmanStep(step, storesRef.current);
      capture.done();
      // Latency excludes pre/post-request script time — only the upstream HTTP call.
      const durationMs = result.upstreamMs;
      const { mockHits, serviceLogs } = await capture.result;
      result.serviceLogs = serviceLogs;

      if (result.skipped) {
        setStepStates(prev => ({ ...prev, [step.id]: { status: 'skip', result, durationMs: 0, mockHits } }));
        onLog('info', `SKIP ${step.name}`);
        for (const line of result.consoleLogs) onLog('info', `  [script] ${line}`);
        if (stepIdx < visibleSteps.length - 1) await interStepWait(abortRef);
        continue;
      }

      const failed = result.assertions.some(a => !a.passed) || !!result.scriptError;
      const status = failed ? 'fail' : 'pass';

      setStepStates(prev => ({ ...prev, [step.id]: { status, result, durationMs, mockHits } }));

      // Log result
      const logLevel = failed ? 'error' : 'success';
      const assertSummary = result.assertions.length > 0
        ? ` [${result.assertions.filter(a => a.passed).length}/${result.assertions.length} assertions]`
        : '';
      onLog(logLevel, `${status === 'pass' ? 'PASS' : 'FAIL'} ${step.name} (${durationMs}ms, ${result.status})${assertSummary}`, {
        request: { method: step.method, url: result.resolvedUrl, body: result.resolvedBody, headers: result.resolvedHeaders },
        response: { status: result.status, body: result.data, headers: result.responseHeaders },
        serviceLogs: result.serviceLogs,
      });

      // Log console output from test script
      for (const line of result.consoleLogs) {
        onLog('info', `  [script] ${line}`);
      }

      if (failed) {
        if (result.scriptError) onLog('error', `  Script error: ${result.scriptError}`);
        for (const a of result.assertions.filter(a => !a.passed)) {
          onLog('error', `  Assertion failed: ${a.name} — ${a.error}`);
        }
        // Auto-expand failed step
        setExpandedSteps(prev => new Set(prev).add(step.id));
        break; // bail on first failure
      }

      if (stepIdx < visibleSteps.length - 1) {
        await interStepWait(abortRef);
      }
    }

    setIsRunning(false);
    setRunningStepId(null);
    onLog('info', '-- Collection run complete --');
  }, [isRunning, visibleSteps, currentEnv, currentSuite, selectedDir, selectedSuite, selectedEnv, selectedEnvType, selectedSyncEnv, ensureSyncedFrom, ensureTollDashboardSeed, onLog]);

  const stop = useCallback(() => { abortRef.current = true; }, []);

  // Run ALL suites across ALL collections and environments
  const runAllCollections = useCallback(async () => {
    if (isRunning || groups.length === 0) return;
    abortRef.current = false;
    setIsRunning(true);
    setRunAllResults([]);

    // Build the full list of (group, env, suite) combos
    const jobs: Array<{ group: CollectionGroup; env: CollectionEnvironment; suite: CollectionSuite }> = [];
    for (const group of groups) {
      for (const env of group.environments) {
        for (const suite of group.suites) {
          jobs.push({ group, env, suite });
        }
      }
    }

    setRunAllProgress({ current: 0, total: jobs.length, currentSuite: '' });
    setRunAllStartedAt(Date.now());
    startNewCoverageRun(`run-all-${Date.now()}`);
    onLog('info', `== Running ALL ${jobs.length} collection suites in parallel ==`);

    // Config sync runs once up-front when any job targets the Local stack.
    // The local DB is single-state, so we can't sync per-job; sync from
    // selectedSyncEnv before launching jobs. Non-Local envs hit their own
    // upstream and don't need a local sync.
    const hasLocal = jobs.some(j => j.env.envType === 'Local');
    if (hasLocal) {
      onLog('info', `[config-sync] Run All has Local-stack jobs — syncing from '${selectedSyncEnv}'`);
      const sync = await ensureSyncedFrom(selectedSyncEnv, 'Local');
      if (!sync.ok) {
        onLog('error', '[config-sync] aborting Run All — config sync failed');
        setIsRunning(false);
        setRunAllProgress(null);
        return;
      }
    } else {
      onLog('info', '[config-sync] no Local-stack jobs — skipping sync');
    }

    const hasToll = jobs.some(j => isTollCollectionDir(j.group.directory));
    if (hasToll) {
      onLog('info', '[toll-setup] seeding dashboard access for toll collections');
      const configSyncApplied = hasLocal;
      const seed = await seedTollDashboardAccess(selectedSyncEnv, { skipConfigSync: configSyncApplied });
      if (!seed.ok && !configSyncApplied) {
        onLog('error', `[toll-setup] failed: ${seed.error ?? 'unknown'} — aborting Run All`);
        setIsRunning(false);
        setRunAllProgress(null);
        return;
      }
      if (!seed.ok) {
        onLog('info', `[toll-setup] seed API unavailable (${seed.error ?? 'unknown'}) — continuing`);
      } else {
        onLog('success', '[toll-setup] done');
      }
    }

    let completed = 0;

    const runOneJob = async (job: { group: CollectionGroup; env: CollectionEnvironment; suite: CollectionSuite }) => {
      const { group, env, suite } = job;
      const label = `${group.directory} / ${suite.name} (${env.city})`;
      const tag = `[${label}]`;
      const suiteStart = performance.now();
      onLog('info', `-- start ${label} --`);

      const raw = await fetchCollection(group.directory, suite.filename);
      if (!raw) {
        onLog('error', `${tag} Failed to fetch collection: ${suite.filename}`);
        setRunAllResults(prev => [...prev, { group: group.directory, env: env.envName, suite: suite.name, passed: 0, failed: 0, total: 0 }]);
        return;
      }

      const parsed = parseCollection(raw as PostmanCollection, env.variables);

      const stores: VariableStores = {
        environment: { ...env.variables },
        collection: { ...parsed.collectionVars },
      };
      if (raw.event) {
        for (const ev of raw.event) {
          if (ev.listen === 'prerequest' && ev.script?.exec) {
            await executePrereqScript(ev.script.exec.join('\n'), stores);
          }
        }
      }

      let passed = 0;
      let failed = 0;
      const suiteStepMetrics: Array<{ name: string; method: string; path: string; elapsed_ms: number }> = [];
      for (let stepIdx = 0; stepIdx < parsed.steps.length; stepIdx++) {
        if (abortRef.current) break;
        const step = parsed.steps[stepIdx];

        const capture = await startStepCapture(env.envType);
        const result = await callPostmanStep(step, stores);
        capture.done();
        const durationMs = result.upstreamMs;
        const { serviceLogs } = await capture.result;
        result.serviceLogs = serviceLogs;

        if (result.skipped) {
          onLog('info', `${tag} SKIP ${step.name}`);
          for (const line of result.consoleLogs) onLog('info', `${tag}   [script] ${line}`);
          if (stepIdx < parsed.steps.length - 1) await interStepWait(abortRef);
          continue;
        }

        const stepFailed = result.assertions.some(a => !a.passed) || !!result.scriptError;
        if (stepFailed) failed++; else passed++;

        suiteStepMetrics.push({
          name: step.name,
          method: step.method,
          path: result.resolvedUrl || step.pathTemplate,
          elapsed_ms: result.upstreamMs,
        });

        const logLevel = stepFailed ? 'error' : 'success';
        const assertSummary = result.assertions.length > 0
          ? ` [${result.assertions.filter(a => a.passed).length}/${result.assertions.length}]`
          : '';
        onLog(logLevel, `${tag} ${stepFailed ? 'FAIL' : 'PASS'} ${step.name} (${durationMs}ms)${assertSummary}`, {
          request: { method: step.method, url: result.resolvedUrl, body: result.resolvedBody, headers: result.resolvedHeaders },
          response: { status: result.status, body: result.data, headers: result.responseHeaders },
          serviceLogs: result.serviceLogs,
        });

        for (const line of result.consoleLogs) onLog('info', `${tag}   [script] ${line}`);

        if (stepFailed) {
          if (result.scriptError) onLog('error', `${tag}   Script error: ${result.scriptError}`);
          for (const a of result.assertions.filter(a => !a.passed)) {
            onLog('error', `${tag}   Assertion failed: ${a.name} — ${a.error}`);
          }
          break;
        }

        if (stepIdx < parsed.steps.length - 1) await interStepWait(abortRef);
      }

      const suiteResult: RunAllSuiteResult = {
        group: group.directory, env: env.envName, suite: suite.name,
        passed, failed, total: parsed.steps.length,
        durationMs: Math.round(performance.now() - suiteStart),
      };
      setRunAllResults(prev => [...prev, suiteResult]);
      const statusEmoji = failed > 0 ? 'FAIL' : 'PASS';
      onLog(failed > 0 ? 'error' : 'success', `${tag} [${statusEmoji}] ${passed}/${parsed.steps.length} passed`);

      completed++;
      setRunAllProgress({ current: completed, total: jobs.length, currentSuite: label });

      if (versionId) {
        try {
          const metricsResp = await fetch(`${PROXY_BASE}/api/metrics/push`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
              versionId,
              collection: group.directory,
              env: env.envName,
              suite: suite.name,
              steps: suiteStepMetrics,
              allPassed: failed === 0,
            }),
          });
          const metricsBody = await metricsResp.json().catch(() => ({}));
          if (metricsResp.ok && metricsBody.pushed) {
            onLog('info', `${tag} [metrics] ✓ pushed (status=${failed === 0 ? 'pass' : 'fail'}) version=${versionId}`);
          } else {
            const reason = metricsBody.msg || `HTTP ${metricsResp.status}`;
            onLog('info', `${tag} [metrics] ✗ push failed — ${reason}`);
          }
        } catch (e: any) {
          onLog('info', `${tag} [metrics] ✗ push error — ${e?.message || e}`);
        }
      }
    };

    const BATCH_SIZE = 10;
    for (let i = 0; i < jobs.length; i += BATCH_SIZE) {
      if (abortRef.current) break;
      const batch = jobs.slice(i, i + BATCH_SIZE);
      onLog('info', `-- batch ${Math.floor(i / BATCH_SIZE) + 1}/${Math.ceil(jobs.length / BATCH_SIZE)} (${batch.length} suites) --`);
      await Promise.all(batch.map(runOneJob));
    }

    setIsRunning(false);
    setRunningStepId(null);
    setRunAllProgress(null);
    onLog('info', '== All collections complete ==');
  }, [isRunning, groups, onLog, versionId, selectedSyncEnv, ensureSyncedFrom]);

  // Initialise variable stores for manual mode (re-runs collection prereq once).
  // Called the first time the user clicks a per-step Run in manual mode after
  // toggling the mode on (or after suite change), so random vars match what a
  // single Run-All click would have generated.
  const manualStoresInitedRef = useRef<string | null>(null);
  const ensureManualStoresInited = useCallback(async () => {
    const sig = `${selectedDir}::${selectedSuite}::${selectedEnv}`;
    if (manualStoresInitedRef.current === sig) return;
    if (!currentEnv) return;
    const freshStores: VariableStores = {
      environment: { ...currentEnv.variables },
      collection: {},
    };
    const raw = await fetchCollection(selectedDir, selectedSuite);
    if (raw) {
      const parsed = parseCollection(raw as PostmanCollection, currentEnv.variables);
      freshStores.collection = { ...parsed.collectionVars };
      if (raw.event) {
        for (const ev of raw.event) {
          if (ev.listen === 'prerequest' && ev.script?.exec) {
            await executePrereqScript(ev.script.exec.join('\n'), freshStores);
          }
        }
      }
    }
    storesRef.current = freshStores;
    manualStoresInitedRef.current = sig;
  }, [currentEnv, selectedDir, selectedSuite, selectedEnv]);

  // Reset the init marker whenever selection changes so the next manual click reinitialises.
  useEffect(() => { manualStoresInitedRef.current = null; }, [selectedDir, selectedSuite, selectedEnv]);

  // Run a single step in manual mode — shares execute/log path with rerunStep
  // but additionally lazy-inits the variable stores on the first click.
  const runStepManual = useCallback(async (stepId: string) => {
    if (isRunning) return;
    const step = steps.find(s => s.id === stepId);
    if (!step) return;
    await ensureManualStoresInited();

    setIsRunning(true);
    setRunningStepId(stepId);
    setStepStates(prev => ({ ...prev, [stepId]: { status: 'running' } }));
    onLog('req', `[Step ${visibleSteps.findIndex(s => s.id === stepId) + 1}] ${step.method} ${step.name}`);

    const start = performance.now();
    const result = await callPostmanStep(step, storesRef.current);
    const durationMs = Math.round(performance.now() - start);

    if (result.skipped) {
      setStepStates(prev => ({ ...prev, [stepId]: { status: 'skip', result, durationMs: 0 } }));
      onLog('info', `SKIP ${step.name}`);
      for (const line of result.consoleLogs) onLog('info', `  [script] ${line}`);
      setIsRunning(false);
      setRunningStepId(null);
      return;
    }

    const failed = result.assertions.some(a => !a.passed) || !!result.scriptError;
    const status = failed ? 'fail' : 'pass';

    setStepStates(prev => ({ ...prev, [stepId]: { status, result, durationMs } }));

    const logLevel = failed ? 'error' : 'success';
    const assertSummary = result.assertions.length > 0
      ? ` [${result.assertions.filter(a => a.passed).length}/${result.assertions.length} assertions]`
      : '';
    onLog(logLevel, `${status === 'pass' ? 'PASS' : 'FAIL'} ${step.name} (${durationMs}ms, ${result.status})${assertSummary}`, {
      request: { method: step.method, url: result.resolvedUrl, body: result.resolvedBody, headers: result.resolvedHeaders },
      response: { status: result.status, body: result.data, headers: result.responseHeaders },
      serviceLogs: result.serviceLogs,
    });

    for (const line of result.consoleLogs) {
      onLog('info', `  [script] ${line}`);
    }
    if (failed) {
      if (result.scriptError) onLog('error', `  Script error: ${result.scriptError}`);
      for (const a of result.assertions.filter(a => !a.passed)) {
        onLog('error', `  Assertion failed: ${a.name} — ${a.error}`);
      }
      setExpandedSteps(prev => new Set(prev).add(stepId));
    }

    setIsRunning(false);
    setRunningStepId(null);
  }, [isRunning, steps, visibleSteps, ensureManualStoresInited, onLog]);

  // Mark a step as skipped without executing — unlocks the next step's gate.
  const skipStepManual = useCallback((stepId: string) => {
    if (isRunning) return;
    setStepStates(prev => ({ ...prev, [stepId]: { status: 'skip' } }));
    const step = steps.find(s => s.id === stepId);
    if (step) onLog('info', `SKIP ${step.name}`);
  }, [isRunning, steps, onLog]);

  // Re-run a single step in isolation (uses current variable state)
  const rerunStep = useCallback(async (stepId: string) => {
    if (isRunning) return;
    const step = steps.find(s => s.id === stepId);
    if (!step) return;

    setIsRunning(true);
    setRunningStepId(stepId);
    setStepStates(prev => ({ ...prev, [stepId]: { status: 'running' } }));
    onLog('req', `[Re-run] ${step.method} ${step.name}`);

    const start = performance.now();
    const capture = await startStepCapture(selectedEnvType);
    const result = await callPostmanStep(step, storesRef.current);
    capture.done();
    // Latency excludes pre/post-request script time — only the upstream HTTP call.
    const durationMs = result.upstreamMs;
    const { mockHits, serviceLogs } = await capture.result;
    result.serviceLogs = serviceLogs;

    if (result.skipped) {
      setStepStates(prev => ({ ...prev, [stepId]: { status: 'skip', result, durationMs: 0, mockHits } }));
      onLog('info', `SKIP ${step.name}`);
      for (const line of result.consoleLogs) onLog('info', `  [script] ${line}`);
      setIsRunning(false);
      setRunningStepId(null);
      return;
    }

    const failed = result.assertions.some(a => !a.passed) || !!result.scriptError;
    const status = failed ? 'fail' : 'pass';

    setStepStates(prev => ({ ...prev, [stepId]: { status, result, durationMs, mockHits } }));

    const logLevel = failed ? 'error' : 'success';
    const assertSummary = result.assertions.length > 0
      ? ` [${result.assertions.filter(a => a.passed).length}/${result.assertions.length} assertions]`
      : '';
    onLog(logLevel, `${status === 'pass' ? 'PASS' : 'FAIL'} ${step.name} (${durationMs}ms, ${result.status})${assertSummary}`, {
      request: { method: step.method, url: result.resolvedUrl, body: result.resolvedBody },
      response: { status: result.status, body: result.data },
      serviceLogs: result.serviceLogs,
    });

    for (const line of result.consoleLogs) {
      onLog('info', `  [script] ${line}`);
    }
    if (failed) {
      if (result.scriptError) onLog('error', `  Script error: ${result.scriptError}`);
      for (const a of result.assertions.filter(a => !a.passed)) {
        onLog('error', `  Assertion failed: ${a.name} — ${a.error}`);
      }
      setExpandedSteps(prev => new Set(prev).add(stepId));
    }

    setIsRunning(false);
    setRunningStepId(null);
  }, [isRunning, steps, onLog]);

  // Re-run from a given step through the end of the collection, reusing the
  // current variable state (does NOT reset stores or re-run the collection
  // prerequest) so it continues from where the prior run left off.
  const rerunFromStep = useCallback(async (stepId: string) => {
    if (isRunning) return;
    const startIdx = visibleSteps.findIndex(s => s.id === stepId);
    if (startIdx < 0) return;

    abortRef.current = false;
    setIsRunning(true);

    // Clear states for this step and everything after it; keep earlier results.
    setStepStates(prev => {
      const next = { ...prev };
      for (let i = startIdx; i < visibleSteps.length; i++) delete next[visibleSteps[i].id];
      return next;
    });

    onLog('info', `-- Re-running from step ${startIdx + 1} (${visibleSteps[startIdx].name}) to end --`);

    for (let stepIdx = startIdx; stepIdx < visibleSteps.length; stepIdx++) {
      if (abortRef.current) break;
      const step = visibleSteps[stepIdx];

      setRunningStepId(step.id);
      setStepStates(prev => ({ ...prev, [step.id]: { status: 'running' } }));
      onLog('req', `${step.method} ${step.name}`);

      const capture = await startStepCapture(selectedEnvType);
      const result = await callPostmanStep(step, storesRef.current);
      capture.done();
      const durationMs = result.upstreamMs;
      const { mockHits, serviceLogs } = await capture.result;
      result.serviceLogs = serviceLogs;

      if (result.skipped) {
        setStepStates(prev => ({ ...prev, [step.id]: { status: 'skip', result, durationMs: 0, mockHits } }));
        onLog('info', `SKIP ${step.name}`);
        for (const line of result.consoleLogs) onLog('info', `  [script] ${line}`);
        if (stepIdx < visibleSteps.length - 1) await interStepWait(abortRef);
        continue;
      }

      const failed = result.assertions.some(a => !a.passed) || !!result.scriptError;
      const status = failed ? 'fail' : 'pass';

      setStepStates(prev => ({ ...prev, [step.id]: { status, result, durationMs, mockHits } }));

      const logLevel = failed ? 'error' : 'success';
      const assertSummary = result.assertions.length > 0
        ? ` [${result.assertions.filter(a => a.passed).length}/${result.assertions.length} assertions]`
        : '';
      onLog(logLevel, `${status === 'pass' ? 'PASS' : 'FAIL'} ${step.name} (${durationMs}ms, ${result.status})${assertSummary}`, {
        request: { method: step.method, url: result.resolvedUrl, body: result.resolvedBody, headers: result.resolvedHeaders },
        response: { status: result.status, body: result.data, headers: result.responseHeaders },
        serviceLogs: result.serviceLogs,
      });

      for (const line of result.consoleLogs) {
        onLog('info', `  [script] ${line}`);
      }

      if (failed) {
        if (result.scriptError) onLog('error', `  Script error: ${result.scriptError}`);
        for (const a of result.assertions.filter(a => !a.passed)) {
          onLog('error', `  Assertion failed: ${a.name} — ${a.error}`);
        }
        setExpandedSteps(prev => new Set(prev).add(step.id));
        break;
      }

      if (stepIdx < visibleSteps.length - 1) {
        await interStepWait(abortRef);
      }
    }

    setIsRunning(false);
    setRunningStepId(null);
    onLog('info', '-- Re-run from step complete --');
  }, [isRunning, visibleSteps, selectedEnvType, onLog]);

  const toggleStep = (id: string) => {
    setExpandedSteps(prev => {
      const next = new Set(prev);
      next.has(id) ? next.delete(id) : next.add(id);
      return next;
    });
  };

  const toggleLogs = (id: string) => {
    setExpandedLogs(prev => {
      const next = new Set(prev);
      next.has(id) ? next.delete(id) : next.add(id);
      return next;
    });
  };

  return (
    <div className="collection-runner">
      {/* Selector bar */}
      <div className="cr-selector">
        <label>
          <span>Collection</span>
          <select value={selectedDir} onChange={e => setSelectedDir(e.target.value)}>
            {groups.map(g => <option key={g.directory} value={g.directory}>{g.directory}</option>)}
          </select>
        </label>
        <label>
          <span>Env Type</span>
          <select value={selectedEnvType} onChange={e => setSelectedEnvType(e.target.value)}>
            {envTypes.map(t => <option key={t} value={t}>{t}</option>)}
          </select>
        </label>
        <label title={syncEnvDisabled ? 'Config-sync only applies to the Local stack' : 'Upstream config bundle to sync into the local DB before running'}>
          <span>Sync From</span>
          <select
            className="config-sync-env"
            value={selectedSyncEnv}
            onChange={e => setSelectedSyncEnv(e.target.value)}
            disabled={syncEnvDisabled}
          >
            {syncEnvOptions.map(t => <option key={t} value={t}>{t}</option>)}
          </select>
        </label>
        <label>
          <span>Environment</span>
          <select value={selectedEnv} onChange={e => setSelectedEnv(e.target.value)} disabled={filteredEnvs.length === 0}>
            {filteredEnvs.length === 0
              ? <option value="">No {selectedEnvType} environments</option>
              : filteredEnvs.map(e => (
                  <option key={e.envName} value={e.envName}>{e.city} ({e.envName})</option>
                ))}
          </select>
        </label>
        <label>
          <span>Suite</span>
          <select value={selectedSuite} onChange={e => setSelectedSuite(e.target.value)}>
            {currentGroup?.suites.map(s => (
              <option key={s.filename} value={s.filename}>{s.name}</option>
            ))}
          </select>
        </label>
        <div className="cr-actions">
          {!manualMode && (
            <button className="cr-run-btn" onClick={runAll} disabled={isRunning || visibleSteps.length === 0}>
              {isRunning && !runAllProgress ? 'Running...' : `Run (${visibleSteps.length} steps${visibleSteps.length !== steps.length ? `, ${steps.length - visibleSteps.length} hidden` : ''})`}
            </button>
          )}
          <button
            className={`cr-manual-toggle-btn ${manualMode ? 'cr-manual-toggle-active' : ''}`}
            onClick={() => { setManualMode(m => !m); setStepStates({}); manualStoresInitedRef.current = null; }}
            disabled={isRunning || visibleSteps.length === 0}
            title="Run each step individually with Run / Skip buttons"
          >
            {manualMode ? 'Exit Step Mode' : 'Step-by-Step'}
          </button>
          <input
            className="cr-version-input"
            type="text"
            placeholder="Version ID (e.g. v1.2.3)"
            value={versionId}
            onChange={e => setVersionId(e.target.value)}
            disabled={isRunning}
            title="Version ID tagged on metrics pushed to Grafana"
          />
          <button className="cr-run-all-btn" onClick={runAllCollections} disabled={isRunning || groups.length === 0 || manualMode}>
            {runAllProgress ? `Running ${runAllProgress.current}/${runAllProgress.total}...` : 'Run All Collections'}
          </button>
          {isRunning && <button className="cr-stop-btn" onClick={stop}>Stop</button>}
        </div>
      </div>

      {(runAllProgress || runAllResults.length > 0) ? (
        <RunAllDashboard
          progress={runAllProgress}
          results={runAllResults}
          startedAt={runAllStartedAt}
          isRunning={isRunning}
          onStop={stop}
          onClose={() => { setRunAllResults([]); setRunAllStartedAt(null); }}
        />
      ) : (
      <div className="cr-steps">
        {visibleNodes.map(node => (
          <div key={node.id} className="cr-node">
            <div className="cr-node-header">
              {!node.prefixGroup && <span className={`cr-tag cr-tag-${node.tag}`}>{node.tag}</span>}
              <span className="cr-node-title">{node.title}</span>
            </div>
            {node.stepIds.map(stepId => {
              const step = steps.find(s => s.id === stepId);
              if (!step) return null;
              const state = stepStates[stepId];
              const isExpanded = expandedSteps.has(stepId);
              const logsExpanded = expandedLogs.has(stepId);
              const logCount = state?.result?.serviceLogs ? Object.keys(state.result.serviceLogs).length : 0;
              const mockHitCount = state?.mockHits?.length ?? 0;

              // Manual-mode gating: first step always enabled; later steps require prev step pass or skip.
              const visibleIdx = visibleSteps.findIndex(s => s.id === stepId);
              const prevVisibleId = visibleIdx > 0 ? visibleSteps[visibleIdx - 1].id : null;
              const prevState = prevVisibleId ? stepStates[prevVisibleId] : null;
              const gateOpen = visibleIdx === 0 || (prevState?.status === 'pass' || prevState?.status === 'skip');
              const notYetRun = !state || state.status === 'pending';

              return (
                <div key={stepId} className={`cr-step cr-step-${state?.status || 'pending'}`}>
                  <div className="cr-step-header" onClick={() => state && toggleStep(stepId)}>
                    <span className={`cr-dot cr-dot-${state?.status || 'pending'}`} />
                    {node.prefixGroup && <span className={`cr-tag cr-tag-${step.tag}`}>{step.tag}</span>}
                    <span className="cr-step-method">{step.method}</span>
                    <span className="cr-step-name">{step.name}</span>
                    {runningStepId === stepId && <span className="cr-spinner" />}
                    {state?.durationMs != null && <span className="cr-duration">{state.durationMs}ms</span>}
                    {state?.result && <span className={`cr-status-code ${(state.result.status >= 400) ? 'cr-status-error' : ''}`}>{state.result.status}</span>}
                    {logCount > 0 && (
                      <button className="cr-logs-badge" onClick={e => { e.stopPropagation(); toggleLogs(stepId); }}>
                        logs({logCount})
                      </button>
                    )}
                    {manualMode && notYetRun && (
                      <>
                        <button
                          className="cr-step-run-btn"
                          onClick={e => { e.stopPropagation(); runStepManual(stepId); }}
                          disabled={isRunning || !gateOpen}
                          title={gateOpen ? 'Run this step' : 'Previous step must pass or be skipped first'}
                        >
                          ▶ Run
                        </button>
                        <button
                          className="cr-step-skip-btn"
                          onClick={e => { e.stopPropagation(); skipStepManual(stepId); }}
                          disabled={isRunning || !gateOpen}
                          title="Skip this step (unlocks next)"
                        >
                          Skip
                        </button>
                      </>
                    )}
                    {mockHitCount > 0 && (
                      <span className="cr-mock-badge" title={`${mockHitCount} outbound mock call${mockHitCount === 1 ? '' : 's'} during this step`}>
                        🟣 {mockHitCount} mock
                      </span>
                    )}
                    {state?.status === 'fail' && (
                      <>
                        <button
                          className="cr-rerun-btn"
                          onClick={e => { e.stopPropagation(); rerunStep(stepId); }}
                          disabled={isRunning}
                          title="Re-run this step"
                        >
                          ↻ Re-run
                        </button>
                        <button
                          className="cr-rerun-btn"
                          onClick={e => { e.stopPropagation(); rerunFromStep(stepId); }}
                          disabled={isRunning}
                          title="Re-run from this step through the end of the collection (keeps current variable state)"
                        >
                          ↻↓ Re-run to end
                        </button>
                      </>
                    )}
                  </div>
                  {isExpanded && state?.result && (
                    <div className="cr-step-detail">
                      {state.result.assertions.length > 0 && (
                        <div className="cr-assertions">
                          {state.result.assertions.map((a, i) => (
                            <div key={i} className={`cr-assertion ${a.passed ? 'cr-assert-pass' : 'cr-assert-fail'}`}>
                              {a.passed ? '\u2713' : '\u2717'} {a.name}
                              {a.error && <span className="cr-assert-error"> — {a.error}</span>}
                            </div>
                          ))}
                        </div>
                      )}
                      <div className="cr-response-actions">
                        <button className="cr-view-btn" onClick={() => setModalData(state.result!.data)} title="View JSON">
                          &#128065; View
                        </button>
                        <button className="cr-copy-btn" onClick={() => {
                          navigator.clipboard.writeText(JSON.stringify(state.result!.data, null, 2));
                        }} title="Copy JSON">
                          &#128203; Copy
                        </button>
                      </div>
                      <pre className="cr-response-body">{JSON.stringify(state.result.data, null, 2)}</pre>
                      {mockHitCount > 0 && <MockHitsPanel hits={state.mockHits!} />}
                    </div>
                  )}
                  {logsExpanded && state?.result?.serviceLogs && (
                    <ServiceLogsTabs logs={state.result.serviceLogs} />
                  )}
                </div>
              );
            })}
          </div>
        ))}
        {steps.length === 0 && <div className="cr-empty">Select a collection, environment, and suite to load steps.</div>}
        {steps.length > 0 && visibleSteps.length === 0 && (
          <div className="cr-empty">All steps in this suite are mock-only; nothing to run on envType={selectedEnvType}.</div>
        )}
      </div>
      )}

      {/* JSON Viewer Modal */}
      {modalData !== null && <JsonViewerModal data={modalData} onClose={() => setModalData(null)} />}
    </div>
  );
};

function MockHitsPanel({ hits }: { hits: MockHit[] }) {
  const [expanded, setExpanded] = useState<Set<number>>(new Set());
  const toggle = (id: number) => setExpanded(prev => {
    const next = new Set(prev);
    next.has(id) ? next.delete(id) : next.add(id);
    return next;
  });
  return (
    <div className="cr-mock-hits">
      <div className="cr-mock-hits-header">🟣 Mock calls ({hits.length})</div>
      {hits.map(h => {
        const open = expanded.has(h.id);
        const errored = h.status >= 400;
        return (
          <div key={h.id} className={`cr-mock-hit ${errored ? 'cr-mock-hit-err' : ''}`}>
            <div className="cr-mock-hit-header" onClick={() => toggle(h.id)}>
              <span className="cr-mock-hit-chev">{open ? '▼' : '▸'}</span>
              <span className="cr-mock-hit-method">{h.method}</span>
              <span className="cr-mock-hit-path" title={h.url}>{h.path}{h.query ? `?${h.query}` : ''}</span>
              {h.service && <span className="cr-mock-hit-svc">{h.service}</span>}
              <span className="cr-mock-hit-duration">{h.duration_ms}ms</span>
              <span className={`cr-mock-hit-status ${errored ? 'cr-status-error' : ''}`}>{h.status}</span>
            </div>
            {open && (
              <div className="cr-mock-hit-detail">
                <div className="cr-mock-hit-row"><b>Full URL</b><code>http://localhost:8080{h.url}</code></div>
                <div className="cr-mock-hit-row"><b>Request headers</b>
                  <pre>{JSON.stringify(h.request_headers, null, 2)}</pre>
                </div>
                {h.request_body !== null && h.request_body !== undefined && (
                  <div className="cr-mock-hit-row"><b>Request body</b>
                    <pre>{typeof h.request_body === 'string' ? h.request_body : JSON.stringify(h.request_body, null, 2)}</pre>
                  </div>
                )}
                <div className="cr-mock-hit-row"><b>Response headers</b>
                  <pre>{JSON.stringify(h.response_headers, null, 2)}</pre>
                </div>
                <div className="cr-mock-hit-row"><b>Response body</b>
                  <pre>{typeof h.response_body === 'string' ? h.response_body : JSON.stringify(h.response_body, null, 2)}</pre>
                </div>
              </div>
            )}
          </div>
        );
      })}
    </div>
  );
}

function ServiceLogsTabs({ logs }: { logs: Record<string, string> }) {
  const [active, setActive] = useState(Object.keys(logs)[0] || '');
  const services = Object.keys(logs);
  return (
    <div className="cr-service-logs">
      <div className="cr-svc-tabs">
        {services.map(s => (
          <button key={s} className={`cr-svc-tab ${s === active ? 'active' : ''}`} onClick={() => setActive(s)}>
            {s.replace('-exe', '')}
          </button>
        ))}
      </div>
      {active && logs[active] && <pre className="cr-svc-log-body">{logs[active]}</pre>}
    </div>
  );
}
