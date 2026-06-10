import React, { useState, useEffect, useRef, useCallback } from 'react';
import './LoadTestModal.css';
import { ParsedStep } from '../services/postman-parser';
import {
  WorkerState, WorkerStepResult, LoadTestTokenPool,
  fetchLoadTestTokens,
  startBackendLoadTest, stopBackendLoadTest,
} from '../services/loadTestRunner';
import { LOCAL_API_BASE } from '../config';

interface Props {
  collectionName: string;
  suiteName: string;
  collectionDir: string;
  collectionSuite: string;
  envFile?: string;
  steps: ParsedStep[];
  baseEnv: Record<string, string>;
  onClose: () => void;
}

type UIPhase = 'config' | 'running' | 'done';

function fmt(ms: number): string {
  if (ms < 1000) return `${ms}ms`;
  if (ms < 60000) return `${(ms / 1000).toFixed(1)}s`;
  return `${Math.floor(ms / 60000)}m ${Math.floor((ms % 60000) / 1000)}s`;
}

function WorkerBadge({ w, selected, onClick }: { w: WorkerState; selected: boolean; onClick: () => void }) {
  const cls = `lt-badge lt-badge-${w.status}${selected ? ' lt-badge-selected' : ''}`;
  const rideInfo = w.totalRides > 1
    ? `Ride  : ${Math.min(w.currentRide + 1, w.totalRides)}/${w.totalRides}`
    : '';
  const title = [
    `#${w.id + 1}  ${w.status.toUpperCase()}`,
    `Rider : ${w.riderPhone}`,
    `Driver: ${w.driverPhone}`,
    rideInfo,
    w.ridesPassed > 0 || w.ridesFailed > 0
      ? `Rides : ${w.ridesPassed}✓ ${w.ridesFailed}✗`
      : '',
    w.currentStep ? `On    : ${w.currentStep}` : '',
    w.failedStepName ? `Step  : ${w.failedStepName}` : '',
    w.failedStepStatus ? `HTTP  : ${w.failedStepStatus}` : '',
    w.failedStepReason ? `Error : ${w.failedStepReason}` : '',
    w.finishedAt ? `Time  : ${fmt(w.finishedAt - w.startedAt)}` : '',
  ].filter(Boolean).join('\n');
  return (
    <div className={cls} title={title} onClick={onClick} style={{ cursor: 'pointer' }}>
      {w.id + 1}
    </div>
  );
}

function tryPretty(s: string | null | undefined): string {
  if (!s) return '';
  try { return JSON.stringify(JSON.parse(s), null, 2); } catch { return s; }
}

function StepDetail({ step }: { step: WorkerStepResult }) {
  const hasReqBody  = !!step.requestBody;
  const hasRespBody = !!step.responseBody;
  const hasHeaders  = step.requestHeaders && Object.keys(step.requestHeaders).length > 0;
  const assertions  = step.assertions ?? [];

  return (
    <div className="lt-step-detail">

      <div className="lt-step-detail-section">
        <div className="lt-step-detail-label"><span>Request</span></div>
        <div className="lt-step-detail-url">
          <span className="lt-step-detail-method">{step.method}</span>
          {' '}{step.url}
        </div>
        {hasHeaders && (
          <pre className="lt-step-detail-pre lt-step-detail-headers">
            {Object.entries(step.requestHeaders!).map(([k, v]) =>
              `${k}: ${k.toLowerCase().includes('token') || k.toLowerCase() === 'authorization'
                ? v.slice(0, 8) + '…'
                : v}`
            ).join('\n')}
          </pre>
        )}
        {hasReqBody && (
          <pre className="lt-step-detail-pre">{tryPretty(step.requestBody)}</pre>
        )}
        {!hasReqBody && <span className="lt-step-detail-empty">(no body)</span>}
      </div>

      <div className="lt-step-detail-section">
        <div className="lt-step-detail-label">
          <span>Response</span>
          <span className={`lt-step-detail-status-badge ${step.status >= 200 && step.status < 300 ? 'lt-status-ok' : 'lt-status-err'}`}>
            {step.status > 0 ? step.status : '—'}
          </span>
          <span className="lt-step-detail-elapsed">{step.elapsed}ms</span>
        </div>
        {hasRespBody
          ? <pre className="lt-step-detail-pre">{tryPretty(step.responseBody)}</pre>
          : <span className="lt-step-detail-empty">
              {step.status === 408 ? '(request timed out — no body)' : '(no body)'}
            </span>
        }
      </div>

      {assertions.length > 0 && (
        <div className="lt-step-detail-section">
          <div className="lt-step-detail-label">Assertions</div>
          {assertions.map((a, i) => (
            <div key={i} className={`lt-step-assert ${a.passed ? 'lt-step-assert-pass' : 'lt-step-assert-fail'}`}>
              <span className="lt-step-assert-icon">{a.passed ? '✓' : '✗'}</span>
              <span className="lt-step-assert-name">{a.name}</span>
              {!a.passed && a.error && (
                <span className="lt-step-assert-error">— {a.error}</span>
              )}
            </div>
          ))}
        </div>
      )}
    </div>
  );
}

function StepDrawer({ worker, onClose }: { worker: WorkerState; onClose: () => void }) {
  const [expanded, setExpanded] = useState<Set<string>>(new Set());
  const log = worker.stepLog;
  const byRide: WorkerStepResult[][] = [];
  const warmupLog: WorkerStepResult[] = [];
  for (const s of log) {
    if (s.rideIndex < 0) { warmupLog.push(s); continue; }
    while (byRide.length <= s.rideIndex) byRide.push([]);
    byRide[s.rideIndex].push(s);
  }

  const toggle = (key: string) => setExpanded(prev => {
    const next = new Set(prev);
    if (next.has(key)) next.delete(key); else next.add(key);
    return next;
  });

  return (
    <div className="lt-drawer">
      <div className="lt-drawer-header">
        <div className="lt-drawer-title-row">
          <span className="lt-drawer-title">Worker {worker.id + 1}</span>
          <span className={`lt-drawer-badge lt-drawer-badge-${worker.status}`}>{worker.status}</span>
        </div>
        <div className="lt-drawer-meta">
          <span className="lt-drawer-phone-pair">
            <span className="lt-drawer-phone-label">driver</span> {worker.driverPhone}
          </span>
          <span className="lt-drawer-phone-sep">/</span>
          <span className="lt-drawer-phone-pair">
            <span className="lt-drawer-phone-label">rider</span> {worker.riderPhone}
          </span>
        </div>
        <button className="lt-close" onClick={onClose}>✕</button>
      </div>

      <div className="lt-drawer-body">
        {log.length === 0 && (
          <div className="lt-drawer-empty">No steps recorded yet — run will populate this</div>
        )}

        {warmupLog.length > 0 && (
          <div className="lt-drawer-ride">
            <div className="lt-drawer-ride-title lt-drawer-ride-title-warmup">
              <span>Warmup</span>
              <span className="lt-drawer-ride-stats">
                {warmupLog.filter(s => s.passed).length}/{warmupLog.length} passed
              </span>
            </div>
            {warmupLog.map((s, si) => {
              const key = `warmup-${si}`;
              const isOpen = expanded.has(key);
              const cls = s.skipped ? 'lt-step-skipped' : s.passed ? 'lt-step-pass' : 'lt-step-fail';
              return (
                <div key={si} className="lt-step-item">
                  <div
                    className={`lt-step-row ${cls}${isOpen ? ' lt-step-open' : ''}`}
                    onClick={() => toggle(key)}
                  >
                    <span className="lt-step-icon">{s.skipped ? '⊘' : s.passed ? '✓' : '✗'}</span>
                    <span className="lt-step-method">{s.method}</span>
                    <span className="lt-step-name">
                      {s.name}
                      {s.url && <span className="lt-step-url">{s.url.replace(/^https?:\/\/[^/]+/, '')}</span>}
                      {!s.passed && s.error && <span className="lt-step-error">{s.error}</span>}
                    </span>
                    <span className="lt-step-status">{s.status > 0 ? s.status : '—'}</span>
                    <span className="lt-step-elapsed">{s.elapsed}ms</span>
                    <span className="lt-step-toggle">{isOpen ? '▾' : '▸'}</span>
                  </div>
                  {isOpen && <StepDetail step={s} />}
                </div>
              );
            })}
          </div>
        )}

        {byRide.map((steps, rideIdx) => (
          <div key={rideIdx} className="lt-drawer-ride">
            <div className="lt-drawer-ride-title">
              <span>Ride {rideIdx + 1}</span>
              <span className="lt-drawer-ride-stats">
                {steps.filter(s => s.passed).length}/{steps.length} passed
              </span>
            </div>
            {steps.map((s, si) => {
              const key = `${rideIdx}-${si}`;
              const isOpen = expanded.has(key);
              const cls = s.skipped ? 'lt-step-skipped' : s.passed ? 'lt-step-pass' : 'lt-step-fail';
              return (
                <div key={si} className="lt-step-item">
                  <div
                    className={`lt-step-row ${cls}${isOpen ? ' lt-step-open' : ''}`}
                    onClick={() => toggle(key)}
                  >
                    <span className="lt-step-icon">
                      {s.skipped ? '⊘' : s.passed ? '✓' : '✗'}
                    </span>
                    <span className="lt-step-method">{s.method}</span>
                    <span className="lt-step-name">
                      {s.name}
                      {s.url && (
                        <span className="lt-step-url">
                          {s.url.replace(/^https?:\/\/[^/]+/, '')}
                        </span>
                      )}
                      {!s.passed && s.error && (
                        <span className="lt-step-error">{s.error}</span>
                      )}
                    </span>
                    <span className="lt-step-status">{s.status > 0 ? s.status : '—'}</span>
                    <span className="lt-step-elapsed">{s.elapsed}ms</span>
                    <span className="lt-step-toggle">{isOpen ? '▾' : '▸'}</span>
                  </div>
                  {isOpen && <StepDetail step={s} />}
                </div>
              );
            })}
          </div>
        ))}
      </div>
    </div>
  );
}

export const LoadTestModal: React.FC<Props> = ({
  collectionName, suiteName, collectionDir, collectionSuite, envFile,
  steps, baseEnv, onClose,
}) => {
  const [uiPhase, setUiPhase]         = useState<UIPhase>('config');
  const [runPhase, setRunPhase]        = useState<'warmup' | 'riding'>('warmup');
  const [workerCount, setWorkerCount]  = useState(10);
  const [ridesPerWorker, setRidesPW]   = useState(1);
  const [pool, setPool]                = useState<LoadTestTokenPool | null>(null);
  const [poolError, setPoolError]      = useState<string | null>(null);
  const [useLocust, setUseLocust]      = useState(true);
  const [workers, setWorkers]          = useState<WorkerState[]>([]);
  const [selectedWorker, setSelected]  = useState<number | null>(null);
  const [result, setResult]            = useState<{ passed: number; failed: number; durationMs: number } | null>(null);
  const [svcError, setSvcError]        = useState<string | null>(null);

  const runIdRef          = useRef<string | null>(null);
  const esRef             = useRef<EventSource | null>(null);
  const workersRef        = useRef<WorkerState[]>([]); // mutable ref for SSE handler
  const autoCompleteTimer = useRef<ReturnType<typeof setTimeout> | null>(null);

  const runSteps = steps.filter(s =>
    s.service !== 'provider-dashboard' && !s.pathTemplate.includes('/plan/')
  );

  useEffect(() => {
    fetchLoadTestTokens()
      .then(p => setPool(p))
      .catch(e => setPoolError(String(e)));
  }, []);

  useEffect(() => () => { esRef.current?.close(); }, []);

  const maxWorkers = pool ? Math.min(pool.riderCount, pool.driverCount) : 0;
  const totalRides = workerCount * ridesPerWorker;

  const handleStart = useCallback(async () => {
    if (!pool) return;
    setSvcError(null);
    setUiPhase('running');
    setRunPhase('warmup');
    setSelected(null);
    setResult(null);

    const initial: WorkerState[] = Array.from({ length: workerCount }, (_, i) => ({
      id: i,
      status: 'pending',
      phase: 'warmup',
      currentStep: '',
      currentRide: 0,
      totalRides: ridesPerWorker,
      stepsDone: 0,
      stepsTotal: runSteps.length,
      ridesPassed: 0,
      ridesFailed: 0,
      startedAt: Date.now(),
      finishedAt: null,
      riderPhone: pool.riders[i]?.phone ?? '',
      driverPhone: pool.drivers[i]?.phone ?? '',
      stepLog: [],
    }));
    workersRef.current = initial;
    setWorkers([...initial]);

    let runId: string;
    try {
      runId = await startBackendLoadTest({
        steps: runSteps,
        baseEnv,
        workerCount,
        ridesPerWorker,
        riderTokens: pool.riders.slice(0, workerCount),
        driverTokens: pool.drivers.slice(0, workerCount),
        engine: useLocust ? 'locust' : 'postman',
        collectionDir: collectionDir,
        collectionSuite: collectionSuite,
        envFile: envFile,
      });
    } catch (e: any) {
      setSvcError(`Could not start: ${e?.message ?? e}. Is load-test-service running on port 7083?`);
      setUiPhase('config');
      return;
    }

    runIdRef.current = runId;

    const es = new EventSource(`${LOCAL_API_BASE}/api/load-test/events/${runId}`);
    esRef.current = es;

    const _finishRun = (durationMs: number) => {
      if (autoCompleteTimer.current) { clearTimeout(autoCompleteTimer.current); autoCompleteTimer.current = null; }
      es.close();
      const ws = workersRef.current;
      const totalPassed = ws.reduce((s, w) => s + w.ridesPassed, 0);
      const totalFailed = ws.reduce((s, w) => s + w.ridesFailed, 0);
      setResult({ passed: totalPassed, failed: totalFailed, durationMs });
      setUiPhase('done');
    };

    es.onmessage = (ev) => {
      let msg: any;
      try { msg = JSON.parse(ev.data); } catch { return; }

      const ws = workersRef.current;

      if (msg.type === 'phase_change') {
        setRunPhase(msg.phase === 'warmup' ? 'warmup' : 'riding');
        return;
      }

      if (msg.type === 'worker_update') {
        const { workerId, ...patch } = msg;
        if (workerId >= 0 && workerId < ws.length) {
          Object.assign(ws[workerId], patch);
          setWorkers([...ws]);
          if (patch.phase === 'ride') setRunPhase('riding');
        }
        const allDone = ws.length > 0 && ws.every(
          w => ['passed', 'failed', 'stopped'].includes(w.status)
        );
        if (allDone && !autoCompleteTimer.current) {
          autoCompleteTimer.current = setTimeout(() => _finishRun(0), 5000);
        }
        return;
      }

      if (msg.type === 'step_result') {
        const { workerId, rideIndex, step } = msg;
        if (workerId >= 0 && workerId < ws.length) {
          ws[workerId].stepLog = [
            ...ws[workerId].stepLog,
            { ...step, rideIndex },
          ];
          setWorkers([...ws]);
        }
        return;
      }

      if (msg.type === 'run_complete') {
        _finishRun(msg.durationMs ?? 0);
        return;
      }

      if (msg.type === 'error') {
        if (autoCompleteTimer.current) { clearTimeout(autoCompleteTimer.current); autoCompleteTimer.current = null; }
        es.close();
        setSvcError(msg.error);
        setUiPhase('done');
      }
    };

    es.onerror = () => {
      setUiPhase(prev => {
        if (prev !== 'running') return prev;
        const ws = workersRef.current;
        const totalPassed = ws.reduce((s, w) => s + w.ridesPassed, 0);
        const totalFailed = ws.reduce((s, w) => s + w.ridesFailed, 0);
        setResult(r => r ?? { passed: totalPassed, failed: totalFailed, durationMs: 0 });
        return 'done';
      });
    };
  }, [pool, workerCount, ridesPerWorker, runSteps, baseEnv]);

  const handleStop = () => {
    if (autoCompleteTimer.current) { clearTimeout(autoCompleteTimer.current); autoCompleteTimer.current = null; }
    if (runIdRef.current) stopBackendLoadTest(runIdRef.current);
    esRef.current?.close();
    const ws = workersRef.current;
    ws.forEach(w => { if (w.status === 'running' || w.status === 'pending') w.status = 'stopped'; });
    const totalPassed = ws.reduce((s, w) => s + w.ridesPassed, 0);
    const totalFailed = ws.reduce((s, w) => s + w.ridesFailed, 0);
    setWorkers([...ws]);
    setResult({ passed: totalPassed, failed: totalFailed, durationMs: 0 });
    setUiPhase('done');
  };

  const totalRidesPassed = workers.reduce((s, w) => s + w.ridesPassed, 0);
  const totalRidesFailed = workers.reduce((s, w) => s + w.ridesFailed, 0);
  const workersRunning   = workers.filter(w => w.status === 'running').length;
  const workersDone      = workers.filter(w => ['passed', 'failed', 'stopped'].includes(w.status)).length;
  const totalWorkers     = workers.length;
  const totalRidesCount  = totalWorkers * ridesPerWorker;
  const ridesDone        = totalRidesPassed + totalRidesFailed;

  const selWorker = selectedWorker !== null ? workers[selectedWorker] : null;

  return (
    <div className="lt-overlay" onClick={e => {
      if (e.target === e.currentTarget && uiPhase !== 'running') onClose();
    }}>
      <div className={`lt-modal ${selWorker ? 'lt-modal-with-drawer' : ''}`}>
        <div className="lt-header">
          <div className="lt-title">
            <span className="lt-icon">⚡</span>
            <div>
              <div className="lt-title-main">Load Test</div>
              <div className="lt-title-sub">{collectionName} / {suiteName}</div>
            </div>
          </div>
          {uiPhase !== 'running' && (
            <button className="lt-close" onClick={onClose}>✕</button>
          )}
        </div>

        {uiPhase === 'config' && (
          <div className="lt-body">
            {poolError && (
              <div className="lt-error">
                Failed to load token pool: {poolError}<br />
                Run <code>seed_users.py</code> first.
              </div>
            )}
            {svcError && <div className="lt-error">{svcError}</div>}

            {pool && (
              <div className="lt-pool-info">
                <div className="lt-pool-stat">
                  <span className="lt-pool-num">{pool.riderCount}</span>
                  <span className="lt-pool-label">riders available</span>
                </div>
                <div className="lt-pool-div">×</div>
                <div className="lt-pool-stat">
                  <span className="lt-pool-num">{pool.driverCount}</span>
                  <span className="lt-pool-label">drivers available</span>
                </div>
              </div>
            )}

            <div className="lt-field">
              <label className="lt-label">Parallel workers</label>
              <div className="lt-worker-row">
                <input type="range" min={1} max={Math.min(maxWorkers, 500)}
                  value={workerCount} className="lt-slider" disabled={!pool}
                  onChange={e => setWorkerCount(Number(e.target.value))} />
                <input type="number" min={1} max={maxWorkers}
                  value={workerCount} className="lt-number" disabled={!pool}
                  onChange={e => setWorkerCount(Math.max(1, Math.min(maxWorkers, Number(e.target.value))))} />
              </div>
              {pool && (
                <div className="lt-hint">
                  Each worker uses a unique phone pair •&nbsp;
                  Riders {pool.riders[0]?.phone}–{pool.riders[workerCount - 1]?.phone},&nbsp;
                  Drivers {pool.drivers[0]?.phone}–{pool.drivers[workerCount - 1]?.phone}
                </div>
              )}
            </div>

            <div className="lt-field">
              <label className="lt-label">Rides per worker</label>
              <div className="lt-worker-row">
                <input type="range" min={1} max={500}
                  value={ridesPerWorker} className="lt-slider"
                  onChange={e => setRidesPW(Number(e.target.value))} />
                <input type="number" min={1} max={9999}
                  value={ridesPerWorker} className="lt-number"
                  onChange={e => setRidesPW(Math.max(1, Number(e.target.value)))} />
              </div>
              <div className="lt-hint">
                Each worker re-uses the same phone pair, cycling through {ridesPerWorker} complete ride{ridesPerWorker !== 1 ? 's' : ''} •&nbsp;
                <strong style={{ color: '#f59e0b' }}>Total rides: {totalRides.toLocaleString()}</strong>
              </div>
            </div>

            <div className="lt-steps-info">
              <div className="lt-steps-row">
                <span className="lt-steps-run">{runSteps.length} steps per ride</span>
                <span className="lt-steps-skip">{steps.length - runSteps.length} setup steps skipped</span>
              </div>
              <div className="lt-steps-row" style={{ marginTop: 6 }}>
                <span style={{ fontSize: 11, color: '#4b5563' }}>
                  Runs via <code style={{ color: '#60a5fa' }}>local-api</code> on port 7083 (auto-started by Overmind)
                </span>
              </div>
            </div>

            <div className="lt-field">
              <label className="lt-label">Execution engine</label>
              <div style={{ display: 'flex', gap: 8, marginTop: 4 }}>
                <button
                  className={`lt-engine-btn${!useLocust ? ' lt-engine-active' : ''}`}
                  onClick={() => setUseLocust(false)}
                >
                  Postman runner
                  <span className="lt-engine-desc">collection steps, per-step assertions</span>
                </button>
                <button
                  className={`lt-engine-btn${useLocust ? ' lt-engine-active' : ''}`}
                  onClick={() => setUseLocust(true)}
                >
                  Locust
                  <span className="lt-engine-desc">native HTTP users, high concurrency</span>
                </button>
              </div>
            </div>

            <div className="lt-actions">
              <button className="lt-btn-cancel" onClick={onClose}>Cancel</button>
              <button className="lt-btn-start" onClick={handleStart} disabled={!pool || workerCount < 1}>
                Start — {workerCount}w × {ridesPerWorker}r = {totalRides.toLocaleString()} rides
              </button>
            </div>
          </div>
        )}

        {(uiPhase === 'running' || uiPhase === 'done') && (
          <div className="lt-body">
            {uiPhase === 'running' && runPhase === 'warmup' && (
              <div className="lt-warmup-banner">
                <span className="lt-warmup-spinner">⟳</span>
                Activating {workerCount} drivers — going online before rider searches start
              </div>
            )}

            <div className="lt-summary">
              <div className="lt-sum-stat lt-sum-pass">
                <span className="lt-sum-num">{totalRidesPassed}</span>
                <span className="lt-sum-label">rides passed</span>
              </div>
              <div className="lt-sum-stat lt-sum-fail">
                <span className="lt-sum-num">{totalRidesFailed}</span>
                <span className="lt-sum-label">rides failed</span>
              </div>
              <div className="lt-sum-stat lt-sum-run">
                <span className="lt-sum-num">{workersRunning}</span>
                <span className="lt-sum-label">active</span>
              </div>
              <div className="lt-sum-stat lt-sum-pend">
                <span className="lt-sum-num">{totalWorkers - workersDone - workersRunning}</span>
                <span className="lt-sum-label">pending</span>
              </div>
            </div>

            {totalRidesCount > 0 && (
              <div className="lt-progress-wrap">
                <div className="lt-progress-bar"
                  style={{ width: `${Math.round(100 * ridesDone / totalRidesCount)}%` }} />
                <div className="lt-progress-label">{ridesDone}/{totalRidesCount} rides</div>
              </div>
            )}

            <div className="lt-grid-label">Workers <span className="lt-grid-hint">(click to view requests)</span></div>
            <div className="lt-grid">
              {workers.map(w => (
                <WorkerBadge key={w.id} w={w}
                  selected={selectedWorker === w.id}
                  onClick={() => setSelected(selectedWorker === w.id ? null : w.id)} />
              ))}
            </div>

            {uiPhase === 'done' && result && (
              <div className="lt-done-banner">
                {result.failed === 0
                  ? <span className="lt-done-pass">✓ All {result.passed} rides passed in {fmt(result.durationMs)}</span>
                  : <span className="lt-done-mixed">
                      ✓ {result.passed} passed &nbsp;✗ {result.failed} failed &nbsp;— {fmt(result.durationMs)}
                    </span>
                }
              </div>
            )}
            {svcError && <div className="lt-error">{svcError}</div>}

            {uiPhase === 'done' && workers.some(w => w.failedStepName) && (
              <div className="lt-failures">
                <div className="lt-failures-title">First failures — click a worker badge for full step log</div>
                {workers.filter(w => w.failedStepName).slice(0, 5).map(w => (
                  <div key={w.id} className="lt-failure-row">
                    <span className="lt-failure-worker">W{w.id + 1}</span>
                    <span className="lt-failure-step">{w.failedStepName}</span>
                    {w.failedStepStatus ? <span className="lt-failure-status">HTTP {w.failedStepStatus}</span> : null}
                    {w.failedStepReason ? <span className="lt-failure-reason">{w.failedStepReason}</span> : null}
                  </div>
                ))}
                {workers.filter(w => w.failedStepName).length > 5 && (
                  <div className="lt-failure-more">
                    …and {workers.filter(w => w.failedStepName).length - 5} more — click badge to inspect
                  </div>
                )}
              </div>
            )}

            <div className="lt-actions">
              {uiPhase === 'running' && (
                <button className="lt-btn-stop" onClick={handleStop}>Stop</button>
              )}
              {uiPhase === 'done' && (
                <>
                  <button className="lt-btn-cancel" onClick={onClose}>Close</button>
                  <button className="lt-btn-start" onClick={() => {
                    setUiPhase('config');
                    setRunPhase('warmup');
                    setWorkers([]);
                    setResult(null);
                    setSelected(null);
                    setSvcError(null);
                    workersRef.current = [];
                  }}>Run Again</button>
                </>
              )}
            </div>
          </div>
        )}
      </div>

      {selWorker && (
        <StepDrawer worker={selWorker} onClose={() => setSelected(null)} />
      )}
    </div>
  );
};
