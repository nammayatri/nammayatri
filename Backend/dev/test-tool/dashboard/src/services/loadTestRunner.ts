import { ParsedStep, PostmanCollection } from './postman-parser';
import { VariableStores, executePrereqScript } from './postman-runtime';
import { callPostmanStep } from './api';
import { PROXY_BASE, LOCAL_API_BASE } from '../config';
import { fetchCollection } from './context';
import { parseCollection } from './postman-parser';

export async function startBackendLoadTest(payload: {
  steps: ParsedStep[];
  baseEnv: Record<string, string>;
  workerCount: number;
  ridesPerWorker: number;
  riderTokens: LoadTestToken[];
  driverTokens: LoadTestToken[];
  engine?: 'postman' | 'locust';
  collectionDir?: string;
  collectionSuite?: string;
  envFile?: string;
}): Promise<string> {
  const resp = await fetch(`${LOCAL_API_BASE}/api/load-test/start`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(payload),
  });
  if (!resp.ok) throw new Error(`Load test service error: ${resp.status}`);
  const { runId } = await resp.json();
  return runId;
}

export async function stopBackendLoadTest(runId: string): Promise<void> {
  await fetch(`${LOCAL_API_BASE}/api/load-test/stop/${runId}`, { method: 'POST' });
}

export interface LoadTestToken {
  token: string;
  personId: string;
  phone: string;
}

export interface WorkerStepResult {
  name: string;
  method: string;
  url: string;
  status: number;
  elapsed: number;
  passed: boolean;
  skipped: boolean;
  assertions: { name: string; passed: boolean; error?: string | null }[];
  error: string | null;
  rideIndex: number;
  requestBody?: string | null;
  requestHeaders?: Record<string, string>;
  responseBody?: string | null;
}

export interface WorkerState {
  id: number;
  status: 'pending' | 'running' | 'passed' | 'failed' | 'stopped';
  phase: 'warmup' | 'ride';
  currentStep: string;
  currentRide: number;
  totalRides: number;
  stepsDone: number;
  stepsTotal: number;
  ridesPassed: number;
  ridesFailed: number;
  startedAt: number;
  finishedAt: number | null;
  riderPhone: string;
  driverPhone: string;
  failedStepName?: string;
  failedStepStatus?: number;
  failedStepReason?: string;
  /** Per-step results for the step-list drawer (click worker badge to view) */
  stepLog: WorkerStepResult[];
}

export interface LoadTestResult {
  workers: WorkerState[];
  totalPassed: number;
  totalFailed: number;
  durationMs: number;
  startedAt: number;
}

export interface LoadTestTokenPool {
  riders: LoadTestToken[];
  drivers: LoadTestToken[];
  riderCount: number;
  driverCount: number;
}

const isSetupStep = (step: ParsedStep): boolean =>
  step.service === 'provider-dashboard' ||
  step.pathTemplate.includes('/plan/');

export async function fetchLoadTestTokens(): Promise<LoadTestTokenPool> {
  const resp = await fetch(`${PROXY_BASE}/api/load-test/tokens`);
  if (!resp.ok) throw new Error(`Failed to fetch tokens: ${resp.status}`);
  return resp.json();
}

const AUTH_RESET_KEYS = [
  'driver_token', 'driver_id', 'driver_authId',
  'driver2_token', 'driver2_id', 'driver2_authId',
  'rider_token', 'rider_authId',
  'searchId', 'estimateId', 'customer_bookingId',
  'driver_ride_id', 'ride_otp', 'searchTryId',
];

function workerLatLonOffset(workerId: number): { dLat: number; dLon: number } {
  const cols = 4;
  const step = 0.1;
  return {
    dLat: Math.floor(workerId / cols) * step,
    dLon: (workerId % cols) * step,
  };
}

async function buildStores(
  riderToken: LoadTestToken,
  driverToken: LoadTestToken,
  workerId: number,
  baseEnv: Record<string, string>,
  collectionDir: string,
  collectionSuite: string,
): Promise<VariableStores> {
  const { dLat, dLon } = workerLatLonOffset(workerId);

  const originLat = parseFloat(baseEnv['origin_lat'] ?? '12.9352') + dLat;
  const originLon = parseFloat(baseEnv['origin_lon'] ?? '77.6245') + dLon;
  const destLat   = parseFloat(baseEnv['dest_lat']   ?? '12.9716') + dLat;
  const destLon   = parseFloat(baseEnv['dest_lon']   ?? '77.6412') + dLon;

  const workerEnv: Record<string, string> = {
    ...baseEnv,
    _test_rider_number:  riderToken.phone,
    _test_driver_number: driverToken.phone,
    origin_lat: String(originLat),
    origin_lon: String(originLon),
    dest_lat:   String(destLat),
    dest_lon:   String(destLon),
  };

  const stores: VariableStores = { environment: workerEnv, collection: {} };

  try {
    const raw = await fetchCollection(collectionDir, collectionSuite);
    if (raw) {
      const parsed = parseCollection(raw as PostmanCollection, workerEnv);
      stores.collection = { ...parsed.collectionVars };
      if ((raw as any).event) {
        for (const ev of (raw as any).event) {
          if (ev.listen === 'prerequest' && ev.script?.exec) {
            await executePrereqScript(ev.script.exec.join('\n'), stores);
          }
        }
      }
    }
  } catch { /* */ }

  stores.collection['_test_driver_number'] = driverToken.phone;
  stores.collection['_test_rider_number']  = riderToken.phone;
  stores.collection['origin_lat'] = String(originLat);
  stores.collection['origin_lon'] = String(originLon);
  stores.collection['dest_lat']   = String(destLat);
  stores.collection['dest_lon']   = String(destLon);

  return stores;
}

async function runStepList(
  steps: ParsedStep[],
  stores: VariableStores,
  onStep: (name: string, idx: number) => void,
  abortRef: { current: boolean },
): Promise<{ passed: boolean; failedName?: string; failedStatus?: number; failedReason?: string }> {
  for (let i = 0; i < steps.length; i++) {
    if (abortRef.current) return { passed: false, failedReason: 'aborted' };

    const step = steps[i];
    onStep(step.name, i);

    try {
      const result = await callPostmanStep(step, stores);
      if (result.skipped) continue;
      const failedAssertion = result.assertions.find(a => !a.passed);
      if (failedAssertion || result.scriptError) {
        const reason = failedAssertion?.error ?? result.scriptError ?? `HTTP ${result.status}`;
        console.warn(`[LoadTest] FAILED @ "${step.name}"`, { status: result.status, reason, data: result.data });
        return { passed: false, failedName: step.name, failedStatus: result.status, failedReason: reason };
      }
    } catch (err: any) {
      const reason = err?.message ?? String(err);
      console.warn(`[LoadTest] EXCEPTION @ "${step.name}"`, { err });
      return { passed: false, failedName: step.name, failedReason: reason };
    }
  }
  return { passed: true };
}

async function prewarmDriver(
  workerId: number,
  riderToken: LoadTestToken,
  driverToken: LoadTestToken,
  warmupSteps: ParsedStep[],
  baseEnv: Record<string, string>,
  collectionDir: string,
  collectionSuite: string,
  onUpdate: (patch: Partial<WorkerState>) => void,
  abortRef: { current: boolean },
): Promise<void> {
  onUpdate({ status: 'running', phase: 'warmup', currentStep: 'Authenticating driver...' });
  try {
    const stores = await buildStores(riderToken, driverToken, workerId, baseEnv, collectionDir, collectionSuite);
    await runStepList(warmupSteps, stores, (name) => {
      onUpdate({ currentStep: name });
    }, abortRef);
  } catch (e) {
    console.warn(`[LoadTest] warm-up worker ${workerId} exception`, e);
  }
}

async function runWorker(
  workerId: number,
  riderToken: LoadTestToken,
  driverToken: LoadTestToken,
  ridesPerWorker: number,
  allRunSteps: ParsedStep[],
  baseEnv: Record<string, string>,
  collectionDir: string,
  collectionSuite: string,
  onUpdate: (state: Partial<WorkerState>) => void,
  abortRef: { current: boolean },
): Promise<WorkerState> {
  const startedAt = Date.now();

  let ridesPassed = 0;
  let ridesFailed = 0;
  let failedStepName: string | undefined;
  let failedStepStatus: number | undefined;
  let failedStepReason: string | undefined;

  onUpdate({
    status: 'running',
    phase: 'ride',
    currentRide: 0,
    totalRides: ridesPerWorker,
    stepsTotal: allRunSteps.length,
    stepsDone: 0,
    ridesPassed: 0,
    ridesFailed: 0,
    riderPhone: riderToken.phone,
    driverPhone: driverToken.phone,
    startedAt,
    finishedAt: null,
  });

  for (let ride = 0; ride < ridesPerWorker; ride++) {
    if (abortRef.current) break;

    const stores = await buildStores(riderToken, driverToken, workerId, baseEnv, collectionDir, collectionSuite);
    for (const k of AUTH_RESET_KEYS) {
      delete stores.collection[k];
      delete stores.environment[k];
    }
    stores.collection['_test_driver_number'] = driverToken.phone;
    stores.collection['_test_rider_number']  = riderToken.phone;

    onUpdate({ currentRide: ride, stepsDone: 0, currentStep: '' });

    const r = await runStepList(
      allRunSteps,
      stores,
      (name, idx) => onUpdate({ currentStep: name, stepsDone: idx }),
      abortRef,
    );

    if (r.passed) {
      ridesPassed++;
    } else {
      ridesFailed++;
      failedStepName   = r.failedName;
      failedStepStatus = r.failedStatus;
      failedStepReason = r.failedReason;
    }
    onUpdate({ ridesPassed, ridesFailed });
  }

  const status = abortRef.current ? 'stopped' : ridesFailed === 0 ? 'passed' : 'failed';
  const finishedAt = Date.now();
  onUpdate({ status, finishedAt, currentStep: '', failedStepName, failedStepStatus, failedStepReason });

  return {
    id: workerId, status, phase: 'ride' as const, currentStep: '',
    currentRide: ridesPerWorker, totalRides: ridesPerWorker,
    stepsDone: allRunSteps.length, stepsTotal: allRunSteps.length,
    ridesPassed, ridesFailed, startedAt, finishedAt,
    riderPhone: riderToken.phone, driverPhone: driverToken.phone,
    failedStepName, failedStepStatus, failedStepReason,
    stepLog: [],
  };
}

export async function runLoadTest(
  workerCount: number,
  ridesPerWorker: number,
  steps: ParsedStep[],
  baseEnv: Record<string, string>,
  collectionDir: string,
  collectionSuite: string,
  riderTokens: LoadTestToken[],
  driverTokens: LoadTestToken[],
  onUpdate: (workers: WorkerState[]) => void,
  onPhaseChange: (phase: 'warmup' | 'riding') => void,
  abortRef: { current: boolean },
): Promise<LoadTestResult> {
  const count = Math.min(workerCount, riderTokens.length, driverTokens.length);
  const startedAt = Date.now();

  const allRunSteps = steps.filter(s => !isSetupStep(s));

  // Driver warm-up steps = non-setup steps before the first rider-service step.
  const firstRiderIdx = allRunSteps.findIndex(s => s.service === 'rider');
  const warmupSteps   = firstRiderIdx > 0 ? allRunSteps.slice(0, firstRiderIdx) : [];

  const workerStates: WorkerState[] = Array.from({ length: count }, (_, i) => ({
    id: i,
    status: 'pending' as const,
    phase: 'warmup' as const,
    currentStep: '',
    currentRide: 0,
    totalRides: ridesPerWorker,
    stepsDone: 0,
    stepsTotal: 0,
    ridesPassed: 0,
    ridesFailed: 0,
    startedAt: 0,
    finishedAt: null,
    riderPhone: riderTokens[i]?.phone ?? '',
    driverPhone: driverTokens[i]?.phone ?? '',
    stepLog: [],
  }));
  onUpdate([...workerStates]);

  if (warmupSteps.length > 0 && !abortRef.current) {
    onPhaseChange('warmup');
    await Promise.all(Array.from({ length: count }, (_, i) =>
      prewarmDriver(
        i,
        riderTokens[i],
        driverTokens[i],
        warmupSteps,
        baseEnv,
        collectionDir,
        collectionSuite,
        (patch) => { Object.assign(workerStates[i], patch); onUpdate([...workerStates]); },
        abortRef,
      )
    ));
  }

  if (abortRef.current) {
    const finishedAt = Date.now();
    workerStates.forEach(w => { w.status = 'stopped'; w.finishedAt = finishedAt; });
    onUpdate([...workerStates]);
    return { workers: workerStates, totalPassed: 0, totalFailed: 0, durationMs: finishedAt - startedAt, startedAt };
  }

  onPhaseChange('riding');

  const results = await Promise.all(Array.from({ length: count }, (_, i) =>
    runWorker(
      i,
      riderTokens[i],
      driverTokens[i],
      ridesPerWorker,
      allRunSteps,
      baseEnv,
      collectionDir,
      collectionSuite,
      (patch) => { Object.assign(workerStates[i], patch); onUpdate([...workerStates]); },
      abortRef,
    )
  ));

  const totalPassed = results.reduce((s, r) => s + r.ridesPassed, 0);
  const totalFailed = results.reduce((s, r) => s + r.ridesFailed, 0);

  return { workers: results, totalPassed, totalFailed, durationMs: Date.now() - startedAt, startedAt };
}
