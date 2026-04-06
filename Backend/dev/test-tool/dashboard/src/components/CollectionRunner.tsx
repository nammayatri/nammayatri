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
  fetchCollections, fetchCollection
} from '../services/context';
import { parseCollection, ParsedStep, ParsedTreeNode, PostmanCollection } from '../services/postman-parser';
import { VariableStores, executePrereqScript } from '../services/postman-runtime';
import { callPostmanStep, PostmanStepResult } from '../services/api';
import { StepResult, LogEntry } from '../types';

interface Props {
  onLog: (level: LogEntry['level'], message: string, extra?: Partial<LogEntry>) => void;
}

interface StepState {
  status: 'pending' | 'running' | 'pass' | 'fail' | 'skip';
  result?: PostmanStepResult;
  durationMs?: number;
}

export const CollectionRunner: React.FC<Props> = ({ onLog }) => {
  // Selection state
  const [groups, setGroups] = useState<CollectionGroup[]>([]);
  const [selectedDir, setSelectedDir] = useState('');
  const [selectedEnv, setSelectedEnv] = useState('');
  const [selectedSuite, setSelectedSuite] = useState('');

  // Parsed collection state
  const [steps, setSteps] = useState<ParsedStep[]>([]);
  const [nodes, setNodes] = useState<ParsedTreeNode[]>([]);
  const [stepStates, setStepStates] = useState<Record<string, StepState>>({});

  // Execution state
  const [isRunning, setIsRunning] = useState(false);
  const [runningStepId, setRunningStepId] = useState<string | null>(null);
  const abortRef = useRef(false);
  const storesRef = useRef<VariableStores>({ environment: {}, collection: {} });

  // Expanded steps for viewing details
  const [expandedSteps, setExpandedSteps] = useState<Set<string>>(new Set());
  const [expandedLogs, setExpandedLogs] = useState<Set<string>>(new Set());

  // Load collection groups on mount
  useEffect(() => {
    fetchCollections().then(g => {
      setGroups(g);
      if (g.length > 0) setSelectedDir(g[0].directory);
    });
  }, []);

  const currentGroup = groups.find(g => g.directory === selectedDir);
  const currentEnv = currentGroup?.environments.find(e => e.envName === selectedEnv);
  const currentSuite = currentGroup?.suites.find(s => s.filename === selectedSuite);

  // Auto-select first env and suite when group changes
  useEffect(() => {
    if (currentGroup) {
      if (currentGroup.environments.length > 0 && !currentGroup.environments.find(e => e.envName === selectedEnv)) {
        setSelectedEnv(currentGroup.environments[0].envName);
      }
      if (currentGroup.suites.length > 0 && !currentGroup.suites.find(s => s.filename === selectedSuite)) {
        setSelectedSuite(currentGroup.suites[0].filename);
      }
    }
  }, [selectedDir, currentGroup]);

  // Load and parse collection when suite changes
  useEffect(() => {
    if (!selectedDir || !selectedSuite || !currentEnv) {
      setSteps([]);
      setNodes([]);
      return;
    }
    fetchCollection(selectedDir, selectedSuite).then(raw => {
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
            executePrereqScript(ev.script.exec.join('\n'), stores);
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

  // Run all steps sequentially
  const runAll = useCallback(async () => {
    if (isRunning || steps.length === 0) return;
    abortRef.current = false;
    setIsRunning(true);
    setStepStates({});

    // Re-init stores from env
    if (currentEnv) {
      storesRef.current.environment = { ...currentEnv.variables, ...storesRef.current.environment };
    }

    onLog('info', `-- Running ${currentSuite?.name || selectedSuite} (${currentEnv?.city || selectedEnv}) --`);

    for (const step of steps) {
      if (abortRef.current) break;

      setRunningStepId(step.id);
      setStepStates(prev => ({ ...prev, [step.id]: { status: 'running' } }));
      onLog('req', `${step.method} ${step.name}`);

      const start = performance.now();
      const result = await callPostmanStep(step, storesRef.current);
      const durationMs = Math.round(performance.now() - start);

      const failed = !result.ok || result.assertions.some(a => !a.passed) || !!result.scriptError;
      const status = failed ? 'fail' : 'pass';

      setStepStates(prev => ({ ...prev, [step.id]: { status, result, durationMs } }));

      // Log result
      const logLevel = failed ? 'error' : 'success';
      const assertSummary = result.assertions.length > 0
        ? ` [${result.assertions.filter(a => a.passed).length}/${result.assertions.length} assertions]`
        : '';
      onLog(logLevel, `${status === 'pass' ? 'PASS' : 'FAIL'} ${step.name} (${durationMs}ms, ${result.status})${assertSummary}`, {
        request: { method: step.method, url: result.resolvedUrl, body: result.resolvedBody },
        response: { status: result.status, body: result.data },
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

    }

    setIsRunning(false);
    setRunningStepId(null);
    onLog('info', '-- Collection run complete --');
  }, [isRunning, steps, currentEnv, currentSuite, selectedSuite, selectedEnv, onLog]);

  const stop = useCallback(() => { abortRef.current = true; }, []);

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
          <span>Environment</span>
          <select value={selectedEnv} onChange={e => setSelectedEnv(e.target.value)}>
            {currentGroup?.environments.map(e => (
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
          <button className="cr-run-btn" onClick={runAll} disabled={isRunning || steps.length === 0}>
            {isRunning ? 'Running...' : `Run (${steps.length} steps)`}
          </button>
          {isRunning && <button className="cr-stop-btn" onClick={stop}>Stop</button>}
        </div>
      </div>

      {/* Step list */}
      <div className="cr-steps">
        {nodes.map(node => (
          <div key={node.id} className="cr-node">
            <div className="cr-node-header">
              <span className={`cr-tag cr-tag-${node.tag}`}>{node.tag}</span>
              <span className="cr-node-title">{node.title}</span>
            </div>
            {node.stepIds.map(stepId => {
              const step = steps.find(s => s.id === stepId);
              if (!step) return null;
              const state = stepStates[stepId];
              const isExpanded = expandedSteps.has(stepId);
              const logsExpanded = expandedLogs.has(stepId);
              const logCount = state?.result?.serviceLogs ? Object.keys(state.result.serviceLogs).length : 0;

              return (
                <div key={stepId} className={`cr-step cr-step-${state?.status || 'pending'}`}>
                  <div className="cr-step-header" onClick={() => state && toggleStep(stepId)}>
                    <span className={`cr-dot cr-dot-${state?.status || 'pending'}`} />
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
                      <pre className="cr-response-body">{JSON.stringify(state.result.data, null, 2)}</pre>
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
      </div>
    </div>
  );
};

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
