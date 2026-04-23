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

  // Re-run collection-level prerequest script (generates random phone numbers, reg nos, etc.)
  const reinitStores = useCallback(() => {
    if (!currentEnv || !selectedDir || !selectedSuite) return;
    // Reset stores to fresh env (discard prior run's random values)
    const freshStores: VariableStores = {
      environment: { ...currentEnv.variables },
      collection: {},
    };
    // Re-run collection prerequest script to regenerate random vars
    fetchCollection(selectedDir, selectedSuite).then(raw => {
      if (!raw) return;
      const parsed = parseCollection(raw as PostmanCollection, currentEnv.variables);
      freshStores.collection = { ...parsed.collectionVars };
      if (raw.event) {
        for (const ev of raw.event) {
          if (ev.listen === 'prerequest' && ev.script?.exec) {
            executePrereqScript(ev.script.exec.join('\n'), freshStores);
          }
        }
      }
      storesRef.current = freshStores;
    });
  }, [currentEnv, selectedDir, selectedSuite]);

  // Run all steps sequentially
  const runAll = useCallback(async () => {
    if (isRunning || steps.length === 0) return;
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
            executePrereqScript(ev.script.exec.join('\n'), freshStores);
          }
        }
      }
    }
    storesRef.current = freshStores;

    onLog('info', `-- Running ${currentSuite?.name || selectedSuite} (${currentEnv?.city || selectedEnv}) --`);

    for (const step of steps) {
      if (abortRef.current) break;

      setRunningStepId(step.id);
      setStepStates(prev => ({ ...prev, [step.id]: { status: 'running' } }));
      onLog('req', `${step.method} ${step.name}`);

      const start = performance.now();
      const result = await callPostmanStep(step, storesRef.current);
      const durationMs = Math.round(performance.now() - start);

      const failed = result.assertions.some(a => !a.passed) || !!result.scriptError;
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
  }, [isRunning, steps, currentEnv, currentSuite, selectedDir, selectedSuite, selectedEnv, onLog]);

  const stop = useCallback(() => { abortRef.current = true; }, []);

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
    const result = await callPostmanStep(step, storesRef.current);
    const durationMs = Math.round(performance.now() - start);

    const failed = result.assertions.some(a => !a.passed) || !!result.scriptError;
    const status = failed ? 'fail' : 'pass';

    setStepStates(prev => ({ ...prev, [stepId]: { status, result, durationMs } }));

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
                    {state?.status === 'fail' && (
                      <button
                        className="cr-rerun-btn"
                        onClick={e => { e.stopPropagation(); rerunStep(stepId); }}
                        disabled={isRunning}
                        title="Re-run this step"
                      >
                        ↻ Re-run
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

      {/* JSON Viewer Modal */}
      {modalData !== null && <JsonViewerModal data={modalData} onClose={() => setModalData(null)} />}
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
