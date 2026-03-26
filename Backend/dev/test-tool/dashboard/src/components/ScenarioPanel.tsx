import React, { useState, useEffect, useRef } from 'react';
import { Scenario, StepResult } from '../types';
import { ApiCatalog } from '../api-catalog';
import './ScenarioPanel.css';

interface Props {
  scenario: Scenario;
  results: StepResult[];
  catalog: ApiCatalog;
  presetSelections: Record<string, string>;
  onPresetChange: (stepId: string, presetId: string) => void;
  onRun: () => void;
  onRunStep: (stepId: string) => void;
  isRunning: boolean;
}

export const ScenarioPanel: React.FC<Props> = ({
  scenario, results, catalog, presetSelections, onPresetChange, onRun, onRunStep, isRunning
}) => {
  const [expandedStep, setExpandedStep] = useState<string | null>(null);
  const prevResultsLen = useRef(results.length);

  // Auto-expand the latest completed step (pass or fail)
  useEffect(() => {
    if (results.length > prevResultsLen.current || results.length === prevResultsLen.current) {
      const lastCompleted = [...results].reverse().find(r => r.status === 'pass' || r.status === 'fail');
      if (lastCompleted) setExpandedStep(lastCompleted.stepId);
    }
    prevResultsLen.current = results.length;
  }, [results]);

  const getResult = (stepId: string): StepResult | undefined =>
    results.find(r => r.stepId === stepId);

  return (
    <div className="scenario-panel">
      <div className="scenario-header">
        <div>
          <h2>{scenario.name}</h2>
          <p className="scenario-desc">{scenario.description}</p>
        </div>
        <button className="btn-run-scenario" onClick={onRun} disabled={isRunning}>
          {isRunning ? '⏳ Running...' : '▶ Run All Steps'}
        </button>
      </div>

      <div className="steps-list">
        {scenario.steps.map((step, idx) => {
          const result = getResult(step.id);
          const status = result?.status || (step.skip ? 'skip' : 'pending');
          const isExpanded = expandedStep === step.id;
          const catalogApi = step.useCatalog ? catalog[step.useCatalog] : undefined;
          const presets = catalogApi?.mockDataPresets || [];
          const selectedPreset = presetSelections[step.id] || (presets.length > 0 ? presets[0].id : '');

          return (
            <div key={step.id} className={`step step-${status}`}>
              <div className="step-main" onClick={() => setExpandedStep(isExpanded ? null : step.id)}>
                <div className={`step-num num-${status}`}>{idx + 1}</div>
                <div className="step-info">
                  <div className="step-name">
                    {!step.skip && <span className={`method-badge method-${step.method.toLowerCase()}`}>{step.method}</span>}
                    {step.name}
                    {catalogApi && <span className="catalog-badge">📦</span>}
                  </div>
                  <div className="step-path">
                    {typeof catalogApi?.path === 'function' ? `/${step.useCatalog} (dynamic)` : (catalogApi?.path || (typeof step.path === 'function' ? '(dynamic)' : step.path))}
                    {step.note && <em className="step-note"> — {step.note}</em>}
                  </div>
                </div>
                <div className="step-actions">
                  {!step.skip && (
                    <button className="btn-run-step" onClick={(e) => { e.stopPropagation(); onRunStep(step.id); }}
                      disabled={isRunning} title="Run this step">▶</button>
                  )}
                  <span className="step-time">{result?.durationMs ? `${result.durationMs}ms` : ''}</span>
                </div>
              </div>

              {/* Mock data preset selector */}
              {presets.length > 0 && (
                <div className="step-preset-bar" onClick={e => e.stopPropagation()}>
                  <label>Mock Data:</label>
                  <select value={selectedPreset}
                    onChange={e => onPresetChange(step.id, e.target.value)}>
                    {presets.map(p => (
                      <option key={p.id} value={p.id} title={p.description}>{p.name}</option>
                    ))}
                  </select>
                  <span className="preset-desc">
                    {presets.find(p => p.id === selectedPreset)?.description || ''}
                  </span>
                </div>
              )}

              {/* Response viewer */}
              {isExpanded && result?.response && (
                <div className="step-response">
                  <div className="response-status">HTTP {result.statusCode} • {result.durationMs}ms</div>
                  <pre>{JSON.stringify(result.response, null, 2)}</pre>
                </div>
              )}
              {isExpanded && result?.error && (
                <div className="step-response step-error-detail">
                  <pre>{result.error}</pre>
                </div>
              )}
            </div>
          );
        })}
      </div>
    </div>
  );
};
