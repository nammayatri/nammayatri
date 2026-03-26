import React from 'react';
import { FlowGroup, StepResult } from '../types';
import './Sidebar.css';

interface Props {
  flows: FlowGroup[];
  activeScenario: string | null;
  scenarioResults: Record<string, StepResult[]>;
  onSelect: (scenarioId: string) => void;
}

function getScenarioStatus(results: StepResult[] | undefined): 'pending' | 'pass' | 'fail' | 'running' {
  if (!results || results.length === 0) return 'pending';
  if (results.some(r => r.status === 'running')) return 'running';
  if (results.some(r => r.status === 'fail')) return 'fail';
  if (results.every(r => r.status === 'pass' || r.status === 'skip')) return 'pass';
  return 'pending';
}

export const Sidebar: React.FC<Props> = ({ flows, activeScenario, scenarioResults, onSelect }) => {
  return (
    <div className="sidebar">
      <div className="sidebar-logo">🧪 NY Test Dashboard</div>
      <div className="sidebar-subtitle">E2E Payment Test Suite</div>

      {flows.map(flow => (
        <div key={flow.id} className="flow-group">
          <div className="flow-group-title">{flow.name}</div>
          {flow.scenarios.map(sc => {
            const status = getScenarioStatus(scenarioResults[sc.id]);
            return (
              <div
                key={sc.id}
                className={`flow-item ${activeScenario === sc.id ? 'active' : ''}`}
                onClick={() => onSelect(sc.id)}
              >
                <span className="flow-item-name">{sc.name}</span>
                <span className={`flow-item-dot dot-${status}`}>●</span>
              </div>
            );
          })}
        </div>
      ))}
    </div>
  );
};
