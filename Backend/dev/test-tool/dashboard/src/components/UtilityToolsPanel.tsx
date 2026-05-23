import React, { useState } from 'react';
import { FinanceViewer } from './FinanceViewer';
import { ToolsPanel } from './ToolsPanel';
import './UtilityToolsPanel.css';

type ToolKey = 'clients' | 'visualizations';

interface ToolDef {
  key: ToolKey;
  label: string;
  render: () => React.ReactNode;
}

const TOOLS: ToolDef[] = [
  { key: 'clients',        label: 'Client Applications', render: () => <ToolsPanel title="Client Applications" /> },
  { key: 'visualizations', label: 'Visualizations',      render: () => <FinanceViewer /> },
];

export const UtilityToolsPanel: React.FC = () => {
  const [active, setActive] = useState<ToolKey>('visualizations');
  const tool = TOOLS.find(t => t.key === active) ?? TOOLS[0];
  return (
    <div className="utility-tools">
      <nav className="utility-tools-nav">
        {TOOLS.map(t => (
          <button
            key={t.key}
            className={`utility-tools-nav-item ${active === t.key ? 'active' : ''}`}
            onClick={() => setActive(t.key)}
          >
            {t.label}
          </button>
        ))}
      </nav>
      <div className="utility-tools-body">
        {tool.render()}
      </div>
    </div>
  );
};
