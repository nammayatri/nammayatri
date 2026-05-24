/**
 * CoverageReport — API Coverage Metrics Dashboard
 *
 * Shows:
 * - Layer 1: Endpoint hit/miss (binary coverage)
 * - Layer 2A: Scenario coverage (enum field combinations tested vs possible)
 * - Layer 2C: Runtime fingerprints per endpoint
 * - Layer 3: Internal chain detection from service logs
 */

import React, { useState, useEffect, useCallback } from 'react';
import axios from 'axios';
import './CoverageReport.css';
import { PROXY_BASE } from '../config';

// All coverage endpoints live on the same context-api as PROXY_BASE.
// Keeping a local alias preserves the existing call-site syntax.
const API_BASE = PROXY_BASE;

interface EndpointSpec {
  method: string;
  path: string;
  service: string;
  enumFields: Record<string, any[]>;
  scenarios: string[];
  operationId: string;
  tags: string[];
}

interface EndpointCoverage {
  covered: boolean;
  hitCount: number;
  scenarioCoverage: Record<string, {
    possible: any[];
    tested: string[];
    coverage: number;
  }>;
  operationId: string;
  tags: string[];
}

interface CoverageReport {
  totals: Record<string, {
    total: number;
    covered: number;
    endpointCoverage: number;
    scenarios_total: number;
    scenarios_covered: number;
    scenarioCoverage: number;
  }>;
  report: Record<string, Record<string, EndpointCoverage>>;
  runs: Array<{ id: string; startedAt: number; hitCount: number }>;
  totalHits: number;
}

interface ChainEntry {
  trigger: { service: string; method: string; path: string };
  internal: Array<{ service: string; method: string; path: string }>;
  timestamp: string;
}

export const CoverageReportPanel: React.FC = () => {
  const [report, setReport] = useState<CoverageReport | null>(null);
  const [spec, setSpec] = useState<any>(null);
  const [chains, setChains] = useState<ChainEntry[]>([]);
  const [loading, setLoading] = useState(false);
  const [activeTab, setActiveTab] = useState<'overview' | 'rider' | 'driver' | 'chains' | 'gaps'>('overview');
  const [expandedEndpoints, setExpandedEndpoints] = useState<Set<string>>(new Set());
  const [filterText, setFilterText] = useState('');

  const fetchReport = useCallback(async () => {
    setLoading(true);
    try {
      const [reportResp, specResp, chainsResp] = await Promise.all([
        axios.get(`${API_BASE}/api/coverage/report`, { timeout: 10000 }),
        axios.get(`${API_BASE}/api/coverage/spec`, { timeout: 10000 }),
        axios.get(`${API_BASE}/api/coverage/chains`, { timeout: 10000 }),
      ]);
      setReport(reportResp.data);
      setSpec(specResp.data);
      setChains(chainsResp.data?.chains || []);
    } catch (e) {
      console.error('Failed to fetch coverage:', e);
    }
    setLoading(false);
  }, []);

  const refreshSpec = useCallback(async () => {
    setLoading(true);
    try {
      await axios.get(`${API_BASE}/api/coverage/spec?refresh=1`, { timeout: 15000 });
      await fetchReport();
    } catch (e) {
      console.error('Failed to refresh spec:', e);
    }
    setLoading(false);
  }, [fetchReport]);

  const clearRuns = useCallback(async () => {
    if (!window.confirm('Clear all recorded coverage runs?')) return;
    await axios.delete(`${API_BASE}/api/coverage/runs`, { timeout: 5000 });
    await fetchReport();
  }, [fetchReport]);

  useEffect(() => { fetchReport(); }, [fetchReport]);

  const toggleEndpoint = (key: string) => {
    setExpandedEndpoints(prev => {
      const next = new Set(prev);
      next.has(key) ? next.delete(key) : next.add(key);
      return next;
    });
  };

  if (loading && !report) {
    return <div className="cov-loading">Loading coverage data...</div>;
  }

  return (
    <div className="coverage-report">
      <div className="cov-header">
        <div className="cov-tabs">
          {(['overview', 'rider', 'driver', 'chains', 'gaps'] as const).map(tab => (
            <button
              key={tab}
              className={`cov-tab ${activeTab === tab ? 'active' : ''}`}
              onClick={() => setActiveTab(tab)}
            >
              {tab.charAt(0).toUpperCase() + tab.slice(1)}
            </button>
          ))}
        </div>
        <div className="cov-actions">
          <button onClick={fetchReport} disabled={loading} className="cov-btn">
            {loading ? 'Loading...' : 'Refresh'}
          </button>
          <button onClick={refreshSpec} disabled={loading} className="cov-btn cov-btn-secondary">
            Re-fetch OpenAPI
          </button>
          <button onClick={clearRuns} className="cov-btn cov-btn-danger">
            Clear Runs
          </button>
        </div>
      </div>

      {activeTab === 'overview' && report && <OverviewTab report={report} spec={spec} />}
      {activeTab === 'rider' && report && (
        <ServiceTab
          service="rider"
          report={report.report.rider || {}}
          filter={filterText}
          onFilterChange={setFilterText}
          expanded={expandedEndpoints}
          onToggle={toggleEndpoint}
        />
      )}
      {activeTab === 'driver' && report && (
        <ServiceTab
          service="driver"
          report={report.report.driver || {}}
          filter={filterText}
          onFilterChange={setFilterText}
          expanded={expandedEndpoints}
          onToggle={toggleEndpoint}
        />
      )}
      {activeTab === 'chains' && <ChainsTab chains={chains} />}
      {activeTab === 'gaps' && report && <GapsTab report={report} />}
    </div>
  );
};

// ── Overview Tab ──

function OverviewTab({ report, spec }: { report: CoverageReport; spec: any }) {
  const riderTotals = report.totals.rider;
  const driverTotals = report.totals.driver;

  return (
    <div className="cov-overview">
      <div className="cov-stats-grid">
        <StatCard
          title="Rider Endpoints"
          value={`${riderTotals.covered}/${riderTotals.total}`}
          percentage={riderTotals.endpointCoverage}
          color="blue"
        />
        <StatCard
          title="Driver Endpoints"
          value={`${driverTotals.covered}/${driverTotals.total}`}
          percentage={driverTotals.endpointCoverage}
          color="green"
        />
        <StatCard
          title="Rider Scenarios"
          value={`${riderTotals.scenarios_covered}/${riderTotals.scenarios_total}`}
          percentage={riderTotals.scenarioCoverage}
          color="purple"
        />
        <StatCard
          title="Driver Scenarios"
          value={`${driverTotals.scenarios_covered}/${driverTotals.scenarios_total}`}
          percentage={driverTotals.scenarioCoverage}
          color="orange"
        />
      </div>

      <div className="cov-summary-bar">
        <div className="cov-summary-item">
          <span className="cov-summary-label">Total Test Runs:</span>
          <span className="cov-summary-value">{report.runs.length}</span>
        </div>
        <div className="cov-summary-item">
          <span className="cov-summary-label">Total API Hits:</span>
          <span className="cov-summary-value">{report.totalHits}</span>
        </div>
        <div className="cov-summary-item">
          <span className="cov-summary-label">Spec Endpoints:</span>
          <span className="cov-summary-value">{spec?.summary?.total_endpoints || '?'}</span>
        </div>
      </div>

      {/* Coverage bar visualization */}
      <div className="cov-bar-section">
        <h3>Endpoint Coverage</h3>
        <CoverageBar label="Rider" covered={riderTotals.covered} total={riderTotals.total} color="#4a9eff" />
        <CoverageBar label="Driver" covered={driverTotals.covered} total={driverTotals.total} color="#4acf8a" />
      </div>

      <div className="cov-bar-section">
        <h3>Scenario Coverage</h3>
        <CoverageBar label="Rider" covered={riderTotals.scenarios_covered} total={riderTotals.scenarios_total} color="#9b59b6" />
        <CoverageBar label="Driver" covered={driverTotals.scenarios_covered} total={driverTotals.scenarios_total} color="#e67e22" />
      </div>

      {/* Recent runs */}
      {report.runs.length > 0 && (
        <div className="cov-runs-section">
          <h3>Recent Runs</h3>
          <div className="cov-runs-list">
            {report.runs.slice(-10).reverse().map(run => (
              <div key={run.id} className="cov-run-item">
                <span className="cov-run-id">{run.id}</span>
                <span className="cov-run-hits">{run.hitCount} hits</span>
                <span className="cov-run-time">{new Date(run.startedAt * 1000).toLocaleString()}</span>
              </div>
            ))}
          </div>
        </div>
      )}
    </div>
  );
}

// ── Service Tab (Rider/Driver detailed endpoint list) ──

function ServiceTab({
  service, report, filter, onFilterChange, expanded, onToggle
}: {
  service: string;
  report: Record<string, EndpointCoverage>;
  filter: string;
  onFilterChange: (v: string) => void;
  expanded: Set<string>;
  onToggle: (k: string) => void;
}) {
  const entries = Object.entries(report);
  const filtered = filter
    ? entries.filter(([k, v]) =>
        k.toLowerCase().includes(filter.toLowerCase()) ||
        v.operationId.toLowerCase().includes(filter.toLowerCase()) ||
        v.tags.some(t => t.toLowerCase().includes(filter.toLowerCase()))
      )
    : entries;

  // Sort: covered first (highest hit count at top), then uncovered
  const sorted = [...filtered].sort((a, b) => {
    if (a[1].covered !== b[1].covered) return a[1].covered ? -1 : 1;
    if (a[1].covered && b[1].covered) return b[1].hitCount - a[1].hitCount;
    return a[0].localeCompare(b[0]);
  });

  const coveredCount = entries.filter(([, v]) => v.covered).length;

  return (
    <div className="cov-service-tab">
      <div className="cov-filter-bar">
        <input
          type="text"
          placeholder="Filter by path, operationId, or tag..."
          value={filter}
          onChange={e => onFilterChange(e.target.value)}
          className="cov-filter-input"
        />
        <span className="cov-filter-count">
          {coveredCount}/{entries.length} covered ({filtered.length} shown)
        </span>
      </div>

      <div className="cov-endpoint-list">
        {sorted.map(([key, cov]) => {
          const isExpanded = expanded.has(`${service}:${key}`);
          const hasScenarios = Object.keys(cov.scenarioCoverage).length > 0;
          const scenarioPercent = hasScenarios
            ? Math.round(
                Object.values(cov.scenarioCoverage).reduce((sum, s) => sum + s.coverage, 0) /
                Object.keys(cov.scenarioCoverage).length * 100
              )
            : null;

          return (
            <div key={key} className={`cov-endpoint ${cov.covered ? 'cov-covered' : 'cov-uncovered'}`}>
              <div className="cov-endpoint-header" onClick={() => hasScenarios && onToggle(`${service}:${key}`)}>
                <span className={`cov-dot ${cov.covered ? 'cov-dot-pass' : 'cov-dot-fail'}`} />
                <span className="cov-endpoint-key">{key}</span>
                {cov.hitCount > 0 && <span className="cov-hit-badge">{cov.hitCount}x</span>}
                {scenarioPercent !== null && (
                  <span className={`cov-scenario-badge ${scenarioPercent === 100 ? 'full' : scenarioPercent > 0 ? 'partial' : 'empty'}`}>
                    {scenarioPercent}% scenarios
                  </span>
                )}
                {cov.tags.length > 0 && (
                  <span className="cov-tags">{cov.tags.join(', ')}</span>
                )}
              </div>

              {isExpanded && hasScenarios && (
                <div className="cov-scenario-detail">
                  {Object.entries(cov.scenarioCoverage).map(([field, sc]) => (
                    <div key={field} className="cov-scenario-field">
                      <span className="cov-field-name">{field}</span>
                      <div className="cov-field-values">
                        {sc.possible.map((val: any) => {
                          const tested = sc.tested.includes(String(val));
                          return (
                            <span key={String(val)} className={`cov-value-chip ${tested ? 'tested' : 'untested'}`}>
                              {String(val)}
                            </span>
                          );
                        })}
                      </div>
                    </div>
                  ))}
                </div>
              )}
            </div>
          );
        })}
      </div>
    </div>
  );
}

// ── Chains Tab ──

function ChainsTab({ chains }: { chains: ChainEntry[] }) {
  if (chains.length === 0) {
    return <div className="cov-empty">No chain data yet. Run some tests and check again.</div>;
  }

  return (
    <div className="cov-chains-tab">
      <p className="cov-chains-desc">
        Internal API chains detected from service logs. Shows which internal endpoints were triggered after each user-facing call.
      </p>
      <div className="cov-chain-list">
        {chains.slice().reverse().slice(0, 30).map((chain, i) => (
          <div key={i} className="cov-chain-item">
            <div className="cov-chain-trigger">
              <span className="cov-chain-svc">{chain.trigger.service}</span>
              <span className="cov-chain-method">{chain.trigger.method}</span>
              <span className="cov-chain-path">{chain.trigger.path}</span>
            </div>
            {chain.internal.length > 0 && (
              <div className="cov-chain-internal">
                {chain.internal.map((e, j) => (
                  <div key={j} className="cov-chain-internal-item">
                    <span className="cov-chain-arrow">→</span>
                    <span className="cov-chain-svc">{e.service}</span>
                    <span className="cov-chain-method">{e.method}</span>
                    <span className="cov-chain-path">{e.path}</span>
                  </div>
                ))}
              </div>
            )}
          </div>
        ))}
      </div>
    </div>
  );
}

// ── Gaps Tab ──

function GapsTab({ report }: { report: CoverageReport }) {
  const gaps: Array<{ service: string; endpoint: string; reason: string }> = [];

  for (const service of ['rider', 'driver'] as const) {
    for (const [key, cov] of Object.entries(report.report[service] || {})) {
      if (!cov.covered) {
        gaps.push({ service, endpoint: key, reason: 'Never called' });
      } else {
        // Check for untested scenarios
        for (const [field, sc] of Object.entries(cov.scenarioCoverage)) {
          const untested = sc.possible.filter((v: any) => !sc.tested.includes(String(v)));
          if (untested.length > 0) {
            gaps.push({
              service,
              endpoint: key,
              reason: `${field}: untested values [${untested.join(', ')}]`,
            });
          }
        }
      }
    }
  }

  // Sort: uncovered endpoints first, then gaps by service
  gaps.sort((a, b) => {
    if (a.reason === 'Never called' && b.reason !== 'Never called') return -1;
    if (b.reason === 'Never called' && a.reason !== 'Never called') return 1;
    return a.service.localeCompare(b.service) || a.endpoint.localeCompare(b.endpoint);
  });

  return (
    <div className="cov-gaps-tab">
      <h3>Coverage Gaps ({gaps.length})</h3>
      <p className="cov-gaps-desc">
        Endpoints and scenarios that have never been tested. Add test cases for these to improve coverage.
      </p>
      <div className="cov-gap-list">
        {gaps.map((gap, i) => (
          <div key={i} className={`cov-gap-item ${gap.reason === 'Never called' ? 'cov-gap-critical' : 'cov-gap-partial'}`}>
            <span className={`cov-gap-svc cov-gap-svc-${gap.service}`}>{gap.service}</span>
            <span className="cov-gap-endpoint">{gap.endpoint}</span>
            <span className="cov-gap-reason">{gap.reason}</span>
          </div>
        ))}
        {gaps.length === 0 && <div className="cov-empty">No coverage gaps found! All endpoints and scenarios are tested.</div>}
      </div>
    </div>
  );
}

// ── Shared Components ──

function StatCard({ title, value, percentage, color }: { title: string; value: string; percentage: number; color: string }) {
  return (
    <div className={`cov-stat-card cov-stat-${color}`}>
      <div className="cov-stat-title">{title}</div>
      <div className="cov-stat-value">{value}</div>
      <div className="cov-stat-bar">
        <div className="cov-stat-fill" style={{ width: `${Math.min(100, percentage)}%` }} />
      </div>
      <div className="cov-stat-percent">{percentage.toFixed(1)}%</div>
    </div>
  );
}

function CoverageBar({ label, covered, total, color }: { label: string; covered: number; total: number; color: string }) {
  const percent = total > 0 ? (covered / total) * 100 : 0;
  return (
    <div className="cov-bar-row">
      <span className="cov-bar-label">{label}</span>
      <div className="cov-bar-track">
        <div className="cov-bar-fill" style={{ width: `${percent}%`, backgroundColor: color }} />
      </div>
      <span className="cov-bar-text">{covered}/{total} ({percent.toFixed(1)}%)</span>
    </div>
  );
}
