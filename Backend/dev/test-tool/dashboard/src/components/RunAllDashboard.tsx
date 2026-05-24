import React, { useMemo } from 'react';
import {
  BarChart, Bar, XAxis, YAxis, Tooltip, Legend, ResponsiveContainer,
  CartesianGrid, PieChart, Pie, Cell,
} from 'recharts';

export interface RunAllSuiteResult {
  group: string;
  env: string;
  suite: string;
  passed: number;
  failed: number;
  total: number;
  durationMs?: number;
}

export interface RunAllProgress {
  current: number;
  total: number;
  currentSuite: string;
}

interface Props {
  progress: RunAllProgress | null;
  results: RunAllSuiteResult[];
  startedAt: number | null;
  onClose: () => void;
  onStop: () => void;
  isRunning: boolean;
}

const PASS_COLOR = '#2ea043';
const FAIL_COLOR = '#da3633';
const PENDING_COLOR = '#30363d';

function fmtDuration(ms: number): string {
  if (ms < 1000) return `${Math.round(ms)}ms`;
  if (ms < 60_000) return `${(ms / 1000).toFixed(1)}s`;
  const m = Math.floor(ms / 60_000);
  const s = Math.round((ms % 60_000) / 1000);
  return `${m}m ${s}s`;
}

function shortSuite(name: string): string {
  const parts = name.split(/[-_/]/);
  if (parts.length <= 2) return name;
  return `${parts[0]}…${parts[parts.length - 1]}`;
}

export const RunAllDashboard: React.FC<Props> = ({
  progress, results, startedAt, onClose, onStop, isRunning,
}) => {
  const totals = useMemo(() => {
    const completedSuites = results.length;
    const passedSuites = results.filter(r => r.failed === 0 && r.total > 0).length;
    const failedSuites = results.filter(r => r.failed > 0).length;
    const totalSteps = results.reduce((s, r) => s + r.total, 0);
    const passedSteps = results.reduce((s, r) => s + r.passed, 0);
    const failedSteps = results.reduce((s, r) => s + r.failed, 0);
    const passRate = totalSteps > 0 ? (passedSteps / totalSteps) * 100 : 0;
    return { completedSuites, passedSuites, failedSuites, totalSteps, passedSteps, failedSteps, passRate };
  }, [results]);

  const totalSuites = progress?.total ?? totals.completedSuites;
  const inProgress = progress ? Math.max(0, progress.total - totals.completedSuites) : 0;
  const remaining = progress ? progress.total - totals.completedSuites : 0;
  const pct = totalSuites > 0 ? Math.round((totals.completedSuites / totalSuites) * 100) : 0;
  const elapsedMs = startedAt ? Date.now() - startedAt : 0;
  const etaMs = (progress && totals.completedSuites > 0)
    ? Math.round((elapsedMs / totals.completedSuites) * remaining)
    : null;

  const perSuiteBars = useMemo(() => results.map(r => ({
    suite: shortSuite(r.suite),
    full: `${r.group} / ${r.suite} (${r.env})`,
    pass: r.passed,
    fail: r.failed,
  })), [results]);

  const perCollection = useMemo(() => {
    const byCol: Record<string, { pass: number; fail: number; suites: number }> = {};
    for (const r of results) {
      const k = r.group;
      byCol[k] = byCol[k] || { pass: 0, fail: 0, suites: 0 };
      byCol[k].pass += r.passed;
      byCol[k].fail += r.failed;
      byCol[k].suites += 1;
    }
    return Object.entries(byCol).map(([group, v]) => ({ group, ...v }));
  }, [results]);

  const durationData = useMemo(() => results
    .filter(r => r.durationMs != null)
    .map(r => ({ suite: shortSuite(r.suite), ms: r.durationMs!, full: r.suite }))
    .sort((a, b) => b.ms - a.ms)
    .slice(0, 12), [results]);

  const statusPie = [
    { name: 'Passed', value: totals.passedSuites, color: PASS_COLOR },
    { name: 'Failed', value: totals.failedSuites, color: FAIL_COLOR },
    { name: 'Pending', value: Math.max(0, totalSuites - totals.completedSuites), color: PENDING_COLOR },
  ].filter(s => s.value > 0);

  return (
    <div className="rad-root">
      <div className="rad-header">
        <div className="rad-header-left">
          <span className="rad-title">Run All Collections</span>
          {isRunning && <span className="rad-running-pill">● live</span>}
          {!isRunning && results.length > 0 && (
            <span className={`rad-running-pill ${totals.failedSuites > 0 ? 'rad-pill-fail' : 'rad-pill-pass'}`}>
              {totals.failedSuites > 0 ? '✗ done with failures' : '✓ all passed'}
            </span>
          )}
        </div>
        <div className="rad-header-right">
          {isRunning && <button className="rad-btn rad-btn-danger" onClick={onStop}>Stop</button>}
          {!isRunning && <button className="rad-btn" onClick={onClose}>Close</button>}
        </div>
      </div>

      <div className="rad-stats">
        <Stat label="Suites" value={`${totals.completedSuites}/${totalSuites}`} sub={`${pct}% complete`} />
        <Stat label="Passed" value={String(totals.passedSuites)} sub={`${totals.passedSteps} steps`} accent={PASS_COLOR} />
        <Stat label="Failed" value={String(totals.failedSuites)} sub={`${totals.failedSteps} steps`} accent={FAIL_COLOR} />
        <Stat label="In flight" value={String(inProgress)} sub={progress ? 'running…' : 'idle'} />
        <Stat label="Pass rate" value={`${Math.round(totals.passRate)}%`} sub={`${totals.passedSteps}/${totals.totalSteps} steps`}
              accent={totals.passRate >= 95 ? PASS_COLOR : totals.passRate >= 80 ? '#d29922' : FAIL_COLOR} />
        <Stat label="Elapsed" value={fmtDuration(elapsedMs)} sub={etaMs != null && remaining > 0 ? `ETA ${fmtDuration(etaMs)}` : ''} />
      </div>

      <div className="rad-progress-bar">
        <div className="rad-progress-fill" style={{ width: `${pct}%` }} />
      </div>
      {progress && (
        <div className="rad-progress-caption">
          Suite {progress.current}/{progress.total} — {progress.currentSuite || '…'}
        </div>
      )}

      <div className="rad-grid">
        <Card title="Suite status">
          {statusPie.length === 0 ? (
            <Empty msg="No suites completed yet" />
          ) : (
            <ResponsiveContainer width="100%" height={220}>
              <PieChart>
                <Pie data={statusPie} dataKey="value" nameKey="name" outerRadius={80} innerRadius={45} paddingAngle={2}>
                  {statusPie.map((e, i) => <Cell key={i} fill={e.color} />)}
                </Pie>
                <Tooltip contentStyle={tooltipStyle} />
                <Legend wrapperStyle={{ fontSize: 11 }} />
              </PieChart>
            </ResponsiveContainer>
          )}
        </Card>

        <Card title="Per-suite pass / fail (steps)">
          {perSuiteBars.length === 0 ? (
            <Empty msg="Waiting for first suite to finish" />
          ) : (
            <ResponsiveContainer width="100%" height={220}>
              <BarChart data={perSuiteBars} margin={{ top: 5, right: 10, left: -10, bottom: 50 }}>
                <CartesianGrid stroke="#21262d" strokeDasharray="3 3" />
                <XAxis dataKey="suite" tick={{ fontSize: 10, fill: '#8b949e' }} interval={0} angle={-30} textAnchor="end" />
                <YAxis tick={{ fontSize: 10, fill: '#8b949e' }} allowDecimals={false} />
                <Tooltip contentStyle={tooltipStyle} labelFormatter={(_, p) => (p?.[0]?.payload as any)?.full ?? ''} />
                <Legend wrapperStyle={{ fontSize: 11 }} />
                <Bar dataKey="pass" stackId="a" fill={PASS_COLOR} />
                <Bar dataKey="fail" stackId="a" fill={FAIL_COLOR} />
              </BarChart>
            </ResponsiveContainer>
          )}
        </Card>

        <Card title="By collection">
          {perCollection.length === 0 ? (
            <Empty msg="No data yet" />
          ) : (
            <ResponsiveContainer width="100%" height={220}>
              <BarChart data={perCollection} margin={{ top: 5, right: 10, left: -10, bottom: 30 }}>
                <CartesianGrid stroke="#21262d" strokeDasharray="3 3" />
                <XAxis dataKey="group" tick={{ fontSize: 10, fill: '#8b949e' }} interval={0} angle={-20} textAnchor="end" />
                <YAxis tick={{ fontSize: 10, fill: '#8b949e' }} allowDecimals={false} />
                <Tooltip contentStyle={tooltipStyle} />
                <Legend wrapperStyle={{ fontSize: 11 }} />
                <Bar dataKey="pass" stackId="a" fill={PASS_COLOR} />
                <Bar dataKey="fail" stackId="a" fill={FAIL_COLOR} />
              </BarChart>
            </ResponsiveContainer>
          )}
        </Card>

        <Card title="Slowest suites (ms)">
          {durationData.length === 0 ? (
            <Empty msg="No timing data yet" />
          ) : (
            <ResponsiveContainer width="100%" height={220}>
              <BarChart data={durationData} layout="vertical" margin={{ top: 5, right: 20, left: 60, bottom: 5 }}>
                <CartesianGrid stroke="#21262d" strokeDasharray="3 3" />
                <XAxis type="number" tick={{ fontSize: 10, fill: '#8b949e' }} />
                <YAxis type="category" dataKey="suite" tick={{ fontSize: 10, fill: '#8b949e' }} width={100} />
                <Tooltip contentStyle={tooltipStyle} labelFormatter={(_, p) => (p?.[0]?.payload as any)?.full ?? ''} />
                <Bar dataKey="ms" fill="#388bfd" />
              </BarChart>
            </ResponsiveContainer>
          )}
        </Card>
      </div>

      <div className="rad-results-list">
        <div className="rad-results-header">
          <span>Suite results ({results.length})</span>
        </div>
        {results.length === 0 ? (
          <div className="rad-results-empty">No suites completed yet — {progress ? `running ${progress.current}/${progress.total}…` : 'waiting'}</div>
        ) : (
          <div className="rad-results-rows">
            {results.map((r, i) => {
              const failed = r.failed > 0 || r.total === 0;
              return (
                <div key={i} className={`rad-result-row ${failed ? 'rad-row-fail' : 'rad-row-pass'}`}>
                  <span className={`rad-row-dot ${failed ? 'rad-row-dot-fail' : 'rad-row-dot-pass'}`} />
                  <span className="rad-row-suite">{r.suite}</span>
                  <span className="rad-row-meta">{r.group}</span>
                  <span className="rad-row-meta">{r.env}</span>
                  <span className="rad-row-score">{r.passed}/{r.total}</span>
                  {r.durationMs != null && <span className="rad-row-dur">{fmtDuration(r.durationMs)}</span>}
                </div>
              );
            })}
          </div>
        )}
      </div>
    </div>
  );
};

const tooltipStyle = { background: '#1d1d1d', border: '1px solid #333', fontSize: 11 };

const Stat: React.FC<{ label: string; value: string; sub?: string; accent?: string }> = ({ label, value, sub, accent }) => (
  <div className="rad-stat">
    <div className="rad-stat-label">{label}</div>
    <div className="rad-stat-value" style={accent ? { color: accent } : undefined}>{value}</div>
    {sub && <div className="rad-stat-sub">{sub}</div>}
  </div>
);

const Card: React.FC<{ title: string; children: React.ReactNode }> = ({ title, children }) => (
  <div className="rad-card">
    <div className="rad-card-title">{title}</div>
    <div className="rad-card-body">{children}</div>
  </div>
);

const Empty: React.FC<{ msg: string }> = ({ msg }) => (
  <div className="rad-empty">{msg}</div>
);
