import React, { useCallback, useEffect, useState } from 'react';
import {
  BarChart, Bar, LineChart, Line, XAxis, YAxis, Tooltip, Legend,
  ResponsiveContainer, CartesianGrid,
} from 'recharts';
import { PROXY_BASE } from '../config';

type RangeOption = { label: string; seconds: number; step: number };

const RANGES: RangeOption[] = [
  { label: '1h',  seconds: 3600,    step: 60   },
  { label: '6h',  seconds: 21600,   step: 300  },
  { label: '24h', seconds: 86400,   step: 900  },
  { label: '7d',  seconds: 604800,  step: 3600 },
];

interface VMQueryResp {
  status: string;
  data?: {
    resultType: string;
    result: Array<{ metric: Record<string, string>; values?: [number, string][]; value?: [number, string] }>;
  };
  error?: string;
}

async function vmQueryRange(query: string, seconds: number, step: number): Promise<VMQueryResp> {
  const end = Math.floor(Date.now() / 1000);
  const start = end - seconds;
  const url = `${PROXY_BASE}/api/metrics/v1/query_range?query=${encodeURIComponent(query)}&start=${start}&end=${end}&step=${step}`;
  const r = await fetch(url);
  return r.json();
}

async function vmQueryInstant(query: string): Promise<VMQueryResp> {
  const url = `${PROXY_BASE}/api/metrics/v1/query?query=${encodeURIComponent(query)}`;
  const r = await fetch(url);
  return r.json();
}

export const MetricsPanel: React.FC<{ collection?: string }> = ({ collection }) => {
  const [range, setRange] = useState<RangeOption>(RANGES[2]);
  const [passFail, setPassFail] = useState<Array<{ suite: string; pass: number; fail: number }>>([]);
  const [latency, setLatency] = useState<Array<{ ts: string; p50: number; p95: number }>>([]);
  const [error, setError] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);
  const [lastUpdated, setLastUpdated] = useState<Date | null>(null);

  const collFilter = collection ? `,collection="${collection}"` : '';

  const refresh = useCallback(async () => {
    setLoading(true);
    setError(null);
    try {
      // Pass / fail counts per suite over the window (sum samples by suite).
      const pfQuery = `sum by (suite, status) (sum_over_time(integration_test_suite_runs{${collFilter.slice(1)}}[${range.seconds}s]))`;
      const pfResp = await vmQueryInstant(pfQuery);
      if (pfResp.status !== 'success') throw new Error(pfResp.error || 'pass/fail query failed');
      const bySuite: Record<string, { pass: number; fail: number }> = {};
      for (const series of pfResp.data?.result ?? []) {
        const suite = series.metric.suite || 'unknown';
        const status = series.metric.status || 'pass';
        const val = parseFloat(series.value?.[1] ?? '0');
        bySuite[suite] = bySuite[suite] || { pass: 0, fail: 0 };
        if (status === 'fail') bySuite[suite].fail += val;
        else bySuite[suite].pass += val;
      }
      setPassFail(Object.entries(bySuite)
        .map(([suite, v]) => ({ suite: shortName(suite), pass: v.pass, fail: v.fail }))
        .sort((a, b) => (b.pass + b.fail) - (a.pass + a.fail))
        .slice(0, 20));

      // Suite latency p50 / p95 over time.
      const p50Q = `quantile_over_time(0.5, integration_test_suite_latency_ms{${collFilter.slice(1)}}[${range.step}s])`;
      const p95Q = `quantile_over_time(0.95, integration_test_suite_latency_ms{${collFilter.slice(1)}}[${range.step}s])`;
      const [p50Resp, p95Resp] = await Promise.all([
        vmQueryRange(p50Q, range.seconds, range.step),
        vmQueryRange(p95Q, range.seconds, range.step),
      ]);
      const tsMap: Record<number, { p50?: number; p95?: number }> = {};
      const collectMax = (resp: VMQueryResp, key: 'p50' | 'p95') => {
        for (const series of resp.data?.result ?? []) {
          for (const [ts, v] of series.values ?? []) {
            const cur = tsMap[ts] || (tsMap[ts] = {});
            const num = parseFloat(v);
            if (!isNaN(num)) cur[key] = Math.max(cur[key] ?? 0, num);
          }
        }
      };
      collectMax(p50Resp, 'p50');
      collectMax(p95Resp, 'p95');
      const series = Object.keys(tsMap)
        .map(k => parseInt(k, 10))
        .sort((a, b) => a - b)
        .map(ts => ({
          ts: new Date(ts * 1000).toLocaleTimeString(),
          p50: Math.round(tsMap[ts].p50 ?? 0),
          p95: Math.round(tsMap[ts].p95 ?? 0),
        }));
      setLatency(series);
      setLastUpdated(new Date());
    } catch (e: any) {
      setError(e?.message || String(e));
    } finally {
      setLoading(false);
    }
  }, [collection, range, collFilter]);

  useEffect(() => { refresh(); }, [refresh]);

  const totalPass = passFail.reduce((s, r) => s + r.pass, 0);
  const totalFail = passFail.reduce((s, r) => s + r.fail, 0);
  const total = totalPass + totalFail;
  const passRate = total > 0 ? Math.round((totalPass / total) * 100) : 0;

  return (
    <div className="metrics-panel" style={{ border: '1px solid #2a2a2a', borderRadius: 6, padding: 12, marginBottom: 12, background: '#161616' }}>
      <div style={{ display: 'flex', alignItems: 'center', gap: 12, marginBottom: 10 }}>
        <span style={{ fontWeight: 600 }}>Metrics</span>
        {collection && <span style={{ color: '#888', fontSize: 12 }}>collection: {collection}</span>}
        <span style={{ flex: 1 }} />
        <span style={{ fontSize: 12, color: total === 0 ? '#888' : passRate >= 95 ? '#7ec97e' : passRate >= 80 ? '#e3c25f' : '#e07171' }}>
          {total > 0 ? `${totalPass}/${total} pass (${passRate}%)` : 'no data'}
        </span>
        <select value={range.label} onChange={e => setRange(RANGES.find(r => r.label === e.target.value) || RANGES[2])}>
          {RANGES.map(r => <option key={r.label} value={r.label}>{r.label}</option>)}
        </select>
        <button onClick={refresh} disabled={loading}>{loading ? '…' : '↻'}</button>
        {lastUpdated && <span style={{ fontSize: 11, color: '#666' }}>{lastUpdated.toLocaleTimeString()}</span>}
      </div>

      {error && <div style={{ color: '#e07171', fontSize: 12, marginBottom: 8 }}>{error}</div>}

      <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: 12 }}>
        <div>
          <div style={{ fontSize: 12, color: '#aaa', marginBottom: 4 }}>Pass / fail per suite</div>
          {passFail.length === 0 ? (
            <div style={{ height: 180, display: 'flex', alignItems: 'center', justifyContent: 'center', color: '#666', fontSize: 12 }}>no runs in window</div>
          ) : (
            <ResponsiveContainer width="100%" height={180}>
              <BarChart data={passFail}>
                <CartesianGrid stroke="#2a2a2a" strokeDasharray="3 3" />
                <XAxis dataKey="suite" tick={{ fontSize: 10, fill: '#aaa' }} interval={0} angle={-25} textAnchor="end" height={60} />
                <YAxis tick={{ fontSize: 10, fill: '#aaa' }} />
                <Tooltip contentStyle={{ background: '#1d1d1d', border: '1px solid #333' }} />
                <Legend wrapperStyle={{ fontSize: 11 }} />
                <Bar dataKey="pass" stackId="a" fill="#7ec97e" />
                <Bar dataKey="fail" stackId="a" fill="#e07171" />
              </BarChart>
            </ResponsiveContainer>
          )}
        </div>
        <div>
          <div style={{ fontSize: 12, color: '#aaa', marginBottom: 4 }}>Suite latency p50 / p95 (ms)</div>
          {latency.length === 0 ? (
            <div style={{ height: 180, display: 'flex', alignItems: 'center', justifyContent: 'center', color: '#666', fontSize: 12 }}>no runs in window</div>
          ) : (
            <ResponsiveContainer width="100%" height={180}>
              <LineChart data={latency}>
                <CartesianGrid stroke="#2a2a2a" strokeDasharray="3 3" />
                <XAxis dataKey="ts" tick={{ fontSize: 10, fill: '#aaa' }} minTickGap={30} />
                <YAxis tick={{ fontSize: 10, fill: '#aaa' }} />
                <Tooltip contentStyle={{ background: '#1d1d1d', border: '1px solid #333' }} />
                <Legend wrapperStyle={{ fontSize: 11 }} />
                <Line type="monotone" dataKey="p50" stroke="#7e9ec9" dot={false} strokeWidth={2} />
                <Line type="monotone" dataKey="p95" stroke="#e3c25f" dot={false} strokeWidth={2} />
              </LineChart>
            </ResponsiveContainer>
          )}
        </div>
      </div>
    </div>
  );
};

function shortName(suite: string): string {
  // suite labels look like "BusTicketBookingFlow-Local_FRFS_Bhubaneshwar-Direct_Bus_Booking"
  // — trim to the last segment so the bar chart stays readable.
  const parts = suite.split('-');
  return parts.length > 2 ? `${parts[0]}/${parts[parts.length - 1]}` : suite;
}
