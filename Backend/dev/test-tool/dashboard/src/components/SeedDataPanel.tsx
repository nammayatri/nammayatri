/**
 * SeedDataPanel — UI replacement for the AddSeedData Postman collection.
 *
 * Lets the user pick:
 *   Kind     (Normal / International / Fleet)  →
 *   Merchant (filtered by Kind)                →
 *   City     (filtered by Merchant)            →
 *   Variant  (filtered by service tiers in DB) →
 * …and POSTs /api/seed/run to perform driver+customer auth, dashboard
 * switchMerchantAndCity, addVehicle and enable in one shot.
 */

import React, { useEffect, useMemo, useState } from 'react';
import './SeedDataPanel.css';

const PROXY_BASE = 'http://localhost:7082';

type Kind = 'normal' | 'international' | 'fleet';

interface Merchant {
  merchant_id: string;
  merchant_short_id: string;
  merchant_name: string;
  country: string;
  kind: Kind;
}
interface CityRow {
  city_id: string;
  city: string;
  state: string;
  country: string;
}
interface VariantRow {
  vehicle_variant: string;
  vehicle_category: string | null;
  service_tier_type: string;
  service_tier_name: string;
  seating_capacity: number | null;
  is_air_conditioned: boolean | null;
}
interface SeedRunResult {
  ok: boolean;
  log?: string[];
  error?: string;
  driver?: { id: string; phone: string; registrationNo: string; variant: string };
  rider?: { phone: string };
  merchant?: { id: string; short_id: string; bap_short_id: string };
  city?: { id: string; name: string; country: string };
}

const KIND_LABELS: Record<Kind, string> = {
  normal: 'Normal',
  international: 'International',
  fleet: 'Fleet',
};

export const SeedDataPanel: React.FC = () => {
  const [kind, setKind] = useState<Kind>('normal');
  const [merchants, setMerchants] = useState<Merchant[]>([]);
  const [merchantId, setMerchantId] = useState('');

  const [cities, setCities] = useState<CityRow[]>([]);
  const [cityId, setCityId] = useState('');

  const [variants, setVariants] = useState<VariantRow[]>([]);
  const [variant, setVariant] = useState('');

  const [running, setRunning] = useState(false);
  const [result, setResult] = useState<SeedRunResult | null>(null);
  const [error, setError] = useState<string | null>(null);

  // --- Load merchants whenever Kind changes ---
  useEffect(() => {
    setMerchantId('');
    setCities([]); setCityId('');
    setVariants([]); setVariant('');
    fetch(`${PROXY_BASE}/api/seed/merchants?kind=${kind}`)
      .then(r => r.json())
      .then((rows: Merchant[]) => {
        setMerchants(Array.isArray(rows) ? rows : []);
        if (Array.isArray(rows) && rows.length > 0) setMerchantId(rows[0].merchant_id);
      })
      .catch(e => setError(`Failed to load merchants: ${e.message}`));
  }, [kind]);

  // --- Load cities when merchant changes ---
  useEffect(() => {
    if (!merchantId) return;
    setCityId(''); setVariants([]); setVariant('');
    fetch(`${PROXY_BASE}/api/seed/cities?merchant_id=${encodeURIComponent(merchantId)}`)
      .then(r => r.json())
      .then((rows: CityRow[]) => {
        setCities(Array.isArray(rows) ? rows : []);
        if (Array.isArray(rows) && rows.length > 0) setCityId(rows[0].city_id);
      })
      .catch(e => setError(`Failed to load cities: ${e.message}`));
  }, [merchantId]);

  // --- Load vehicle service tiers when city changes ---
  useEffect(() => {
    if (!cityId) return;
    setVariant('');
    fetch(`${PROXY_BASE}/api/seed/service-tiers?city_id=${encodeURIComponent(cityId)}`)
      .then(r => r.json())
      .then((data: { variants: VariantRow[] } | VariantRow[]) => {
        const vs = Array.isArray(data) ? data : data.variants || [];
        setVariants(vs);
        if (vs.length > 0) setVariant(vs[0].vehicle_variant);
      })
      .catch(e => setError(`Failed to load service tiers: ${e.message}`));
  }, [cityId]);

  const selectedMerchant = useMemo(
    () => merchants.find(m => m.merchant_id === merchantId),
    [merchants, merchantId]
  );

  const seed = async () => {
    if (!merchantId || !cityId || !variant) return;
    setRunning(true); setResult(null); setError(null);
    try {
      const r = await fetch(`${PROXY_BASE}/api/seed/run`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ kind, merchantId, cityId, vehicleVariant: variant }),
      });
      const d: SeedRunResult = await r.json();
      setResult(d);
    } catch (e: any) {
      setError(e.message);
    } finally {
      setRunning(false);
    }
  };

  return (
    <div className="seed-panel">
      <div className="seed-panel-header">
        <h2>Seed Data</h2>
        <p className="seed-panel-sub">
          Provision a fresh customer + driver with a vehicle of the chosen
          variant, against any (merchant, city) the dashboard JUSPAY_ADMIN
          has access to. Replaces the old AddSeedData Postman collection.
        </p>
      </div>

      <div className="seed-row">
        <div className="tree-config-field">
          <label>Kind</label>
          <select value={kind} onChange={e => setKind(e.target.value as Kind)}>
            {(Object.keys(KIND_LABELS) as Kind[]).map(k => (
              <option key={k} value={k}>{KIND_LABELS[k]}</option>
            ))}
          </select>
        </div>

        <div className="tree-config-field">
          <label>Merchant ({merchants.length})</label>
          <select value={merchantId} onChange={e => setMerchantId(e.target.value)} disabled={!merchants.length}>
            {merchants.map(m => (
              <option key={m.merchant_id} value={m.merchant_id}>
                {m.merchant_short_id} {m.merchant_name ? `— ${m.merchant_name}` : ''}
              </option>
            ))}
          </select>
        </div>

        <div className="tree-config-field">
          <label>City ({cities.length})</label>
          <select value={cityId} onChange={e => setCityId(e.target.value)} disabled={!cities.length}>
            {cities.map(c => (
              <option key={c.city_id} value={c.city_id}>
                {c.city}{c.country && c.country !== 'India' ? ` (${c.country})` : ''}
              </option>
            ))}
          </select>
        </div>

        <div className="tree-config-field">
          <label>Vehicle Variant ({variants.length})</label>
          <select value={variant} onChange={e => setVariant(e.target.value)} disabled={!variants.length}>
            {variants.map(v => (
              <option key={v.vehicle_variant} value={v.vehicle_variant}>
                {v.vehicle_variant}
                {v.service_tier_name ? ` — ${v.service_tier_name}` : ''}
                {v.seating_capacity ? ` (${v.seating_capacity}p)` : ''}
                {v.is_air_conditioned ? ' AC' : ''}
              </option>
            ))}
          </select>
        </div>
      </div>

      <div className="seed-actions">
        <button
          className="seed-run-btn"
          onClick={seed}
          disabled={running || !merchantId || !cityId || !variant}
          title={!variant ? 'Pick a variant first' : `Seed driver + customer in ${selectedMerchant?.merchant_short_id} / ${cityId}`}
        >
          {running ? 'Seeding…' : 'Seed Driver + Customer'}
        </button>
      </div>

      {error && (
        <div className="seed-result seed-result-err">
          <strong>Error:</strong> {error}
        </div>
      )}

      {result && (
        <div className={`seed-result ${result.ok ? 'seed-result-ok' : 'seed-result-err'}`}>
          <div className="seed-result-summary">
            {result.ok ? (
              <>
                <strong>✓ Seeded</strong>
                {result.driver && (
                  <span> driver <code>{result.driver.id}</code> · phone <code>{result.driver.phone}</code> · reg <code>{result.driver.registrationNo}</code></span>
                )}
                {result.rider && <span> · rider <code>{result.rider.phone}</code></span>}
              </>
            ) : (
              <strong>✗ Failed{result.error ? `: ${result.error}` : ''}</strong>
            )}
          </div>
          {result.log && result.log.length > 0 && (
            <pre className="seed-result-log">{result.log.join('\n')}</pre>
          )}
        </div>
      )}
    </div>
  );
};
