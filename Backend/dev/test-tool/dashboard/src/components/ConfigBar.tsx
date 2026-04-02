import React, { useEffect, useState } from 'react';
import { Config } from '../types';
import { fetchTestContext, RiderInfo, DriverInfo, VariantInfo, MerchantInfo, AdminCredentials } from '../services/context';
import './ConfigBar.css';

interface Props {
  config: Config;
  onChange: (config: Config) => void;
  onRun: () => void;
  onStop: () => void;
  isRunning: boolean;
  onCityChange?: (city: string) => void;
  onDriverChange?: (driverToken: string, vehicleVariant?: string, merchantId?: string, driverPersonId?: string) => void;
  onMerchantShortIdChange?: (shortId: string) => void;
  onAdminCredentialsChange?: (email: string, password: string) => void;
}

export const ConfigBar: React.FC<Props> = ({ config, onChange, onRun, onStop, isRunning, onCityChange, onDriverChange, onMerchantShortIdChange, onAdminCredentialsChange }) => {
  const [riderMerchants, setRiderMerchants] = useState<MerchantInfo[]>([]);
  const [driverMerchants, setDriverMerchants] = useState<MerchantInfo[]>([]);
  const [adminCreds, setAdminCreds] = useState<AdminCredentials>({});
  const [riders, setRiders] = useState<RiderInfo[]>([]);
  const [drivers, setDrivers] = useState<DriverInfo[]>([]);
  const [variants, setVariants] = useState<VariantInfo[]>([]);
  const [contextLoaded, setContextLoaded] = useState(false);
  const [contextError, setContextError] = useState(false);

  const [selectedMerchant, setSelectedMerchant] = useState('');
  const [selectedCity, setSelectedCity] = useState('');
  const [selectedRider, setSelectedRider] = useState('');
  const [selectedDriver, setSelectedDriver] = useState('');

  const set = (key: keyof Config, value: string) => {
    const updated = { ...config, [key]: value };
    onChange(updated);
    localStorage.setItem('ny-test-config', JSON.stringify(updated));
  };

  useEffect(() => {
    fetchTestContext().then(ctx => {
      if (ctx) {
        const rm = ctx.merchants?.rider_merchants || [];
        const dm = ctx.merchants?.driver_merchants || [];
        const r = ctx.riders || [];
        const d = ctx.drivers || [];
        const v = ctx.variants || [];
        setRiderMerchants(rm);
        setDriverMerchants(dm);
        setRiders(r);
        setDrivers(d);
        setVariants(v);
        const ac = ctx.admin_credentials || {};
        setAdminCreds(ac);
        setContextLoaded(true);

        // Auto-select first merchant + city + rider
        const uniqueM = Array.from(new Map(rm.map((m: MerchantInfo) => [m.short_id, m])).values());
        if (uniqueM.length > 0) {
          const firstMerchant = uniqueM[0].short_id;
          setSelectedMerchant(firstMerchant);
          const cities = rm.filter((m: MerchantInfo) => m.short_id === firstMerchant);
          if (cities.length > 0) {
            setSelectedCity(cities[0].city);
            onCityChange?.(cities[0].city);
            // Find matching driver merchant (convention: rider "FOO" → driver "FOO_PARTNER")
            const driverMerch = dm.find((m: MerchantInfo) => m.short_id === firstMerchant + '_PARTNER') || dm.find((m: MerchantInfo) => m.short_id === firstMerchant);
            const driverShortId = driverMerch?.short_id || firstMerchant;
            onMerchantShortIdChange?.(driverShortId);
            const creds = ac[driverShortId];
            if (creds) onAdminCredentialsChange?.(creds.email, creds.password);
          }
          const merchantRiders = r.filter((ri: RiderInfo) => ri.merchant === firstMerchant);
          if (merchantRiders.length > 0 && !config.token) {
            setSelectedRider(merchantRiders[0].token);
            set('token', merchantRiders[0].token);
          }
          const merchantDrivers = d.filter((dr: DriverInfo) => dr.merchant?.includes(firstMerchant) || dr.merchant?.includes(firstMerchant.replace('_PARTNER', '')));
          if (merchantDrivers.length > 0) {
            setSelectedDriver(merchantDrivers[0].token);
            onDriverChange?.(merchantDrivers[0].token, merchantDrivers[0].vehicle_variant, merchantDrivers[0].merchant_id, merchantDrivers[0].person_id);
          }
        }
      } else {
        setContextError(true);
      }
    });
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  // Derived data
  const uniqueMerchants = Array.from(new Map(riderMerchants.map(m => [m.short_id, m])).values());
  const citiesForMerchant = selectedMerchant
    ? riderMerchants.filter(m => m.short_id === selectedMerchant)
    : riderMerchants;
  const ridersForMerchant = selectedMerchant
    ? riders.filter(r => r.merchant === selectedMerchant)
    : riders;
  const driversForMerchant = selectedMerchant
    ? drivers.filter(d => d.merchant?.includes(selectedMerchant) || d.merchant?.includes(selectedMerchant.replace('_PARTNER', '')))
    : drivers;
  const variantsForCity = selectedCity
    ? variants.filter(v => v.city === selectedCity)
    : variants;

  const handleMerchantChange = (merchant: string) => {
    setSelectedMerchant(merchant);
    setSelectedCity('');
    setSelectedRider('');
    setSelectedDriver('');
    const cities = riderMerchants.filter(m => m.short_id === merchant);
    if (cities.length > 0) {
      setSelectedCity(cities[0].city);
      onCityChange?.(cities[0].city);
      const driverMerch = driverMerchants.find(m => m.short_id === merchant + '_PARTNER') || driverMerchants.find(m => m.short_id === merchant);
      const driverShortId = driverMerch?.short_id || merchant;
      onMerchantShortIdChange?.(driverShortId);
      const creds = adminCreds[driverShortId];
      if (creds) onAdminCredentialsChange?.(creds.email, creds.password);
    }
    const merchantRiders = riders.filter(r => r.merchant === merchant);
    if (merchantRiders.length > 0) {
      setSelectedRider(merchantRiders[0].token);
      set('token', merchantRiders[0].token);
    }
    const merchantDrivers = drivers.filter(d => d.merchant?.includes(merchant) || d.merchant?.includes(merchant.replace('_PARTNER', '')));
    if (merchantDrivers.length > 0) {
      setSelectedDriver(merchantDrivers[0].token);
      onDriverChange?.(merchantDrivers[0].token, merchantDrivers[0].vehicle_variant, merchantDrivers[0].merchant_id, merchantDrivers[0].person_id);
    }
  };

  const handleRiderChange = (token: string) => {
    setSelectedRider(token);
    set('token', token);
  };

  if (!contextLoaded && !contextError) {
    return (
      <div className="config-bar">
        <div className="config-row">
          <div className="config-field" style={{ flex: 1 }}>
            <label>Loading test context from DB...</label>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="config-bar">
      {/* Row 1: URLs */}
      <div className="config-row">
        <div className="config-field">
          <label>Rider App</label>
          <input value={config.riderUrl} onChange={e => set('riderUrl', e.target.value)} />
        </div>
        <div className="config-field">
          <label>Driver App</label>
          <input value={config.driverUrl} onChange={e => set('driverUrl', e.target.value)} />
        </div>
        <div className="config-field">
          <label>Mock Stripe</label>
          <input value={config.stripeUrl} onChange={e => set('stripeUrl', e.target.value)} />
        </div>
        <div className="config-actions">
          <button className="btn-run" onClick={onRun} disabled={isRunning}>▶ Run</button>
          <button className="btn-stop" onClick={onStop} disabled={!isRunning}>■ Stop</button>
        </div>
      </div>

      {/* Row 2: Merchant + City + Rider + Driver */}
      <div className="config-row">
        {contextLoaded && riders.length > 0 ? (
          <>
            <div className="config-field">
              <label>Merchant <span className="ctx-badge">DB</span></label>
              <select value={selectedMerchant} onChange={e => handleMerchantChange(e.target.value)}>
                <option value="">All Merchants</option>
                {uniqueMerchants.map(m => (
                  <option key={m.short_id} value={m.short_id}>{m.short_id}</option>
                ))}
              </select>
            </div>
            <div className="config-field">
              <label>City</label>
              <select value={selectedCity} onChange={e => { setSelectedCity(e.target.value); onCityChange?.(e.target.value); }}>
                <option value="">All Cities</option>
                {Array.from(new Map(citiesForMerchant.map(c => [c.city, c])).values()).map(c => (
                  <option key={c.city_id} value={c.city}>{c.city} ({c.country})</option>
                ))}
              </select>
            </div>
            <div className="config-field" style={{ flex: 1.5 }}>
              <label>Rider ({ridersForMerchant.length})</label>
              <select value={selectedRider} onChange={e => handleRiderChange(e.target.value)}>
                <option value="">— Select Rider —</option>
                {ridersForMerchant.map(r => (
                  <option key={r.token} value={r.token}>
                    {r.first_name} • {r.merchant}/{r.city}
                  </option>
                ))}
              </select>
            </div>
            <div className="config-field" style={{ flex: 1.5 }}>
              <label>Driver ({driversForMerchant.length})</label>
              <select value={selectedDriver} onChange={e => { setSelectedDriver(e.target.value); const d = driversForMerchant.find(dr => dr.token === e.target.value); onDriverChange?.(e.target.value, d?.vehicle_variant, d?.merchant_id, d?.person_id); }}>
                <option value="">— Select Driver —</option>
                {driversForMerchant.map(d => (
                  <option key={d.token} value={d.token}>
                    {d.first_name} • {d.merchant}/{d.city}
                  </option>
                ))}
              </select>
            </div>
          </>
        ) : (
          <div className="config-field" style={{ flex: 1 }}>
            <label>
              Rider Auth Token
              {contextError && <span className="ctx-badge-warn">Context API offline (port 7082)</span>}
              {contextLoaded && riders.length === 0 && <span className="ctx-badge-warn">No riders in DB</span>}
            </label>
            <input type="password" value={config.token} onChange={e => set('token', e.target.value)}
              placeholder="Paste rider registration token" />
          </div>
        )}
      </div>

    </div>
  );
};
