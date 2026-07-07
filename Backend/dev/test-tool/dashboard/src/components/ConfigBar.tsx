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

// BAP merchant → BPP merchant mapping for cases where the naming convention
// (FOO → FOO_PARTNER) doesn't hold. E.g. ANNA_APP (Chennai BAP) is served
// by NAMMA_YATRI_PARTNER which covers Chennai in its geo-restrictions.
const BAP_TO_BPP: Record<string, string> = {
  ANNA_APP: 'NAMMA_YATRI_PARTNER',
};

const resolveBppMerchant = (bapShortId: string, driverMerchants: MerchantInfo[]): string => {
  if (BAP_TO_BPP[bapShortId]) return BAP_TO_BPP[bapShortId];
  const direct = driverMerchants.find(m => m.short_id === bapShortId + '_PARTNER')
    || driverMerchants.find(m => m.short_id === bapShortId);
  return direct?.short_id || bapShortId + '_PARTNER';
};

const normalizeLabel = (value: string) => value.toLowerCase().replace(/[_\s-]+/g, '');

const cityMatches = (left?: string, right?: string): boolean => {
  if (!right) return true;   // no city filter → any driver city matches
  if (!left) return false;   // driver has no city → doesn't match a specific city
  return normalizeLabel(left) === normalizeLabel(right);
};

const selectDriverForMerchantCity = (
  merchant: string,
  city: string,
  drivers: DriverInfo[],
  driverMerchants: MerchantInfo[],
): DriverInfo | undefined => {
  const resolvedBpp = resolveBppMerchant(merchant, driverMerchants);
  // Prefer drivers that have a vehicle (vehicle_variant set) — drivers without a
  // vehicle record are filtered out by the allocator's driver pool processing.
  return drivers.find(dr => dr.merchant === resolvedBpp && cityMatches(dr.city, city) && !!dr.vehicle_variant)
    || drivers.find(dr => dr.merchant === resolvedBpp && cityMatches(dr.city, city))
    || drivers.find(dr => dr.merchant === resolvedBpp && !!dr.vehicle_variant)
    || drivers.find(dr => dr.merchant === resolvedBpp)
    || drivers.find(dr => cityMatches(dr.city, city) && !!dr.vehicle_variant)
    || drivers.find(dr => cityMatches(dr.city, city));
};

export const ConfigBar: React.FC<Props> = ({ config, onChange, onRun, onStop, isRunning, onCityChange, onDriverChange, onMerchantShortIdChange, onAdminCredentialsChange }) => {
  const [riderMerchants, setRiderMerchants] = useState<MerchantInfo[]>([]);
  const [driverMerchants, setDriverMerchants] = useState<MerchantInfo[]>([]);
  const [adminCreds, setAdminCreds] = useState<AdminCredentials>({});
  const [riders, setRiders] = useState<RiderInfo[]>([]);
  const [drivers, setDrivers] = useState<DriverInfo[]>([]);
  const [variants, setVariants] = useState<VariantInfo[]>([]);
  const [contextLoaded, setContextLoaded] = useState(false);
  const [contextError, setContextError] = useState(false);
  // Note: Flush Redis + Sync Data have moved to <TopBarActions /> in the
  // top bar (next to DB Explorer / Redis Explorer).

  const [selectedMerchant, setSelectedMerchant] = useState('');
  const [selectedCity, setSelectedCity] = useState('');
  const [selectedRider, setSelectedRider] = useState('');
  const [selectedDriver, setSelectedDriver] = useState('');

  const syncDriverSelection = (merchant: string, city: string, nextDrivers = drivers, nextDriverMerchants = driverMerchants) => {
    const selected = merchant ? selectDriverForMerchantCity(merchant, city, nextDrivers, nextDriverMerchants) : undefined;
    if (selected) {
      setSelectedDriver(selected.token);
      onDriverChange?.(selected.token, selected.vehicle_variant, selected.merchant_id, selected.person_id);
    } else {
      setSelectedDriver('');
      onDriverChange?.('', undefined, undefined, undefined);
    }
  };

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

        // Auto-select first merchant + city (prefer city that has a driver) + rider
        const uniqueM = Array.from(new Map(rm.map((m: MerchantInfo) => [m.short_id, m])).values());
        if (uniqueM.length > 0) {
          const firstMerchant = uniqueM[0].short_id;
          setSelectedMerchant(firstMerchant);
          const cities = rm.filter((m: MerchantInfo) => m.short_id === firstMerchant);
          const driverShortId = resolveBppMerchant(firstMerchant, dm);
          let initCity = '';
          if (cities.length > 0) {
            const driverCities = d.filter((dr: DriverInfo) => dr.merchant === driverShortId && dr.city);
            const preferred = cities.find(c => driverCities.some((dr: DriverInfo) => cityMatches(dr.city, c.city))) || cities[0];
            initCity = preferred.city;
            setSelectedCity(initCity);
            onCityChange?.(initCity);
            onMerchantShortIdChange?.(driverShortId);
            const creds = ac[driverShortId];
            if (creds) onAdminCredentialsChange?.(creds.email, creds.password);
          }
          const merchantRiders = r.filter((ri: RiderInfo) => ri.merchant === firstMerchant);
          if (merchantRiders.length > 0 && !config.token) {
            setSelectedRider(merchantRiders[0].token);
            set('token', merchantRiders[0].token);
          }
          syncDriverSelection(firstMerchant, initCity, d, dm);
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
  const resolvedBpp = selectedMerchant ? resolveBppMerchant(selectedMerchant, driverMerchants) : '';
  const driversForMerchant = (() => {
    if (!selectedMerchant) return drivers;
    const cityFiltered = drivers.filter(d =>
      d.merchant === resolvedBpp &&
      (!selectedCity || !d.city || cityMatches(d.city, selectedCity))
    );
    // If no city-matched driver, show all drivers for this merchant so user can still see/pick them
    return cityFiltered.length > 0
      ? cityFiltered
      : drivers.filter(d => d.merchant === resolvedBpp);
  })();

  const handleMerchantChange = (merchant: string) => {
    setSelectedMerchant(merchant);
    setSelectedCity('');
    setSelectedRider('');
    setSelectedDriver('');
    const cities = riderMerchants.filter(m => m.short_id === merchant);
    const driverShortId = resolveBppMerchant(merchant, driverMerchants);
    let newCity = '';
    if (cities.length > 0) {
      // Prefer a city that has a matching driver so coordinates align; fall back to first city
      const driverCities = drivers.filter(d => d.merchant === driverShortId && d.city);
      const preferred = cities.find(c => driverCities.some(d => cityMatches(d.city, c.city))) || cities[0];
      newCity = preferred.city;
      setSelectedCity(newCity);
      onCityChange?.(newCity);
      onMerchantShortIdChange?.(driverShortId);
      const creds = adminCreds[driverShortId];
      if (creds) onAdminCredentialsChange?.(creds.email, creds.password);
    }
    const merchantRiders = riders.filter(r => r.merchant === merchant);
    if (merchantRiders.length > 0) {
      setSelectedRider(merchantRiders[0].token);
      set('token', merchantRiders[0].token);
    }
    syncDriverSelection(merchant, newCity);
  };

  const handleCityChange = (city: string) => {
    setSelectedCity(city);
    onCityChange?.(city);
    syncDriverSelection(selectedMerchant, city);
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
              <select value={selectedCity} onChange={e => handleCityChange(e.target.value)}>
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
