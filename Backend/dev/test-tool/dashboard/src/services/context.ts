import axios from 'axios';

const CONTEXT_API = 'http://localhost:7082';

export interface RiderInfo {
  person_id: string;
  first_name: string;
  merchant: string;
  city: string;
  token: string;
  verified: boolean;
}

export interface DriverInfo {
  person_id: string;
  first_name: string;
  merchant: string;
  merchant_id: string;
  city: string;
  currency: string;
  token: string;
  verified: boolean;
  vehicle_variant?: string;
}

export interface MerchantInfo {
  id: string;
  short_id: string;
  name: string;
  city_id: string;
  city: string;
  country: string;
  online_payment?: boolean;
  currency?: string;
}

export interface VariantInfo {
  id: string;
  service_tier_type: string;
  name: string;
  seating_capacity: number;
  is_enabled: boolean;
  merchant: string;
  city: string;
  currency: string;
}

export interface AdminCredentials {
  [merchantShortId: string]: { email: string; password: string };
}

export interface TestContext {
  merchants: { rider_merchants: MerchantInfo[]; driver_merchants: MerchantInfo[] };
  riders: RiderInfo[];
  drivers: DriverInfo[];
  variants: VariantInfo[];
  admin_credentials: AdminCredentials;
}

function safeArray<T>(val: any): T[] {
  return Array.isArray(val) ? val : [];
}

export async function fetchTestContext(): Promise<TestContext | null> {
  try {
    const resp = await axios.get(`${CONTEXT_API}/api/context`, { timeout: 5000 });
    const d = resp.data;
    return {
      merchants: {
        rider_merchants: safeArray(d?.merchants?.rider_merchants),
        driver_merchants: safeArray(d?.merchants?.driver_merchants),
      },
      riders: safeArray(d?.riders),
      drivers: safeArray(d?.drivers),
      variants: safeArray(d?.variants),
      admin_credentials: d?.admin_credentials || {},
    };
  } catch {
    return null;
  }
}

export async function fetchRiders(): Promise<RiderInfo[]> {
  try {
    const resp = await axios.get(`${CONTEXT_API}/api/riders`, { timeout: 5000 });
    return safeArray(resp.data);
  } catch {
    return [];
  }
}

export async function fetchDrivers(): Promise<DriverInfo[]> {
  try {
    const resp = await axios.get(`${CONTEXT_API}/api/drivers`, { timeout: 5000 });
    return safeArray(resp.data);
  } catch {
    return [];
  }
}
