/**
 * Mock Location Data — Pickup and Drop locations grouped by city.
 * Used across search, estimates, and other location-based APIs.
 */

import { LocationPreset } from '../api-catalog/types';

// ===================== KOCHI (YATRI) =====================
export const kochiLocations: LocationPreset[] = [
  {
    name: 'Ernakulam Junction Railway Station',
    city: 'Kochi',
    gps: { lat: 9.9816, lon: 76.2999 },
    address: { street: 'Railway Station Rd', city: 'Kochi', state: 'Kerala', country: 'India', area: 'Ernakulam', areaCode: '682016', title: 'Ernakulam Junction' },
  },
  {
    name: 'Lulu Mall Kochi',
    city: 'Kochi',
    gps: { lat: 10.0271, lon: 76.3083 },
    address: { street: 'NH 47', city: 'Kochi', state: 'Kerala', country: 'India', area: 'Edappally', areaCode: '682024', title: 'Lulu Mall' },
  },
  {
    name: 'Fort Kochi Beach',
    city: 'Kochi',
    gps: { lat: 9.9658, lon: 76.2422 },
    address: { street: 'Beach Rd', city: 'Kochi', state: 'Kerala', country: 'India', area: 'Fort Kochi', areaCode: '682001', title: 'Fort Kochi Beach' },
  },
  {
    name: 'Cochin International Airport',
    city: 'Kochi',
    gps: { lat: 10.1520, lon: 76.3929 },
    address: { street: 'Airport Rd', city: 'Kochi', state: 'Kerala', country: 'India', area: 'Nedumbassery', areaCode: '683572', title: 'CIAL Airport' },
  },
  {
    name: 'Marine Drive Kochi',
    city: 'Kochi',
    gps: { lat: 9.9774, lon: 76.2823 },
    address: { street: 'Marine Drive', city: 'Kochi', state: 'Kerala', country: 'India', area: 'Marine Drive', areaCode: '682031', title: 'Marine Drive' },
  },
];

// ===================== BANGALORE (NAMMA_YATRI) =====================
export const bangaloreLocations: LocationPreset[] = [
  {
    name: 'Koramangala 4th Block',
    city: 'Bangalore',
    gps: { lat: 12.9352, lon: 77.6245 },
    address: { street: '80 Feet Rd', city: 'Bengaluru', state: 'Karnataka', country: 'India', area: 'Koramangala', areaCode: '560034', title: 'Koramangala 4th Block' },
  },
  {
    name: 'Indiranagar 100 Feet Road',
    city: 'Bangalore',
    gps: { lat: 12.9784, lon: 77.6408 },
    address: { street: '100 Feet Rd', city: 'Bengaluru', state: 'Karnataka', country: 'India', area: 'Indiranagar', areaCode: '560038', title: 'Indiranagar' },
  },
  {
    name: 'Kempegowda International Airport',
    city: 'Bangalore',
    gps: { lat: 13.1989, lon: 77.7068 },
    address: { street: 'Airport Rd', city: 'Bengaluru', state: 'Karnataka', country: 'India', area: 'Devanahalli', areaCode: '560300', title: 'KIA Airport' },
  },
  {
    name: 'MG Road Metro Station',
    city: 'Bangalore',
    gps: { lat: 12.9756, lon: 77.6063 },
    address: { street: 'MG Road', city: 'Bengaluru', state: 'Karnataka', country: 'India', area: 'MG Road', areaCode: '560001', title: 'MG Road' },
  },
  {
    name: 'Electronic City Phase 1',
    city: 'Bangalore',
    gps: { lat: 12.8399, lon: 77.6771 },
    address: { street: 'Hosur Rd', city: 'Bengaluru', state: 'Karnataka', country: 'India', area: 'Electronic City', areaCode: '560100', title: 'Electronic City' },
  },
];

// ===================== CHENNAI (ANNA_APP) =====================
export const chennaiLocations: LocationPreset[] = [
  {
    name: 'Chennai Central Railway Station',
    city: 'Chennai',
    gps: { lat: 13.0827, lon: 80.2707 },
    address: { street: 'Park Town', city: 'Chennai', state: 'Tamil Nadu', country: 'India', area: 'Park Town', areaCode: '600003', title: 'Chennai Central' },
  },
  {
    name: 'Chennai Airport',
    city: 'Chennai',
    gps: { lat: 12.9941, lon: 80.1709 },
    address: { street: 'Tirusulam', city: 'Chennai', state: 'Tamil Nadu', country: 'India', area: 'Tirusulam', areaCode: '600016', title: 'Chennai Airport' },
  },
  {
    name: 'T. Nagar Bus Terminus',
    city: 'Chennai',
    gps: { lat: 13.0418, lon: 80.2341 },
    address: { street: 'Thyagaraya Rd', city: 'Chennai', state: 'Tamil Nadu', country: 'India', area: 'T. Nagar', areaCode: '600017', title: 'T. Nagar' },
  },
  {
    name: 'Anna Nagar Tower Park',
    city: 'Chennai',
    gps: { lat: 13.0892, lon: 80.2090 },
    address: { street: '3rd Avenue', city: 'Chennai', state: 'Tamil Nadu', country: 'India', area: 'Anna Nagar', areaCode: '600040', title: 'Anna Nagar' },
  },
  {
    name: 'Adyar Bus Depot',
    city: 'Chennai',
    gps: { lat: 13.0067, lon: 80.2561 },
    address: { street: 'LB Road', city: 'Chennai', state: 'Tamil Nadu', country: 'India', area: 'Adyar', areaCode: '600020', title: 'Adyar' },
  },
];

// ===================== DELHI (BHARAT_TAXI) =====================
export const delhiLocations: LocationPreset[] = [
  {
    name: 'Connaught Place',
    city: 'Delhi',
    gps: { lat: 28.6315, lon: 77.2167 },
    address: { street: 'Connaught Circus', city: 'New Delhi', state: 'Delhi', country: 'India', area: 'Connaught Place', areaCode: '110001', title: 'Connaught Place' },
  },
  {
    name: 'IGI Airport Terminal 3',
    city: 'Delhi',
    gps: { lat: 28.5562, lon: 77.1000 },
    address: { street: 'Airport Rd', city: 'New Delhi', state: 'Delhi', country: 'India', area: 'Aerocity', areaCode: '110037', title: 'IGI T3' },
  },
  {
    name: 'India Gate',
    city: 'Delhi',
    gps: { lat: 28.6129, lon: 77.2295 },
    address: { street: 'Rajpath', city: 'New Delhi', state: 'Delhi', country: 'India', area: 'India Gate', areaCode: '110001', title: 'India Gate' },
  },
  {
    name: 'Chandni Chowk',
    city: 'Delhi',
    gps: { lat: 28.6507, lon: 77.2334 },
    address: { street: 'Chandni Chowk', city: 'Old Delhi', state: 'Delhi', country: 'India', area: 'Chandni Chowk', areaCode: '110006', title: 'Chandni Chowk' },
  },
  {
    name: 'Hauz Khas Village',
    city: 'Delhi',
    gps: { lat: 28.5494, lon: 77.2001 },
    address: { street: 'Hauz Khas Village Rd', city: 'New Delhi', state: 'Delhi', country: 'India', area: 'Hauz Khas', areaCode: '110016', title: 'Hauz Khas Village' },
  },
];

// ===================== HELSINKI (LYNX) =====================
export const helsinkiLocations: LocationPreset[] = [
  {
    name: 'Helsinki Central Station',
    city: 'Helsinki',
    gps: { lat: 60.1719, lon: 24.9414 },
    address: { street: 'Kaivokatu 1', city: 'Helsinki', state: 'Uusimaa', country: 'Finland', area: 'Kluuvi', areaCode: '00100', title: 'Central Station' },
  },
  {
    name: 'Helsinki-Vantaa Airport',
    city: 'Helsinki',
    gps: { lat: 60.3172, lon: 24.9633 },
    address: { street: 'Lentoasemantie 1', city: 'Vantaa', state: 'Uusimaa', country: 'Finland', area: 'Vantaa', areaCode: '01530', title: 'Helsinki Airport' },
  },
  {
    name: 'Senate Square',
    city: 'Helsinki',
    gps: { lat: 60.1695, lon: 24.9527 },
    address: { street: 'Senaatintori', city: 'Helsinki', state: 'Uusimaa', country: 'Finland', area: 'Kruununhaka', areaCode: '00170', title: 'Senate Square' },
  },
  {
    name: 'Suomenlinna Ferry Terminal',
    city: 'Helsinki',
    gps: { lat: 60.1599, lon: 24.9555 },
    address: { street: 'Kauppatori', city: 'Helsinki', state: 'Uusimaa', country: 'Finland', area: 'Kauppatori', areaCode: '00170', title: 'Market Square Ferry' },
  },
];

// ===================== KOLKATA (JATRI_SAATHI) =====================
export const kolkataLocations: LocationPreset[] = [
  {
    name: 'Howrah Junction Railway Station',
    city: 'Kolkata',
    gps: { lat: 22.5830, lon: 88.3426 },
    address: {
      street: 'Station Rd',
      city: 'Howrah',
      state: 'West Bengal',
      country: 'India',
      area: 'Howrah',
      areaCode: '711101',
      title: 'Howrah Junction',
    },
  },
  {
    name: 'Netaji Subhas Chandra Bose International Airport',
    city: 'Kolkata',
    gps: { lat: 22.6547, lon: 88.4467 },
    address: {
      street: 'Jessore Rd',
      city: 'Kolkata',
      state: 'West Bengal',
      country: 'India',
      area: 'Dum Dum',
      areaCode: '700052',
      title: 'Kolkata Airport',
    },
  },
  {
    name: 'Esplanade Metro Station',
    city: 'Kolkata',
    gps: { lat: 22.5656, lon: 88.3528 },
    address: {
      street: 'Jawaharlal Nehru Rd',
      city: 'Kolkata',
      state: 'West Bengal',
      country: 'India',
      area: 'Esplanade',
      areaCode: '700069',
      title: 'Esplanade',
    },
  },
  {
    name: 'Park Street',
    city: 'Kolkata',
    gps: { lat: 22.5519, lon: 88.3527 },
    address: {
      street: 'Park St',
      city: 'Kolkata',
      state: 'West Bengal',
      country: 'India',
      area: 'Park Street',
      areaCode: '700016',
      title: 'Park Street',
    },
  },
  {
    name: 'Salt Lake Sector V',
    city: 'Kolkata',
    gps: { lat: 22.5697, lon: 88.4335 },
    address: {
      street: 'Sector V',
      city: 'Kolkata',
      state: 'West Bengal',
      country: 'India',
      area: 'Salt Lake',
      areaCode: '700091',
      title: 'Sector V',
    },
  },
  {
    name: 'Sealdah Railway Station',
    city: 'Kolkata',
    gps: { lat: 22.5676, lon: 88.3702 },
    address: {
      street: 'Beliaghata Rd',
      city: 'Kolkata',
      state: 'West Bengal',
      country: 'India',
      area: 'Sealdah',
      areaCode: '700014',
      title: 'Sealdah Station',
    },
  },
  {
    name: 'New Town Eco Park',
    city: 'Kolkata',
    gps: { lat: 22.5804, lon: 88.4677 },
    address: {
      street: 'Major Arterial Rd',
      city: 'Kolkata',
      state: 'West Bengal',
      country: 'India',
      area: 'New Town',
      areaCode: '700156',
      title: 'Eco Park',
    },
  },
  {
    name: 'Dakshineswar Kali Temple',
    city: 'Kolkata',
    gps: { lat: 22.6551, lon: 88.3573 },
    address: {
      street: 'Dakshineswar',
      city: 'Kolkata',
      state: 'West Bengal',
      country: 'India',
      area: 'Dakshineswar',
      areaCode: '700076',
      title: 'Dakshineswar Temple',
    },
  },
];

// Get locations by city name
export function getLocationsForCity(city: string): LocationPreset[] {
  const cityLower = city.toLowerCase();
  if (cityLower.includes('kochi')) return kochiLocations;
  if (cityLower.includes('chennai')) return chennaiLocations;
  if (cityLower.includes('kolkata')) return kolkataLocations;
  if (cityLower.includes('bangalore') || cityLower.includes('bengaluru')) return bangaloreLocations;
  if (cityLower.includes('delhi')) return delhiLocations;
  if (cityLower.includes('helsinki')) return helsinkiLocations;
  return kochiLocations; // default
}

// Get all locations
export const allLocations = [...kochiLocations, ...kolkataLocations, ...chennaiLocations, ...bangaloreLocations, ...delhiLocations, ...helsinkiLocations];

