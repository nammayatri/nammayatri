/**
 * API: Ride Search
 * POST /rideSearch
 *
 * Initiates a ride search with pickup + drop locations.
 * Returns searchId used for subsequent estimate/select/confirm calls.
 */

import { ApiDef, MockDataPreset, LocationPreset } from './types';
import { getLocationsForCity } from '../mock-data/locations';

// Build SearchReqLocation from LocationPreset
function mkSearchLocation(loc: LocationPreset) {
  return {
    gps: loc.gps,
    address: loc.address,
  };
}

// Mock data presets for OneWay search
function oneWayPresets(city: string): MockDataPreset[] {
  const locs = getLocationsForCity(city);
  if (locs.length < 2) return [];

  const presets: MockDataPreset[] = [];

  // Generate presets from location pairs
  for (let i = 0; i < Math.min(locs.length - 1, 4); i++) {
    const from = locs[i];
    const to = locs[i + 1];
    presets.push({
      id: `oneway-${city}-${i}`,
      name: `${from.name} → ${to.name}`,
      description: `OneWay ride in ${city}`,
      data: { origin: from, destination: to, type: 'OneWaySearch' },
    });
  }

  // Long distance preset (first → last)
  if (locs.length > 2) {
    presets.push({
      id: `oneway-${city}-long`,
      name: `${locs[0].name} → ${locs[locs.length - 1].name} (Long)`,
      description: `Long distance ride in ${city}`,
      data: { origin: locs[0], destination: locs[locs.length - 1], type: 'OneWaySearch' },
    });
  }

  return presets;
}

// Rental search presets
function rentalPresets(city: string): MockDataPreset[] {
  const locs = getLocationsForCity(city);
  if (locs.length < 1) return [];

  return [
    {
      id: `rental-${city}-2hr`,
      name: `2hr Rental from ${locs[0].name}`,
      description: `2 hour rental starting from ${locs[0].name}`,
      data: {
        origin: locs[0],
        type: 'RentalSearch',
        estimatedRentalDistance: 20000, // 20km in meters
        estimatedRentalDuration: 7200, // 2hrs in seconds
      },
    },
    {
      id: `rental-${city}-4hr`,
      name: `4hr Rental from ${locs[0].name}`,
      description: `4 hour rental starting from ${locs[0].name}`,
      data: {
        origin: locs[0],
        type: 'RentalSearch',
        estimatedRentalDistance: 40000,
        estimatedRentalDuration: 14400,
      },
    },
  ];
}

export function buildRideSearchApi(city: string): ApiDef {
  return {
    id: 'ride-search',
    name: 'Ride Search',
    description: 'Search for available rides. Returns searchId for getting quotes.',
    method: 'POST',
    service: 'rider',
    path: '/rideSearch',
    auth: true,
    bodyBuilder: (preset, ctx) => {
      const d = preset.data;
      // Save origin/destination GPS for driver location + ride end steps
      if (d.origin?.gps) ctx.searchOrigin = d.origin.gps;
      if (d.destination?.gps) ctx.searchDestination = d.destination.gps;
      const now = new Date(Date.now() + 5 * 60000).toISOString(); // 5 min from now

      if (d.type === 'OneWaySearch') {
        return {
          fareProductType: 'ONE_WAY',
          contents: {
            origin: mkSearchLocation(d.origin),
            destination: mkSearchLocation(d.destination),
            isSourceManuallyMoved: false,
            isDestinationManuallyMoved: false,
            isSpecialLocation: false,
            quotesUnifiedFlow: true,
            fareParametersInRateCard: false,
          },
        };
      }

      if (d.type === 'RentalSearch') {
        return {
          fareProductType: 'RENTAL',
          contents: {
            origin: mkSearchLocation(d.origin),
            isSourceManuallyMoved: false,
            isSpecialLocation: false,
            startTime: now,
            estimatedRentalDistance: d.estimatedRentalDistance,
            estimatedRentalDuration: d.estimatedRentalDuration,
            quotesUnifiedFlow: true,
            fareParametersInRateCard: false,
          },
        };
      }

      // Default to OneWay
      return {
        fareProductType: 'ONE_WAY',
        contents: {
          origin: mkSearchLocation(d.origin),
          destination: mkSearchLocation(d.destination || d.origin),
        },
      };
    },
    extractFromResponse: (data, ctx) => {
      ctx.searchId = data.searchId;
      ctx.searchExpiry = data.searchExpiry;
    },
    assert: (data) => data.searchId ? null : 'Missing searchId in response',
    mockDataPresets: [...oneWayPresets(city), ...rentalPresets(city)],
  };
}
