/**
 * API Catalog — Central registry of all testable APIs with their mock data.
 *
 * Usage:
 *   const catalog = buildApiCatalog('Kochi');
 *   const searchApi = catalog['ride-search'];
 *   const presets = searchApi.mockDataPresets;
 */

import { ApiDef } from './types';
import { buildRideSearchApi } from './rideSearch';
import { buildGetEstimatesApi } from './getEstimates';
import { buildEstimateSelectApi } from './estimateSelect';

export type ApiCatalog = Record<string, ApiDef>;

export function buildApiCatalog(city: string): ApiCatalog {
  return {
    'ride-search': buildRideSearchApi(city),
    'get-estimates': buildGetEstimatesApi(),
    'estimate-select': buildEstimateSelectApi(),
    // Future APIs:
    // 'confirm': buildConfirmApi(),
    // 'get-booking-status': buildGetBookingStatusApi(),
    // 'add-tip': buildAddTipApi(),
    // 'cancel-ride': buildCancelRideApi(),
    // 'get-dues': buildGetDuesApi(),
    // 'clear-dues': buildClearDuesApi(),
  };
}

// Re-export types
export type { ApiDef, MockDataPreset, LocationPreset } from './types';
