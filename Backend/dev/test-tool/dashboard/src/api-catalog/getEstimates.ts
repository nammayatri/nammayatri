/**
 * API: Get Quotes / Estimates
 * GET /rideSearch/{searchId}/results
 *
 * Polls for available ride estimates after a search.
 * Returns quotes (direct offers) and estimates (price ranges).
 */

import { ApiDef } from './types';

export function buildGetEstimatesApi(): ApiDef {
  return {
    id: 'get-estimates',
    name: 'Get Estimates / Quotes',
    description: 'Get available ride estimates and quotes for a search. May need polling.',
    method: 'GET',
    service: 'rider',
    path: (ctx) => `/rideSearch/${ctx.searchId}/results`,
    auth: true,
    extractFromResponse: (data, ctx) => {
      ctx.estimates = data.estimates || [];
      ctx.quotes = data.quotes || [];
      // Pick first estimate for select
      if (data.estimates?.length > 0) {
        ctx.selectedEstimateId = data.estimates[0].id;
        ctx.selectedEstimateName = data.estimates[0].vehicleServiceTierType || data.estimates[0].vehicleVariant;
      }
      // Pick first quote for confirm
      if (data.quotes?.length > 0) {
        const firstQuote = data.quotes[0];
        ctx.selectedQuoteId = firstQuote.contents?.id || firstQuote.id;
      }
    },
    assert: (data) => {
      if (!data.estimates && !data.quotes) return 'No estimates or quotes returned';
      if ((data.estimates?.length || 0) === 0 && (data.quotes?.length || 0) === 0)
        return 'Empty estimates and quotes — driver may not be available. Try again.';
      return null;
    },
    mockDataPresets: [
      {
        id: 'default',
        name: 'Default (wait for results)',
        description: 'Just fetch results for the current searchId',
        data: {},
      },
    ],
  };
}
