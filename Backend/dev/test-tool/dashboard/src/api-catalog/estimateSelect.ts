/**
 * API: Estimate Select
 * POST /estimate/{estimateId}/select
 *
 * Selects an estimate to proceed with booking.
 * Returns selectTtl (time to wait for driver quotes).
 */

import { ApiDef } from './types';

export function buildEstimateSelectApi(): ApiDef {
  return {
    id: 'estimate-select',
    name: 'Select Estimate',
    description: 'Select an estimate to get driver offers. Uses estimateId from previous step.',
    method: 'POST',
    service: 'rider',
    path: (ctx) => `/estimate/${ctx.selectedEstimateId}/select2`,
    auth: true,
    bodyBuilder: (preset, ctx) => {
      // PaymentInstrument uses tagged-object encoding: {instrumentType, instrumentName}
      const instrument = preset.data.paymentInstrument || (ctx.paymentMethodId ? { instrumentType: 'Card', instrumentName: 'DefaultCardType' } : null);
      return {
        autoAssignEnabled: true,
        autoAssignEnabledV2: true,
        customerExtraFee: null,
        customerExtraFeeWithCurrency: null,
        otherSelectedEstimates: null,
        isAdvancedBookingEnabled: null,
        paymentMethodId: ctx.paymentMethodId || null,
        paymentInstrument: instrument,
        disabilityDisable: null,
        isPetRide: false,
      };
    },
    extractFromResponse: (data, ctx) => {
      ctx.selectTtl = data.selectTtl || data.result;
    },
    assert: (_data) => null, // select usually returns 200 with ttl
    mockDataPresets: [
      {
        id: 'with-card-payment',
        name: 'Card Payment (default)',
        description: 'Auto-assign with Stripe card payment',
        data: {
          paymentInstrument: { instrumentType: 'Card', instrumentName: 'DefaultCardType' },
        },
      },
      {
        id: 'cash-payment',
        name: 'Cash Payment',
        description: 'Auto-assign with cash payment',
        data: {
          paymentInstrument: { instrumentType: 'Cash' },
        },
      },
    ],
  };
}
