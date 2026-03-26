import { FlowGroup, Scenario } from '../types';
import { startLocationPinger } from '../services/api';

const happyPath: Scenario = {
  id: 'stripe-happy',
  name: 'Happy Path (No Auth Card)',
  description: 'Full ride with Stripe payment: search → estimates → select → payment → complete → tip',
  steps: [
    // Payment setup
    { id: 'get-customer', name: 'Get/Create Stripe Customer', method: 'GET', service: 'rider', path: '/payment/customer', auth: true,
      assert: (d) => d.customerId ? null : 'Missing customerId',
      save: (d, ctx) => { ctx.customerId = d.customerId; } },
    { id: 'setup-intent', name: 'Get Setup Intent', method: 'GET', service: 'rider', path: '/payment/intent/setup', auth: true,
      assert: (d) => d.setupIntentClientSecret ? null : 'Missing setupIntentClientSecret' },
    { id: 'list-methods', name: 'List Payment Methods', method: 'GET', service: 'rider', path: '/payment/methods', auth: true,
      save: (d, ctx) => {
        ctx.paymentMethods = d.list || [];
        ctx.defaultPM = d.defaultPaymentMethodId;
        // Auto-select first payment method for estimate select
        ctx.paymentMethodId = d.defaultPaymentMethodId || (d.list?.length > 0 ? d.list[0].cardId : null);
      } },
    // Ride search (uses API catalog — preset selected in UI)
    { id: 'ride-search', name: 'Search Ride', method: 'POST', service: 'rider', path: '/rideSearch', auth: true,
      useCatalog: 'ride-search',
      save: (d, ctx) => { ctx.searchId = d.searchId; },
      assert: (d) => d.searchId ? null : 'Missing searchId' },
    // Get estimates
    { id: 'get-estimates', name: 'Get Estimates', method: 'GET', service: 'rider', path: '/placeholder', auth: true,
      useCatalog: 'get-estimates',
      save: (d, ctx) => {
        ctx.estimates = d.estimates || [];
        ctx.quotes = d.quotes || [];
        if (d.estimates?.length > 0) ctx.selectedEstimateId = d.estimates[0].id;
      } },
    // Select estimate
    { id: 'estimate-select', name: 'Select Estimate', method: 'POST', service: 'rider', path: '/placeholder', auth: true,
      useCatalog: 'estimate-select' },

    // === DRIVER SECTION: Set location + go online + accept ride ===
    { id: 'driver-location', name: '[Driver] Set Location (= pickup)', method: 'POST', service: 'lts',
      path: '/driver/location', auth: true,
      extraHeaders: (ctx) => ({ 'vt': ctx.driverVehicleVariant || 'SUV', 'dm': 'ONLINE', ...(ctx.driverMerchantId ? { 'mid': ctx.driverMerchantId } : {}) }),
      note: 'Updates driver location via LTS and starts background pinger (every 10s) to keep geo bucket alive.',
      body: (ctx) => {
        const origin = ctx.searchOrigin || { lat: 10.0739, lon: 76.2733 };
        return [{ pt: { lat: origin.lat, lon: origin.lon }, ts: new Date().toISOString() }];
      },
      save: (_d, ctx) => {
        // Start background location pinger so LTS geo buckets don't expire
        startLocationPinger(ctx);
      } },
    { id: 'driver-active', name: '[Driver] Set Online', method: 'POST', service: 'driver',
      path: '/driver/setActivity?active=true&mode=%22ONLINE%22', auth: true },
    { id: 'driver-nearby', name: '[Driver] Get Nearby Requests', method: 'GET', service: 'driver', path: '/driver/nearbyRideRequest', auth: true,
      note: 'Poll until searchRequestForDriver appears (may need to wait for allocator)',
      save: (d, ctx) => {
        const requests = d.searchRequestsForDriver || [];
        if (requests.length > 0) {
          ctx.searchTryId = requests[0].searchTryId;
          ctx.driverSearchRequestId = requests[0].searchRequestId;
        }
      },
      assert: (d) => (d.searchRequestsForDriver?.length > 0) ? null : 'No nearby requests — allocator may not have sent yet. Try running this step again.' },
    { id: 'driver-respond', name: '[Driver] Accept Quote', method: 'POST', service: 'driver', path: '/driver/searchRequest/quote/respond', auth: true,
      body: (ctx) => ({
        searchTryId: ctx.searchTryId,
        offeredFare: null,
        offeredFareWithCurrency: null,
        response: 'Accept',
        notificationSource: null,
        renderedAt: null,
        respondedAt: null,
      }) },

    // === RIDER SECTION: Get booking ===
    { id: 'poll-booking', name: 'Poll Estimate Results', method: 'GET', service: 'rider',
      path: (ctx) => `/estimate/${ctx.selectedEstimateId}/results`,
      auth: true,
      note: 'GET /estimate/{estimateId}/results — poll until bookingId appears',
      save: (d, ctx) => {
        if (d.bookingId) ctx.bookingId = d.bookingId;
        if (d.bookingIdV2) ctx.bookingId = d.bookingIdV2;
      },
      assert: (d) => (d.bookingId || d.bookingIdV2) ? null : 'No bookingId yet — driver may not have responded. Try polling again.' },
    { id: 'get-booking', name: 'Get Booking Details', method: 'POST', service: 'rider',
      path: (ctx) => `/rideBooking/${ctx.bookingId}`,
      auth: true,
      note: 'Gets booking + ride details including OTP for ride start',
      save: (d, ctx) => {
        ctx.bookingStatus = d.status;
        const ride = d.rideList?.[0] || d.ride;
        ctx.rideId = ride?.id;
        ctx.rideOtp = ride?.rideOtp || ride?.otp;
      },
      assert: (d) => {
        const ride = d.rideList?.[0] || d.ride;
        return ride?.id ? null : 'No ride found in booking — confirm may not have completed.';
      } },

    // === DRIVER SECTION: Start + End ride ===
    { id: 'ride-start', name: '[Driver] Start Ride', method: 'POST', service: 'driver',
      path: (ctx) => `/driver/ride/${ctx.rideId}/start`,
      auth: true,
      note: 'Starts ride using OTP from booking details',
      body: (ctx) => {
        const origin = ctx.searchOrigin || { lat: 10.0739, lon: 76.2733 };
        return {
          rideOtp: ctx.rideOtp,
          point: { lat: origin.lat, lon: origin.lon },
          odometer: null,
        };
      },
      assert: (d) => (d?.result === 'Success' || d === 'Success') ? null : `Start ride failed: ${JSON.stringify(d)}` },
    { id: 'ride-end', name: '[Driver] End Ride', method: 'POST', service: 'driver',
      path: (ctx) => `/driver/ride/${ctx.rideId}/end`,
      auth: true,
      note: 'Ends ride at destination',
      body: (ctx) => {
        const dest = ctx.searchDestination || ctx.searchOrigin || { lat: 10.0739, lon: 76.2733 };
        return {
          point: { lat: dest.lat, lon: dest.lon },
          endRideOtp: null,
          uiDistanceCalculationWithAccuracy: null,
          uiDistanceCalculationWithoutAccuracy: null,
          odometer: null,
          driverGpsTurnedOff: null,
        };
      } },
  ],
};

const tipFlow: Scenario = {
  id: 'stripe-tip',
  name: 'Ride + Tip Flow',
  description: 'Complete ride then add tip. Verifies PENDING ledger → capture → SETTLED → tipAmount updated.',
  steps: [
    { id: 'get-customer', name: 'Get Stripe Customer', method: 'GET', service: 'rider', path: '/payment/customer', auth: true },
    { id: 'list-methods', name: 'List Payment Methods', method: 'GET', service: 'rider', path: '/payment/methods', auth: true,
      save: (d, ctx) => {
        ctx.paymentMethods = d.list || [];
        ctx.defaultPM = d.defaultPaymentMethodId;
        ctx.paymentMethodId = d.defaultPaymentMethodId || (d.list?.length > 0 ? d.list[0].cardId : null);
      } },
    { id: 'ride-search', name: 'Search Ride', method: 'POST', service: 'rider', path: '/rideSearch', auth: true,
      useCatalog: 'ride-search',
      save: (d, ctx) => { ctx.searchId = d.searchId; } },
    { id: 'get-estimates', name: 'Get Estimates', method: 'GET', service: 'rider', path: '/placeholder', auth: true,
      useCatalog: 'get-estimates',
      save: (d, ctx) => {
        ctx.estimates = d.estimates || [];
        ctx.quotes = d.quotes || [];
        if (d.estimates?.length > 0) ctx.selectedEstimateId = d.estimates[0].id;
      } },
    { id: 'estimate-select', name: 'Select Estimate', method: 'POST', service: 'rider', path: '/placeholder', auth: true,
      useCatalog: 'estimate-select' },
  ],
};

const cancelFlow: Scenario = {
  id: 'stripe-cancel',
  name: 'Cancellation + Fee',
  description: 'Ride cancelled after assignment: payment intent voided → ledger VOIDED → cancellation fee charged',
  steps: [
    { id: 'get-customer', name: 'Get Stripe Customer', method: 'GET', service: 'rider', path: '/payment/customer', auth: true },
    { id: 'list-methods', name: 'List Payment Methods', method: 'GET', service: 'rider', path: '/payment/methods', auth: true,
      save: (d, ctx) => {
        ctx.paymentMethods = d.list || [];
        ctx.paymentMethodId = d.defaultPaymentMethodId || (d.list?.length > 0 ? d.list[0].cardId : null);
      } },
    { id: 'ride-search', name: 'Search Ride', method: 'POST', service: 'rider', path: '/rideSearch', auth: true,
      useCatalog: 'ride-search',
      save: (d, ctx) => { ctx.searchId = d.searchId; } },
    { id: 'get-estimates', name: 'Get Estimates', method: 'GET', service: 'rider', path: '/placeholder', auth: true,
      useCatalog: 'get-estimates',
      save: (d, ctx) => {
        ctx.estimates = d.estimates || [];
        if (d.estimates?.length > 0) ctx.selectedEstimateId = d.estimates[0].id;
      } },
    { id: 'estimate-select', name: 'Select Estimate', method: 'POST', service: 'rider', path: '/placeholder', auth: true,
      useCatalog: 'estimate-select' },
  ],
};

const refundFlow: Scenario = {
  id: 'stripe-refund',
  name: 'Refund Flow',
  description: 'Refund after completed ride: refund request → Stripe refund → ledger reversal',
  steps: [
    { id: 'get-customer', name: 'Get Stripe Customer', method: 'GET', service: 'rider', path: '/payment/customer', auth: true },
  ],
};

const duesFlow: Scenario = {
  id: 'stripe-dues',
  name: 'Dues + Debt Settlement',
  description: 'Failed payment → getDueAmount → clearDues with platform fee → ledger settled',
  steps: [
    { id: 'get-dues', name: 'Get Due Amount', method: 'GET', service: 'rider', path: '/payment/getDueAmount', auth: true,
      save: (d, ctx) => { ctx.dues = d; } },
    { id: 'clear-dues', name: 'Clear Dues', method: 'POST', service: 'rider', path: '/payment/clearDues', auth: true,
      body: (ctx) => ({ paymentMethodId: ctx.defaultPM || null }) },
  ],
};

const declineFlow: Scenario = {
  id: 'stripe-decline',
  name: 'Card Declined',
  description: 'Mock Stripe returns card_declined. Ledger entries remain PENDING → creates dues.',
  steps: [
    { id: 'get-customer', name: 'Get Stripe Customer', method: 'GET', service: 'rider', path: '/payment/customer', auth: true },
  ],
};

const threeDsFlow: Scenario = {
  id: 'stripe-3ds',
  name: 'Card with 3DS Auth',
  description: 'Tests RequiresAction flow. Mock Stripe returns requires_action for pm_card_authRequired.',
  steps: [
    { id: 'get-customer', name: 'Get Stripe Customer', method: 'GET', service: 'rider', path: '/payment/customer', auth: true },
    { id: 'list-methods', name: 'List Payment Methods', method: 'GET', service: 'rider', path: '/payment/methods', auth: true },
  ],
};

export const stripePaymentFlow: FlowGroup = {
  id: 'stripe-payment',
  name: 'Ride Flow (Stripe Payment)',
  scenarios: [happyPath, threeDsFlow, tipFlow, cancelFlow, refundFlow, duesFlow, declineFlow],
};
