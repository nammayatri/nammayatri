import { FlowGroup } from '../types';
import { stripePaymentFlow } from './stripePayment';

// Register all flow groups here. Add new ones as you build them.
export const ALL_FLOWS: FlowGroup[] = [
  stripePaymentFlow,
  // Future:
  // dynamicOfferFlow,
  // staticOfferFlow,
  // frfsPaymentFlow,
];
