/**
 * Mirrors Lib.Finance.Domain.Types.ReconciliationType / ReconciliationEntry.yaml
 * enum: "DSR_VS_LEDGER, DSR_VS_SUBSCRIPTION, DSSR_VS_SUBSCRIPTION,
 *        PG_PAYMENT_SETTLEMENT_VS_SUBSCRIPTION, PG_PAYOUT_SETTLEMENT_VS_PAYOUT_REQUEST"
 * Keep in sync when finance-kernel adds or renames reconciliation types.
 */

export const RECONCILIATION_TYPE_VALUES = [
  'DSR_VS_LEDGER',
  'DSR_VS_SUBSCRIPTION',
  'DSSR_VS_SUBSCRIPTION',
  'PG_PAYMENT_SETTLEMENT_VS_SUBSCRIPTION',
  'PG_PAYOUT_SETTLEMENT_VS_PAYOUT_REQUEST',
] as const;

export type ReconciliationType = (typeof RECONCILIATION_TYPE_VALUES)[number];

export interface ReconciliationTypeOption {
  value: ReconciliationType;
  label: string;
  /** Matches context-api build_readiness() check names and intent */
  prerequisites: string[];
}

export const RECONCILIATION_TYPE_OPTIONS: ReconciliationTypeOption[] = [
  {
    value: 'DSR_VS_LEDGER',
    label: 'DSR vs Ledger',
    prerequisites: [
      'Completed bookings in the recon window (booking.status = COMPLETED, updated_at in range)',
      'Cancelled bookings in the same window (booking.status = CANCELLED)',
      'finance_ledger_entry rows for the city in the same timestamp window',
    ],
  },
  {
    value: 'DSSR_VS_SUBSCRIPTION',
    label: 'DSSR vs Subscription',
    prerequisites: [
      'ACTIVE subscription_purchase rows with purchase_timestamp in the window',
      'finance_ledger_entry with reference_type = SubscriptionPurchase in the window',
    ],
  },
  {
    value: 'DSR_VS_SUBSCRIPTION',
    label: 'DSR vs Subscription',
    prerequisites: [
      'subscription_purchase in ACTIVE, EXPIRED, or EXHAUSTED with purchase_timestamp on or before recon end',
      'Completed bookings in the window (booking.status = COMPLETED)',
    ],
  },
  {
    value: 'PG_PAYMENT_SETTLEMENT_VS_SUBSCRIPTION',
    label: 'PG Payment vs Subscription',
    prerequisites: [
      'pg_payment_settlement_report rows with txn_status = SUCCESS and txn_date in the window',
      'subscription_purchase rows referenced by those settlement rows (reference_id)',
      'For dashboard seed step: a subscription_purchase id to POST /api/finance/seed-payment-settlement',
    ],
  },
  {
    value: 'PG_PAYOUT_SETTLEMENT_VS_PAYOUT_REQUEST',
    label: 'PG Payout vs Payout Request',
    prerequisites: [
      'pg_payout_settlement_report rows with txn_status = SUCCESS and txn_date in the window',
      'payout_request rows with status = CREDITED for the merchant operating city (readiness counts city-wide)',
      'Payout settlement seeding via context-api is not implemented yet (501) — use mock SQL or real flows',
    ],
  },
];

export const DEFAULT_RECONCILIATION_TYPE: ReconciliationType = 'DSR_VS_LEDGER';

export function isReconciliationType(v: string): v is ReconciliationType {
  return (RECONCILIATION_TYPE_VALUES as readonly string[]).includes(v);
}

export function getReconciliationTypeOption(
  value: string
): ReconciliationTypeOption | undefined {
  return RECONCILIATION_TYPE_OPTIONS.find(o => o.value === value);
}
