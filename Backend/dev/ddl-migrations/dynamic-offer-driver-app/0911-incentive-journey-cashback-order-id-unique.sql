-- One payout row per incentive-journey cash order id.
-- createPayoutService checks then inserts. Two callers can both pass that check if the
-- redis lock expires before the first insert. order_id is not part of a unique key
-- (primary key is (id, order_id)), so both inserts would call the partner.
-- This index makes the second insert fail before the partner call. The journey payout
-- then finds the existing row and does not pay again.
-- Scoped to INCENTIVE_JOURNEY_CASHBACK so coin, referral, and other payouts are unchanged.
CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS payout_order_incentive_journey_cashback_order_id_uidx
  ON atlas_driver_offer_bpp.payout_order (order_id)
  WHERE entity_name = 'INCENTIVE_JOURNEY_CASHBACK';
