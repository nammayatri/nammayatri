-- Outcome of the last after-the-fact split payout pushed to the payment gateway for a
-- transaction (CCAvenue createSplitPayout, driven by the FRFSCCAvenueSplitPayout cron).
--
-- Distinct from split_settlement_response, which is what the gateway told us about a split
-- requested at order-creation time. These record a split we sent after capture.
--
-- split_payout_success is the queryable outcome and the cron's idempotency flag: a row
-- where it is true is never sent again. split_payout_response keeps the full gateway reply
-- for diagnosis. Transactions still owed a split are:
--   select * from atlas_app.payment_transaction where split_payout_success is not true;
ALTER TABLE atlas_app.payment_transaction ADD COLUMN split_payout_response json;
ALTER TABLE atlas_app.payment_transaction ADD COLUMN split_payout_success boolean;
