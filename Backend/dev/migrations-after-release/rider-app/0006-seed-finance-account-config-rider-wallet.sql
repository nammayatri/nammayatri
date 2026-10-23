-- Finance-kernel unification, rider side: enable kernel transfers for the rider
-- wallet class (Liability, RIDER). One merchant-wide ('*' city) row per merchant;
-- without it the class defaults are transfer_out_enabled = false and every cashback /
-- referral / metro payout falls back to the legacy (unledgered-hold) path.
--
-- allow_negative_balance stays TRUE (kernel default behaviour) until the deposit
-- flows are audited for a strict class.

INSERT INTO atlas_app.finance_account_config
  (id, merchant_id, merchant_operating_city_id, account_type, counterparty_type, sub_ledger,
   allow_negative_balance, min_retained_balance, redeemable_cutoff,
   transfer_out_enabled, instant_transfer_out_enabled, min_transfer_out_amount,
   max_transfer_out_per_day, transfer_out_fee, transfer_in_enabled, auto_transfer_out,
   created_at, updated_at)
SELECT
  md5(random()::text || clock_timestamp()::text || m.id),
  m.id,
  '*',
  'Liability',
  'RIDER',
  NULL,
  true,
  0,
  NULL, -- cashback has no redeemable cutoff: everything unsettled is payable
  true,
  true,
  0,
  NULL,
  NULL,
  true, -- booking-deposit topups are inbound transfers
  NULL,
  now(),
  now()
FROM atlas_app.merchant m
WHERE NOT EXISTS (
  SELECT 1
  FROM atlas_app.finance_account_config fac
  WHERE fac.merchant_id = m.id
    AND fac.account_type = 'Liability'
    AND fac.counterparty_type = 'RIDER'
    AND fac.sub_ledger IS NULL
);
