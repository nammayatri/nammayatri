-- Rewards dashboard integration tests: enable flag for NY/BT.
-- Idempotent — safe to re-run before ./run-tests.sh rewards

-- Enable rewards management for Namma Yatri + Bharat Taxi (all operating cities).
UPDATE atlas_app.rider_config rc
SET enable_rewards_management = true
FROM atlas_app.merchant_operating_city moc
JOIN atlas_app.merchant m ON m.id = moc.merchant_id
WHERE rc.merchant_operating_city_id = moc.id
  AND m.short_id IN ('NAMMA_YATRI', 'BHARAT_TAXI');
