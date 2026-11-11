-- Lets a RewardCohort's unlock cap reset every day or every 7-day block,
-- both counted from the campaign's start (Daily | Weekly). NULL keeps
-- today's behaviour: the cap (max_unlocks_per_cohort, 1 when NULL) covers the
-- whole campaign. Per-block unlocks still get a fresh unlock_seq each time, so
-- the existing partial unique index on
-- (person_id, campaign_id, cohort_id, unlock_seq) needs no change.

ALTER TABLE atlas_app.reward_cohort ADD COLUMN IF NOT EXISTS unlock_reset_period text NULL;
