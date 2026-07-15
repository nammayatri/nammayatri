-- Enforces sticky-unlock invariant: at most one non-reclaimed row
-- per (personId, campaignId, cohortId). Reclaimed rows are excluded
-- so the same triple can be re-unlocked after a reclaim.

CREATE UNIQUE INDEX IF NOT EXISTS idx_reward_unlock_active_unique
  ON atlas_app.reward_unlock (person_id, campaign_id, cohort_id)
  WHERE status <> 'Reclaimed';
