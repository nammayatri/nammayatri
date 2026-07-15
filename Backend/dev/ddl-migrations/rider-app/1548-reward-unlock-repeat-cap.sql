DROP INDEX IF EXISTS atlas_app.idx_reward_unlock_active_unique;
CREATE UNIQUE INDEX IF NOT EXISTS idx_reward_unlock_active_unique
  ON atlas_app.reward_unlock (person_id, campaign_id, cohort_id, unlock_seq)
  WHERE status <> 'Reclaimed';
