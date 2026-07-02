-- Allows a RewardCohort to be marked repeatable (maxUnlocksPerCohort) and
-- introduces unlockSeq as the identity discriminator so a rider can hold
-- multiple RewardUnlock rows for the same (personId, campaignId, cohortId)
-- triple. Non-repeatable cohorts keep unlockSeq = 1, preserving the
-- one-shot sticky behavior enforced by the partial unique index below.
--
-- unlock_seq is nullable (not NOT NULL) per NammaDSL's backward-compatibility
-- rule for newly-added columns: a brand new column cannot be NOT NULL, since
-- it isn't present in the migrations-read-only baseline the generator tracks.
--
-- DEFAULT 1 (not just nullable) closes a rolling-deploy race: the new unique
-- index below includes unlock_seq, and Postgres never treats two NULLs as
-- equal in a unique index. Without a column-level default, an old-code pod
-- (pre-this-migration binary, still running during the rollout) issuing an
-- INSERT that omits unlock_seq entirely would land NULL, and two such pods
-- racing to unlock the same non-repeatable cohort for the same rider could
-- both succeed — the exact duplicate-unlock race the old index prevented.
-- With DEFAULT 1, Postgres itself supplies 1 for any insert that omits the
-- column (old-code pods included), so the race is still caught by the index.
-- New application code always sets unlock_seq explicitly on every insert
-- ('fromMaybe 1 . unlockSeq' when reading existing rows still handles NULL
-- from rows written before this migration), so the default only matters for
-- old-code compatibility during rollout, never for new-code behavior.

ALTER TABLE atlas_app.reward_cohort ADD COLUMN max_unlocks_per_cohort integer NULL;
ALTER TABLE atlas_app.reward_unlock ADD COLUMN unlock_seq integer DEFAULT 1;

DROP INDEX IF EXISTS atlas_app.idx_reward_unlock_active_unique;
CREATE UNIQUE INDEX IF NOT EXISTS idx_reward_unlock_active_unique
  ON atlas_app.reward_unlock (person_id, campaign_id, cohort_id, unlock_seq)
  WHERE status <> 'Reclaimed';
