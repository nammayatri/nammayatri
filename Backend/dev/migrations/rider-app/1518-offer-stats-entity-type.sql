-- Add entity_type column to person_offer_stats for multi-entity offer tracking
-- Default 'Person' for backward compatibility with existing rows

ALTER TABLE atlas_app.person_offer_stats ADD COLUMN IF NOT EXISTS entity_type text NOT NULL DEFAULT 'Person';

-- Drop old unique constraint (offer_id, person_id)
ALTER TABLE atlas_app.person_offer_stats DROP CONSTRAINT IF EXISTS unique_person_offer_stats_offer_person;

-- Add new unique constraint: (offer_id, person_id, entity_type)
ALTER TABLE atlas_app.person_offer_stats ADD CONSTRAINT unique_offer_stats_offer_entity
  UNIQUE (offer_id, person_id, entity_type);

-- Index for querying by person_id + entity_type (findAllByEntityIdAndEntityType)
CREATE INDEX IF NOT EXISTS idx_offer_stats_person_entity_type
  ON atlas_app.person_offer_stats (person_id, entity_type);
