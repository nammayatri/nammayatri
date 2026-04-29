-- Add new unique constraint: (offer_id, entity_id, entity_type)
ALTER TABLE atlas_app.person_offer_stats ADD CONSTRAINT unique_offer_stats_offer_entity
  UNIQUE (offer_id, person_id, entity_type);

-- Index for querying by entity_id + entity_type (findAllByEntityIdAndEntityType)
CREATE INDEX IF NOT EXISTS idx_offer_stats_person_entity_type
  ON atlas_app.person_offer_stats (person_id, entity_type);
