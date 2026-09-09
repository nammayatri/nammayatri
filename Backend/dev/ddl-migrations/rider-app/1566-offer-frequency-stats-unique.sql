ALTER TABLE atlas_app.person_offer_frequency_stats
  ADD CONSTRAINT unique_person_offer_frequency_stats_entity_offer
  UNIQUE (entity_id, entity_type, offer_id);

ALTER TABLE atlas_app.offer_frequency_stats_history
  ADD CONSTRAINT unique_offer_frequency_stats_history_entity_offer_window
  UNIQUE (entity_id, entity_type, offer_id, frequency_type, period_start);
