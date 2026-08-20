CREATE INDEX CONCURRENTLY IF NOT EXISTS frfs_ticket_booking_idx_override_applied_entity_id
    ON atlas_app.frfs_ticket_booking USING btree (override_applied_entity_id)
    WHERE override_applied_entity_id IS NOT NULL;
