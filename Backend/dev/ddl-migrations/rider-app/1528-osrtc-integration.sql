-- OSRTC Integration: Add journeyDate to frfs_search
ALTER TABLE atlas_app.frfs_search ADD COLUMN IF NOT EXISTS journey_date timestamp with time zone;

ALTER TABLE atlas_app.frfs_quote ADD COLUMN IF NOT EXISTS osrtc_trip_detail json;
