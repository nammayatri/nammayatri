-- Add bounding box column to geometry table for provider platform
-- Phase 2.1: geometry bbox support for state entry permit / toll pre-filtering
--
-- Format: JSON array of lat-long points where consecutive segments denote a side.
-- For a rectangle: 4 points in order, e.g. [topLeft, topRight, bottomRight, bottomLeft],
-- each point as {"lat": <number>, "lon": <number>}. Scalable to more sides for polygons.
-- Aligns with Kernel.Utils.ComputeIntersection.BoundingBox (topLeft, topRight, bottomLeft, bottomRight).
--
-- Backfill: must be done separately (e.g. from geom via ST_Envelope, then convert
-- envelope corners to the JSON array format above).
-- Column type: jsonb (validated JSON, queryable; same pattern as docStatus in digilocker_verification).

ALTER TABLE atlas_driver_offer_bpp.geometry
ADD COLUMN IF NOT EXISTS bbox jsonb;
