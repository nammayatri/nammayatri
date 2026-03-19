-- Performance indices for critical-path tables in atlas_app schema.
-- Identified via cross-referencing 1,165+ YAML-declared queries against 70 existing indices.
-- All use CONCURRENTLY for zero-downtime deployment (must NOT run inside a transaction block).

-- P0: Ride confirmation callback from BPP uses findByBPPBookingId.
-- Without this index, every on_confirm/on_update callback does a full table scan on booking.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_booking_bpp_ride_booking_id
  ON atlas_app.booking USING btree (bpp_ride_booking_id);

-- P0: Booking creation from quote uses findByQuoteId.
-- Called on every booking confirm — sequential scan on high-volume table.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_booking_quote_id
  ON atlas_app.booking USING btree (quote_id);

-- P1: Ride sharing/support lookup uses findRideByRideShortId.
-- Customer-facing short URL and support ticket lookups scan entire ride table.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_ride_short_id
  ON atlas_app.ride USING btree (short_id);

-- P1: Composite upgrade — findOneByBookingId orders by created_at DESC.
-- Existing idx_ride_booking_id covers equality but not the sort; PG must sort after filter.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_ride_booking_id_created
  ON atlas_app.ride USING btree (booking_id, created_at DESC);

-- P1: Composite upgrade — findBySRIdAndStatus filters on (request_id, status).
-- Existing idx_estimate_request_id covers request_id only; PG must filter status post-scan.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_estimate_request_id_status
  ON atlas_app.estimate USING btree (request_id, status);
