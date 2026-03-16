-- Performance indices for critical-path tables in atlas_driver_offer_bpp schema.
-- Identified via cross-referencing 1,165+ YAML-declared queries against 49 existing indices.
-- All use CONCURRENTLY for zero-downtime deployment (must NOT run inside a transaction block).

-- P0: Driver-side booking lookup uses findByQuoteId / findBySTId on quote_id.
-- Every booking confirmation from rider side triggers this lookup — full table scan without index.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_booking_quote_id
  ON atlas_driver_offer_bpp.booking USING btree (quote_id);

-- P0: Driver fee lookup on every ride end — findPendingFeesByDriverIdAndServiceName
-- and findLatestFeeByDriverIdAndServiceName. Currently only BRIN on time cols + btree on status.
-- No composite covering driver_id, the primary access pattern.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_driver_fee_driver_id_service_status
  ON atlas_driver_offer_bpp.driver_fee USING btree (driver_id, service_name, status);

-- P1: Real-time OTP verification at special zone pickups — findBookingBySpecialZoneOTP.
-- Partial index on active bookings only to keep the index small and fast.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_booking_special_zone_otp
  ON atlas_driver_offer_bpp.booking USING btree (special_zone_otp_code, status)
  WHERE status = 'NEW';

-- P1: Driver allocation — findAllBySTId filters on (search_try_id, status).
-- Existing idx_driver_quote_s_try_id covers search_try_id only; PG must filter status post-scan.
-- Active quotes are the hot path during allocation; composite eliminates the recheck.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_driver_quote_search_try_status
  ON atlas_driver_offer_bpp.driver_quote USING btree (search_try_id, status);

-- P1: Composite upgrade — findAllByDriverId orders by created_at DESC.
-- Existing idx_ride_driver_id_and_status covers (driver_id, status) but not the sort column.
-- Queries like getRidesForDate and findAllByDriverId must sort after filtering without this.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_ride_driver_id_status_created
  ON atlas_driver_offer_bpp.ride USING btree (driver_id, status, created_at DESC);
