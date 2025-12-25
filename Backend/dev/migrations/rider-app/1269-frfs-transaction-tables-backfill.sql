-- NOTE: All queries are required to be run to make below column not null, new version already has it mandatory.
----------------------------------------------------------------------------------------------------------------------------------
-- BACKFILL QUERY for frfs_search table
UPDATE atlas_app.frfs_search
SET
    merchant_id = station.merchant_id,
    merchant_operating_city_id = station.merchant_operating_city_id
FROM atlas_app.station
WHERE atlas_app.frfs_search.from_station_id = station.id;

-- BACKFILL QUERY for frfs_quote table
UPDATE atlas_app.frfs_quote
SET
    merchant_id = station.merchant_id,
    merchant_operating_city_id = station.merchant_operating_city_id
FROM atlas_app.station
WHERE atlas_app.frfs_quote.from_station_id = station.id;

-- BACKFILL QUERY for frfs_ticket_booking table
UPDATE atlas_app.frfs_ticket_booking
SET
    merchant_id = station.merchant_id,
    merchant_operating_city_id = station.merchant_operating_city_id
FROM atlas_app.station
WHERE atlas_app.frfs_ticket_booking.from_station_id = station.id;

-- BACKFILL QUERY for frfs_ticket table
UPDATE atlas_app.frfs_ticket
SET
    merchant_id = frfs_ticket_booking.merchant_id,
    merchant_operating_city_id = frfs_ticket_booking.merchant_operating_city_id
FROM atlas_app.frfs_ticket_booking
WHERE atlas_app.frfs_ticket.frfs_ticket_booking_id = frfs_ticket_booking.id;
------------------------------------------------------* END *---------------------------------------------------------------------
