-- Adding Columns in Driver Stats
-- Updating total distance and total rides in ride_stats table
UPDATE atlas_driver_offer_bpp.driver_stats
    SET total_rides = ride_counts.count,
        total_distance = ride_counts.distance_sum
    FROM (
        SELECT driver_id, COUNT(*) AS count, SUM(chargeable_distance) AS distance_sum
        FROM atlas_driver_offer_bpp.ride
        WHERE status = 'COMPLETED'
        GROUP BY driver_id
    ) AS ride_counts
    WHERE ride_counts.driver_id = driver_stats.driver_id;

-- Add column in Transporter Configs
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_leader_board_expiry int;

-- Update value of driver_leader_board_expiry column in transport_config
UPDATE atlas_driver_offer_bpp.transporter_config SET driver_leader_board_expiry = 3600;