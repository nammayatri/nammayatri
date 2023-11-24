CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.daily_stats (
    id VARCHAR(36) NOT NULL PRIMARY KEY,
    driver_id character(36) NOT NULL,
    total_earnings INT NOT NULL,
    num_rides INT NOT NULL,
    total_distance INT NOT NULL,
    merchant_local_date Date NOT NULL
);

CREATE INDEX idx_driver_date ON atlas_driver_offer_bpp.daily_stats (driver_id, merchant_local_date);