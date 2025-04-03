-- NOTE: Only for local, DON'T RUN IN MASTER / PROD
-- updating the fare_cache_inter_city_search_locations
UPDATE atlas_app.rider_config
SET fare_cache_inter_city_search_locations = '[
    {
        "destination": { "lat": 12.9386938, "lon": 77.62057546 },
        "destinationCity": "Bangalore",
        "destinationCityBannerImageUrl": "https://example.com/banner1.png",
        "destinationCityButtonImageUrl": "https://example.com/button1.png"
    },
    {
        "destination": { "lat": 12.9386938, "lon": 77.62057546 },
        "destinationCity": "Mysore",
        "destinationCityBannerImageUrl": "https://example.com/banner2.png",
        "destinationCityButtonImageUrl": "https://example.com/button2.png"
    }
]'
WHERE merchant_operating_city_id = 'namma-yatri-0-0000-0000-00000000city';

-- fare_cache_rentals_config
UPDATE atlas_app.rider_config
SET fare_cache_rentals_config = '[
    {"rentalDuration": 3600, "rentalDistance": 10000, "rentalPriority": 1, "rentalImageUrl": "https://example.com/rental1.png"},
    {"rentalDuration": 7200, "rentalDistance": 20000, "rentalPriority": 2, "rentalImageUrl": "https://example.com/rental2.png"}
]'
WHERE merchant_operating_city_id = 'namma-yatri-0-0000-0000-00000000city';

-- updating excluded_vehicle_variants

UPDATE atlas_app.rider_config
SET excluded_vehicle_variants = ARRAY[
'AUTO_RICKSHAW','SEDAN']
WHERE merchant_operating_city_id = 'namma-yatri-0-0000-0000-00000000city';
