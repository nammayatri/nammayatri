 -- NOTE: Only for local, DON'T RUN IN MASTER / PROD

UPDATE atlas_app.rider_config
SET fare_cache_inter_city_search_locations =
    '[
        {
            "destination": { "lat": 12.9386938, "lon": 77.62057546 },
            "destinationCity": "Bangalore",
            "destinationCityBannerImageUrl": "https://example.com/banner1.png",
            "destinationCityButtonImageUrl": "https://example.com/button1.png",
            "destinationDistance": 56000,
            "destinationDuration": 7200
        },
        {
            "destination": { "lat": 12.9386938, "lon": 77.62057546 },
            "destinationCity": "Mysore",
            "destinationCityBannerImageUrl": "https://example.com/banner2.png",
            "destinationCityButtonImageUrl": "https://example.com/button2.png",
            "destinationDistance": 150000,
            "destinationDuration": 25000
        }
    ]'
WHERE merchant_operating_city_id = 'namma-yatri-0-0000-0000-00000000city';