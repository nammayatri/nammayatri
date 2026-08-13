CREATE UNIQUE INDEX CONCURRENTLY idx_rider_preferences_rider_ride_config ON atlas_app.rider_preferences (rider_id) WHERE preference_type = 'RIDE_CONFIG';
