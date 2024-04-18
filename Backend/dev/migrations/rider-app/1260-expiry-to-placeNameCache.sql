alter table atlas_app.place_name_cache add column created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;

update atlas_app.rider_config set place_name_cache_expiry_days = 90;