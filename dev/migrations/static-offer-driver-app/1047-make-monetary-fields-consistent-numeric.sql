ALTER TABLE atlas_transporter.one_way_quote ALTER COLUMN distance TYPE numeric (30,2);
ALTER TABLE atlas_transporter.one_way_quote ALTER COLUMN distance_to_nearest_driver TYPE numeric (30,2);

ALTER TABLE atlas_transporter.quote ALTER COLUMN discount TYPE numeric (30,2);

ALTER TABLE atlas_transporter.person ALTER COLUMN rating TYPE numeric (10,2);

ALTER TABLE atlas_transporter.discount_transaction ALTER COLUMN discount TYPE numeric (30,2);

ALTER TABLE atlas_transporter.fare_policy ALTER COLUMN base_fare TYPE numeric(30,2);

ALTER TABLE atlas_transporter.one_way_fare_policy_per_extra_km_rate ALTER COLUMN distance_range_start TYPE integer;
ALTER TABLE atlas_transporter.one_way_fare_policy_per_extra_km_rate ALTER COLUMN fare TYPE numeric(30,2);

ALTER TABLE atlas_transporter.rental_fare_policy ALTER COLUMN base_fare TYPE numeric(30,2);
ALTER TABLE atlas_transporter.rental_fare_policy ALTER COLUMN extra_km_fare TYPE numeric(30,2);
ALTER TABLE atlas_transporter.rental_fare_policy ALTER COLUMN extra_minute_fare TYPE numeric(30,2);
ALTER TABLE atlas_transporter.rental_fare_policy ALTER COLUMN driver_allowance_for_day TYPE numeric(30,2);

ALTER TABLE atlas_transporter.ride ALTER COLUMN fare TYPE numeric(30,2);
ALTER TABLE atlas_transporter.ride ALTER COLUMN traveled_distance TYPE numeric(30,2);
ALTER TABLE atlas_transporter.ride ALTER COLUMN chargeable_distance TYPE numeric(30,2);

ALTER TABLE atlas_transporter.booking ALTER COLUMN estimated_fare TYPE numeric (10,2);
ALTER TABLE atlas_transporter.booking ALTER COLUMN discount TYPE numeric (10,2);
ALTER TABLE atlas_transporter.booking ALTER COLUMN estimated_total_fare TYPE numeric (10,2);
