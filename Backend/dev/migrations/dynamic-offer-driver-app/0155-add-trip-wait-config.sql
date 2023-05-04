ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN waiting_block_in_minutes double precision not null default 15;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN free_waiting_time double precision not null default 5;
ALTER TABLE atlas_driver_offer_bpp.slab_fare_policy ADD COLUMN trip_waiting_charge_per_block int not null default 5;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN trip_waiting_charge_per_block int not null default 5;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN trip_waiting_fare integer;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN pick_up_waiting_fare integer;