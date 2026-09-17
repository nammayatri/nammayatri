CREATE TABLE atlas_driver_offer_bpp.value_add_np ();

ALTER TABLE atlas_driver_offer_bpp.value_add_np ADD COLUMN enabled boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.value_add_np ADD COLUMN subscriber_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.value_add_np ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.value_add_np ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.value_add_np ADD PRIMARY KEY ( subscriber_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.value_add_np ADD COLUMN enable_one_shot_assign boolean ;