ALTER TABLE atlas_driver_offer_bpp.driver_rc_association ADD COLUMN is_rc_active boolean DEFAULT false not null;
ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN multiple_rc boolean;

-- make currently linked RC to driver as active RC
update atlas_driver_offer_bpp.driver_rc_association set is_rc_active = true where associated_till > now();

-- returns drivers who are having multiple RCs active, should be 0 ideally if not zero resolve
select driver_id from
    (select driver_id, count(*) as driver_count from atlas_driver_offer_bpp.driver_rc_association
        where is_rc_active = true group by 1
    ) as foo
where driver_count > 1;

-- Make all RCs as linked to driver (not active), whatever he/she linked earlier
update atlas_driver_offer_bpp.driver_rc_association as T1 set associated_till = '2099-12-12' where associated_till < now() and
    rc_id not in (select rc_id from atlas_driver_offer_bpp.driver_rc_association where driver_id = T1.driver_id and is_rc_active = true);


------------------------------------------------------------------------------------------

ALTER TABLE atlas_driver_offer_bpp.call_status ADD entity_id character(36);
update atlas_driver_offer_bpp.call_status set entity_id = ride_id where entity_id is null;
ALTER TABLE atlas_driver_offer_bpp.call_status ALTER COLUMN entity_id SET DEFAULT 'UNKOWN';
ALTER TABLE atlas_driver_offer_bpp.call_status ALTER COLUMN entity_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD rc_limit integer DEFAULT 3 not null;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD automatic_rc_activation_cut_off integer DEFAULT 432000 not null; -- 5 days

---- DROPS ----
ALTER TABLE atlas_driver_offer_bpp.call_status DROP COLUMN ride_id;
