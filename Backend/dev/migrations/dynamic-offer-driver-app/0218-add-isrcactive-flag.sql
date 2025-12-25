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

ALTER TABLE atlas_driver_offer_bpp.call_status ALTER COLUMN entity_id SET DEFAULT 'UNKOWN';
---- DROPS ----
