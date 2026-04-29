

-- returns drivers who are having multiple RCs active, should be 0 ideally if not zero resolve
select driver_id from
    (select driver_id, count(*) as driver_count from atlas_driver_offer_bpp.driver_rc_association
        where is_rc_active = true group by 1
    ) as foo
where driver_count > 1;

------------------------------------------------------------------------------------------

ALTER TABLE atlas_driver_offer_bpp.call_status ALTER COLUMN entity_id SET DEFAULT 'UNKOWN';
---- DROPS ----
