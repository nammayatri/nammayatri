alter table atlas_driver_offer_bpp.transporter_config add column consider_special_zone_rides_for_plan_charges boolean not null default false;
alter table atlas_driver_offer_bpp.driver_fee add column special_zone_ride_count int not null default 0;
alter table atlas_driver_offer_bpp.driver_fee add column special_zone_amount  numeric(30,2) NOT NULL DEFAULT 0.0;

----- enable plan mandatory for yatri saath ----
------ *** while running in prod pls use the correct merchant id **** ----
update atlas_driver_offer_bpp.transporter_config
set is_plan_mandatory = true,
    driver_fee_calculation_time = 0,
    consider_special_zone_rides_for_plan_charges = false
where merchant_id = '25ade579-fd9c-4288-a015-337af085e66c';


----- backfill for feeWithout discount ------

update atlas_driver_offer_bpp.driver_fee
set fee_without_discount = 3.5 * num_rides
where plan_offer_title like 'DAILY PER RIDE%';

update atlas_driver_offer_bpp.driver_fee
set fee_without_discount = 25
where plan_offer_title like 'DAILY UNLIMITED%';