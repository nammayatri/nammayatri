alter table atlas_driver_offer_bpp.transporter_config add column consider_special_zone_rides_for_plan_charges boolean not null default true;
alter table atlas_driver_offer_bpp.driver_fee add column special_zone_ride_count int not null default 0;
alter table atlas_driver_offer_bpp.driver_fee add column special_zone_amount  double precision NOT NULL DEFAULT 0.0;
alter table atlas_driver_offer_bpp.driver_fee alter column fee_without_discount type double precision; ----- driver fee is not enabled in kv yet we can alter the column --------
----- enable plan mandatory for yatri saath ----
------ *** while running in prod pls use the correct merchant id **** ----
update atlas_driver_offer_bpp.transporter_config
set is_plan_mandatory = true,
    driver_fee_calculation_time = 0,
    consider_special_zone_rides_for_plan_charges = false,
    open_market_un_blocked = false
where merchant_id = '96dd7f78-787e-4a0b-8675-e9e6fe93bb8f';


----- backfill for feeWithout discount ------
update atlas_driver_offer_bpp.driver_fee
set fee_without_discount = least (3.5 * num_rides, 35.0)
where plan_offer_title like 'DAILY PER RIDE%' and merchant_id = '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f';

update atlas_driver_offer_bpp.driver_fee
set fee_without_discount = 25
where plan_offer_title like 'DAILY UNLIMITED%' and merchant_id = '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f';

-----  give default plan to person if not on any in yaatri saathi -----

insert into atlas_driver_offer_bpp.driver_plan ( driver_id,plan_id ,plan_type , mandate_id ,created_at ,updated_at )
select dr.id, '25ade579-fd9c-4288-a015-337af085e66c', 'MANUAL', null, now(), now()
from atlas_driver_offer_bpp.person as dr left JOIN atlas_driver_offer_bpp.driver_plan as dp on dp.driver_id = dr.id
where dp.driver_id is null and dr.merchant_id = '96dd7f78-787e-4a0b-8675-e9e6fe93bb8f';


----  make auto pay status pending for each person if autopay is null ----

update atlas_driver_offer_bpp.driver_information
set auto_pay_status = 'PENDING'
where merchant_id = '96dd7f78-787e-4a0b-8675-e9e6fe93bb8f';
