--alter table atlas_driver_offer_bpp.transporter_config add column consider_special_zone_rides_for_plan_charges boolean not null default true;
alter table atlas_driver_offer_bpp.driver_fee alter column fee_without_discount type double precision;
