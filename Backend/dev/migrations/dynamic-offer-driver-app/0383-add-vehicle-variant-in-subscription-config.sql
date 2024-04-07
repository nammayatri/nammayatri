alter table atlas_driver_offer_bpp.driver_fee add column vehicle_variant text;
alter table atlas_driver_offer_bpp.invoice add column vehicle_variant text;

----- backfill -----

update atlas_driver_offer_bpp.subscription_config
set vehicle_variant = 'AUTO_RICKSHAW'
where merchant_op_city_id in ('NAMMA-YATRI-CITIES/YATRI/PARIS');

update atlas_driver_offer_bpp.subscription_config
set vehicle_variant = 'TAXI'
where merchant_op_city_id in ('YATRI-SAATHI-CITIES');

------- post back fill and release add subscription_config for other variants -----------
