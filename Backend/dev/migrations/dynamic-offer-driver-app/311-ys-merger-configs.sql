
alter table atlas_driver_offer_bpp.transporter_config add column consider_special_zone_ride_charges_in_free_trial boolean not null default false;
alter table atlas_driver_offer_bpp.driver_fee add column plan_id text;
alter table atlas_driver_offer_bpp.driver_fee add column plan_mode text;
alter table atlas_driver_offer_bpp.plan add column max_mandate_amount double precision;
alter table atlas_driver_offer_bpp.transporter_config add column enable_udf_for_offers boolean not null default false;

update atlas_driver_offer_bpp.transporter_config set enable_udf_for_offers = true where merchant_id = '96dd7f78-787e-4a0b-8675-e9e6fe93bb8f';
update atlas_driver_offer_bpp.plan set max_mandate_amount = 35.0 where id = '18911beb-28ba-456d-8cca-4d019461d2b0';
update atlas_driver_offer_bpp.plan set max_mandate_amount = 25.0 where id = 'a35ffc7c-de0d-4dcc-83a8-e36a5a29cc1d';
update atlas_driver_offer_bpp.plan set max_mandate_amount = 150.0 where id = '25ade579-fd9c-4288-a015-337af085e66c';

update atlas_driver_offer_bpp.driver_fee
set plan_id = case
               when plan_offer_title like 'DAILY PER RIDE%' then '18911beb-28ba-456d-8cca-4d019461d2b0'
               when plan_offer_title like 'DAILY UNLIMITED%' then 'a35ffc7c-de0d-4dcc-83a8-e36a5a29cc1d'
               end
where merchant_id = '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f';


update atlas_driver_offer_bpp.driver_fee
set plan_id = case
               when plan_offer_title like 'DAILY PER RIDE%' then '25ade579-fd9c-4288-a015-337af085e66c'
               end
where merchant_id = '96dd7f78-787e-4a0b-8675-e9e6fe93bb8f';

update atlas_driver_offer_bpp.plan
set plan_type = 'DEFAULT'
where id = '25ade579-fd9c-4288-a015-337af085e66c' and plan_type = 'MANUAL';

INSERT INTO atlas_driver_offer_bpp.plan (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type, cgst_percentage , sgst_percentage, max_mandate_amount) VALUES
   ('25ade579-fd9c-4288-a015-337af085e66c', 'favorit0-0000-0000-0000-00000favorit', 'MANUAL', 'DAILY', 'PERRIDE_10', 'DAILY PER RIDE' , 'Up to a maximum of ₹100 per day', 100, 1, true, 350, 0, 'DEFAULT', 0.09 , 0.09, 150.0);
