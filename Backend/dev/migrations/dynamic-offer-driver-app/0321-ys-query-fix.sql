--- already executed in master and prod ---
update atlas_driver_offer_bpp.plan
set plan_type = 'DEFAULT'
where id = '25ade579-fd9c-4288-a015-337af085e66c' and payment_mode = 'MANUAL';


alter table atlas_driver_offer_bpp.transporter_config  add column allow_default_plan_allocation boolean default false;

------- for prod ------
update atlas_driver_offer_bpp.transporter_config
set allow_default_plan_allocation = true
where merchant_id = 'd2929b92-8b12-4e21-9efd-d6203940c4c5' and merchant_operating_city_id = 'e35f2a66-58fe-be6e-cafd-38e67165fd84';

------- for master ------
update atlas_driver_offer_bpp.transporter_config
set allow_default_plan_allocation = true
where merchant_id = '25ade579-fd9c-4288-a015-337af085e66c';

