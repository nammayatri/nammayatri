
 --- alter in driver plan table --

Alter table atlas_driver_offer_bpp.driver_plan add column vehicle_number Text;

--- alter in driver fee ---

----------- NOTE :- don't run this query in prod, just to rectify the column in local------------
Alter table atlas_driver_offer_bpp.driver_fee alter column collected_at SET DATA TYPE  timestamp with time zone;
---------------------------------- -------------------------------    -------------------
-------- alter in invoice ---

----- alter in plan table ----

Alter table atlas_driver_offer_bpp.plan drop CONSTRAINT plan_pkey;
Alter table atlas_driver_offer_bpp.plan add PRIMARY KEY(id, service_name, payment_mode, merchant_op_city_id);

--------- alter in subscription config -----
ALTER TABLE atlas_driver_offer_bpp.subscription_config ALTER COLUMN generic_job_reschedule_time SET DATA TYPE integer USING generic_job_reschedule_time::integer;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ALTER COLUMN payment_link_job_time SET DATA TYPE integer USING payment_link_job_time::integer;

-------- alter payment order in both rider and driver ---

Alter table atlas_driver_offer_bpp.payment_order add column deep_link Text;

ALTER TABLE atlas_driver_offer_bpp.merchant_service_config DROP CONSTRAINT merchant_service_config_pkey;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_config ADD PRIMARY KEY(service_name, merchant_operating_city_id);