Alter table atlas_app.merchant_service_config ADD COLUMN merchant_operating_city_id character varying(36);

update atlas_app.merchant_service_config as msc
set merchant_operating_city_id = moc.id
from (select * from atlas_app.merchant_operating_city where city = (select city from atlas_app.merchant where id = merchant_id)) as moc
where moc.merchant_id = msc.merchant_id;
------------ run this after the 1242 drop key constriants ----------------------------------------
insert into atlas_app.merchant_service_config (merchant_id, service_name, config_json, merchant_operating_city_id)
select msc.merchant_id, msc.service_name, msc.config_json, moc.id
from atlas_app.merchant_service_config as msc, atlas_app.merchant_operating_city as moc
where msc.merchant_id = moc.merchant_id and not msc.merchant_operating_city_id is null
ON CONFLICT (service_name, merchant_operating_city_id)
DO NOTHING;