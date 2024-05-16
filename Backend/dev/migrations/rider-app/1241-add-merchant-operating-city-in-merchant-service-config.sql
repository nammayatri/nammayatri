-------------- pls run the following in prod - already in master --------------

update atlas_app.merchant_service_config as msc
set merchant_operating_city_id = moc.id
from (select * from atlas_app.merchant_operating_city where city = (select city from atlas_app.merchant where id = merchant_id)) as moc
where moc.merchant_id = msc.merchant_id;

ALTER TABLE atlas_app.merchant_service_config DROP CONSTRAINT merchant_service_config_pkey;
ALTER TABLE atlas_app.merchant_service_config ADD PRIMARY KEY(service_name, merchant_operating_city_id);

insert into atlas_app.merchant_service_config (merchant_id, service_name, config_json, merchant_operating_city_id)
select msc.merchant_id, msc.service_name, msc.config_json, moc.id
from atlas_app.merchant_service_config as msc, atlas_app.merchant_operating_city as moc
where msc.merchant_id = moc.merchant_id and not msc.merchant_operating_city_id is null
ON CONFLICT (service_name, merchant_operating_city_id)
DO NOTHING;