CREATE UNIQUE INDEX unique_code_city_id_vehicle_bpp_config_id ON atlas_app.station (code, merchant_operating_city_id, vehicle_type, integrated_bpp_config_id);

-- Run this as a single transaction for Kochi
UPDATE atlas_app.station
      SET integrated_bpp_config_id =
          (SELECT id from atlas_app.integrated_bpp_config where vehicle_category='METRO' AND merchant_operating_city_id= (SELECT id FROM atlas_app.merchant_operating_city WHERE city = 'Kochi') AND platform_type = 'PARTNERORG')
      WHERE vehicle_type = 'METRO' and merchant_operating_city_id = (SELECT id FROM atlas_app.merchant_operating_city WHERE city = 'Kochi');

WITH StationKochi AS (
    SELECT T1.address,T1.code,atlas_app.uuid_generate_v4(),(SELECT id from atlas_app.integrated_bpp_config where vehicle_category = 'METRO'and merchant_operating_city_id = (SELECT id FROM atlas_app.merchant_operating_city WHERE city = 'Kochi') and platform_type='APPLICATION') ,T1.lat,T1.lon,T1.merchant_id,T1.merchant_operating_city_id,T1.name,T1.possible_types,T1.time_bounds,T1.vehicle_type,now(),now() FROM atlas_app.station AS T1
    WHERE  vehicle_type = 'METRO' and merchant_operating_city_id = (SELECT id FROM atlas_app.merchant_operating_city WHERE city = 'Kochi'))
    INSERT  INTO  atlas_app.station (address, code, id, integrated_bpp_config_id, lat, lon, merchant_id, merchant_operating_city_id, name, possible_types, time_bounds, vehicle_type, created_at, updated_at) (SELECT * FROM StationKochi);

-- Run this as a single transaction for Delhi
UPDATE atlas_app.station
      SET integrated_bpp_config_id =
          (SELECT id from atlas_app.integrated_bpp_config where vehicle_category='METRO' AND merchant_operating_city_id= (SELECT id FROM atlas_app.merchant_operating_city WHERE city = 'Delhi') AND platform_type = 'PARTNERORG')
      WHERE vehicle_type = 'METRO' and merchant_operating_city_id = (SELECT id FROM atlas_app.merchant_operating_city WHERE city = 'Delhi');

WITH StationDelhi AS (
    SELECT T1.address,T1.code,atlas_app.uuid_generate_v4(),(SELECT id from atlas_app.integrated_bpp_config where vehicle_category = 'METRO'and merchant_operating_city_id = (SELECT id FROM atlas_app.merchant_operating_city WHERE city = 'Delhi') and platform_type='APPLICATION') ,T1.lat,T1.lon,T1.merchant_id,T1.merchant_operating_city_id,T1.name,T1.possible_types,T1.time_bounds,T1.vehicle_type,now(),now() FROM atlas_app.station AS T1
    WHERE  vehicle_type = 'METRO' and merchant_operating_city_id = (SELECT id FROM atlas_app.merchant_operating_city WHERE city = 'Delhi'))
    INSERT  INTO  atlas_app.station (address, code, id, integrated_bpp_config_id, lat, lon, merchant_id, merchant_operating_city_id, name, possible_types, time_bounds, vehicle_type, created_at, updated_at) (SELECT * FROM StationDelhi);

-- Run this as a single transaction for Chennai
UPDATE atlas_app.station
      SET integrated_bpp_config_id =
          (SELECT id from atlas_app.integrated_bpp_config where vehicle_category='METRO' AND merchant_operating_city_id= (SELECT id FROM atlas_app.merchant_operating_city WHERE city = 'Chennai') AND platform_type = 'PARTNERORG')
      WHERE vehicle_type = 'METRO' and merchant_operating_city_id = (SELECT id FROM atlas_app.merchant_operating_city WHERE city = 'Chennai');

WITH StationChennai AS (
    SELECT T1.address,T1.code,atlas_app.uuid_generate_v4(),(SELECT id from atlas_app.integrated_bpp_config where vehicle_category = 'METRO'and merchant_operating_city_id = (SELECT id FROM atlas_app.merchant_operating_city WHERE city = 'Chennai') and platform_type='MULTIMODAL') ,T1.lat,T1.lon,T1.merchant_id,T1.merchant_operating_city_id,T1.name,T1.possible_types,T1.time_bounds,T1.vehicle_type,now(),now() FROM atlas_app.station AS T1
    WHERE  vehicle_type = 'METRO' and merchant_operating_city_id = (SELECT id FROM atlas_app.merchant_operating_city WHERE city = 'Chennai'))
    INSERT  INTO  atlas_app.station (address, code, id, integrated_bpp_config_id, lat, lon, merchant_id, merchant_operating_city_id, name, possible_types, time_bounds, vehicle_type, created_at, updated_at) (SELECT * FROM StationChennai);
	
WITH StationChennai AS (
    SELECT T1.address,T1.code,atlas_app.uuid_generate_v4(),(SELECT id from atlas_app.integrated_bpp_config where vehicle_category = 'METRO'and merchant_operating_city_id = (SELECT id FROM atlas_app.merchant_operating_city WHERE city = 'Chennai') and platform_type='APPLICATION') ,T1.lat,T1.lon,T1.merchant_id,T1.merchant_operating_city_id,T1.name,T1.possible_types,T1.time_bounds,T1.vehicle_type,now(),now() FROM atlas_app.station AS T1
    WHERE  vehicle_type = 'METRO' and merchant_operating_city_id = (SELECT id FROM atlas_app.merchant_operating_city WHERE city = 'Chennai'))
    INSERT  INTO  atlas_app.station (address, code, id, integrated_bpp_config_id, lat, lon, merchant_id, merchant_operating_city_id, name, possible_types, time_bounds, vehicle_type, created_at, updated_at) (SELECT * FROM StationChennai);	


-- Clear all the cache of station table for key *CachedQueries:Station:*