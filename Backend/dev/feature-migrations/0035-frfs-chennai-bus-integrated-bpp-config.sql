-- FRFS Fleet Operator — IntegratedBPPConfig for Chennai BUS (DIRECT/GIMS)
-- Required by FRFSFleetOperatorFlow/01-ConductorFlow.json.
-- Modeled after the Kolkata BUS entry (kolkata_bus / 3bfe56dd-...).

INSERT INTO atlas_driver_offer_bpp.integrated_bpp_config
  ( id
  , agency_key
  , feed_key
  , domain
  , merchant_id
  , merchant_operating_city_id
  , platform_type
  , config_json
  , city
  , vehicle_category
  , created_at
  , updated_at
  )
SELECT
    md5('frfs-chennai-bus-ibpp-config')::uuid::text
  , 'chennai_bus'
  , 'chennai_bus'
  , 'FRFS'
  , '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f'
  , 'f8e9db0a-96c8-49e4-942a-3e3f7265d2da'
  , 'APPLICATION'
  , '{"tag":"DIRECT","contents":{"baseUrl":"https://api.c2.sandbox.moving.tech/gtfs-inmemory","cipherKey":"4nLE9cN2golcxW16Df5aOcajSJqDAupo2B4rLcczKEI="}}'::json
  , 'Chennai'
  , 'BUS'
  , now()
  , now()
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_driver_offer_bpp.integrated_bpp_config
  WHERE id = md5('frfs-chennai-bus-ibpp-config')::uuid::text
);
