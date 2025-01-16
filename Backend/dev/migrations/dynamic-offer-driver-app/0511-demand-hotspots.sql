-- Do not run in prod:
update atlas_driver_offer_bpp.transporter_config set demand_hotspots_config = json_build_object(
    'enableDemandHotspots', json('true')::jsonb,
    'precisionOfGeohash', 5,
    'analysisDurationMinutes', 25,
    'resultDurationMinutes', 5,
    'noOfGeohashesToReturn', 300
  );
