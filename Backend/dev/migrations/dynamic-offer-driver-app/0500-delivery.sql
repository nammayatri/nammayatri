-- only for local --
UPDATE atlas_driver_offer_bpp.fare_product
SET trip_category = 'Delivery_OneWayOnDemandDynamicOffer'
WHERE vehicle_variant = 'DELIVERY_BIKE' AND trip_category = 'OneWay_OneWayOnDemandDynamicOffer';

-- only for local --
update atlas_driver_offer_bpp.vehicle_service_tier
set allowed_vehicle_variant = '{BIKE,DELIVERY_BIKE}'
where service_tier_type = 'DELIVERY_BIKE';