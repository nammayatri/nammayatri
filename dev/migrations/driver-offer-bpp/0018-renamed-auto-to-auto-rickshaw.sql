UPDATE atlas_driver_offer_bpp.vehicle
    SET variant = 'AUTO_RICKSHAW'
    WHERE variant = 'AUTO_VARIANT';
UPDATE atlas_driver_offer_bpp.booking
    SET vehicle_variant = 'AUTO_RICKSHAW'
    WHERE vehicle_variant = 'AUTO_VARIANT';
UPDATE atlas_driver_offer_bpp.business_event
    SET vehicle_variant = 'AUTO_RICKSHAW'
    WHERE vehicle_variant = 'AUTO_VARIANT';
UPDATE atlas_driver_offer_bpp.driver_quote
    SET  vehicle_variant = 'AUTO_RICKSHAW'
    WHERE vehicle_variant = 'AUTO_VARIANT';
UPDATE atlas_driver_offer_bpp.fare_policy
    SET vehicle_variant = 'AUTO_RICKSHAW'
    WHERE vehicle_variant = 'AUTO_VARIANT';
UPDATE atlas_driver_offer_bpp.search_request_for_driver
    SET  vehicle_variant = 'AUTO_RICKSHAW'
    WHERE vehicle_variant = 'AUTO_VARIANT';