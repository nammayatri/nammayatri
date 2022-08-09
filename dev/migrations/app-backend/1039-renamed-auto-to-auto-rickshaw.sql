UPDATE atlas_app.booking
    SET vehicle_variant = 'AUTO_RICKSHAW'
    WHERE vehicle_variant = 'AUTO';
UPDATE atlas_app.estimate
    SET vehicle_variant = 'AUTO_RICKSHAW'
    WHERE vehicle_variant = 'AUTO';
UPDATE atlas_app.quote
    SET  vehicle_variant = 'AUTO_RICKSHAW'
    WHERE vehicle_variant = 'AUTO';
UPDATE atlas_app.ride
    SET vehicle_variant = 'AUTO_RICKSHAW'
    WHERE vehicle_variant = 'AUTO';
