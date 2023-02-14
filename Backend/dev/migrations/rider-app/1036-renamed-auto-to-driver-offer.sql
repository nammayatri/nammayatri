UPDATE atlas_app.quote
    SET fare_product_type = 'DRIVER_OFFER'
    WHERE fare_product_type = 'AUTO';

UPDATE atlas_app.booking
    SET fare_product_type = 'DRIVER_OFFER'
    WHERE fare_product_type = 'AUTO';