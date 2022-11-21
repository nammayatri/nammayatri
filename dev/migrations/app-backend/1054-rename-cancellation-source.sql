UPDATE atlas_app.booking_cancellation_reason
    SET source = 'ByMerchant'
    WHERE source = 'ByOrganization';
