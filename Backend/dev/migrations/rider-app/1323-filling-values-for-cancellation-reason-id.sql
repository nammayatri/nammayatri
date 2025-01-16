UPDATE atlas_app.frfs_config
SET cancellation_reason_id = CASE
    WHEN moc.city = 'Delhi' THEN 0
    ELSE 7
END
FROM atlas_app.merchant_operating_city moc
WHERE atlas_app.frfs_config.merchant_operating_city_id = moc.id;
