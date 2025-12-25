UPDATE atlas_app.rider_config AS rc
SET kapture_queue =
    CASE
        WHEN moc.state = 'NationalCapitalTerritory' AND moc.merchant_short_id NOT IN ('MOBILITY_PAYTM', 'MOBILITY_REDBUS') THEN 'NY_CR_NCR_API'
        WHEN moc.state = 'Kerala' AND moc.merchant_short_id NOT IN ('MOBILITY_PAYTM', 'MOBILITY_REDBUS') THEN 'NY_CR_KL_API'
        WHEN moc.state = 'Karnataka' AND moc.merchant_short_id NOT IN ('MOBILITY_PAYTM', 'MOBILITY_REDBUS') THEN 'NY_CR_KA_API'
        WHEN moc.state = 'Telangana' AND moc.merchant_short_id NOT IN ('MOBILITY_PAYTM', 'MOBILITY_REDBUS') THEN 'MY_CR_TS_AP_API'
        WHEN moc.state = 'TamilNadu' AND moc.merchant_short_id NOT IN ('MOBILITY_PAYTM', 'MOBILITY_REDBUS') THEN 'NY_CR_TN_API'
        WHEN moc.state = 'WestBengal' AND moc.merchant_short_id NOT IN ('MOBILITY_PAYTM', 'MOBILITY_REDBUS') THEN 'YS_CR_WB_API'
    END
FROM atlas_app.merchant_operating_city AS moc
WHERE rc.merchant_operating_city_id = moc.id;
