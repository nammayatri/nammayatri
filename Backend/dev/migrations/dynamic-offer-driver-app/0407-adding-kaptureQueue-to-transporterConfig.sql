UPDATE atlas_driver_offer_bpp.transporter_config AS tc
SET kapture_queue =
    CASE
        WHEN moc.state = 'NationalCapitalTerritory' THEN 'NY_DR_NCR_API'
        WHEN moc.state = 'Kerala' THEN 'NY_DR_KL_API'
        WHEN moc.state = 'Karnataka' THEN 'NY_DR_KA_API'
        WHEN moc.state = 'Telangana' THEN 'MY_DR_TS_AP_API'
        WHEN moc.state = 'TamilNadu' THEN 'NY_DR_TN_API'
        WHEN moc.state = 'WestBengal' THEN 'YS_DR_WB_API'
    END
FROM atlas_driver_offer_bpp.merchant_operating_city AS moc
WHERE tc.merchant_operating_city_id = moc.id;
