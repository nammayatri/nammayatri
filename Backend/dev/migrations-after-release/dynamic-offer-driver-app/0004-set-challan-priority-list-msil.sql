-- Challan search is opt-in (MSIL_PARTNER only). The challan_providers_priority_list
-- column no longer defaults to '{Signzy}' for all merchants, so enable it explicitly
-- for MSIL op-cities here (paired with the ChallanSearch_Signzy config in 0003).
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config muc
SET challan_providers_priority_list = '{"Signzy"}'
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE muc.merchant_operating_city_id = moc.id
  AND moc.merchant_short_id = 'MSIL_PARTNER';

-- Undo the previous blanket default for every other merchant (only rows still
-- holding the exact default value are reset, so any explicit override is preserved).
-- Without this, non-MSIL merchants attempt Signzy on every vehicle approval and hit
-- a caught-but-noisy MerchantServiceConfigNotFound; NULL takes the clean skip path.
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config muc
SET challan_providers_priority_list = NULL
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE muc.merchant_operating_city_id = moc.id
  AND moc.merchant_short_id <> 'MSIL_PARTNER'
  AND muc.challan_providers_priority_list = '{"Signzy"}';
