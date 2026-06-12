-- Challan search is opt-in (MSIL_PARTNER only). The challan_providers_priority_list
-- column is added with no default, so all merchants start NULL (clean skip path).
-- Enable it explicitly for MSIL op-cities here (paired with the ChallanSearch_Signzy
-- config in 0003).
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config muc
SET challan_providers_priority_list = '{"Signzy"}'
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE muc.merchant_operating_city_id = moc.id
  AND moc.merchant_short_id = 'MSIL_PARTNER';
