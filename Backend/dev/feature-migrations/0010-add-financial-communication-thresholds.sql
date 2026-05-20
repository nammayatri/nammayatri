-- Enable financial notifications for MSIL_PARTNER
UPDATE atlas_driver_offer_bpp.transporter_config
SET enable_financial_notifications = true
WHERE merchant_operating_city_id IN (
  SELECT moc.id FROM atlas_driver_offer_bpp.merchant_operating_city moc
  JOIN atlas_driver_offer_bpp.merchant m ON m.id = moc.merchant_id
  WHERE m.short_id = 'MSIL_PARTNER'
);
