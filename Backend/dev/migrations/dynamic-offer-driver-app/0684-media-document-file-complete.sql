-- Run before media document file confirm/delete/complete flow deployed
-- All previously uploaded files should have status COMPLETED instead of CONFIRMED
UPDATE atlas_driver_offer_bpp.media_file_document
SET status = 'COMPLETED'
WHERE merchant_operating_city_id IN (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'MSIL_PARTNER'
    AND city = 'Delhi'
)
AND status = 'CONFIRMED';
