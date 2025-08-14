-- Make sure that frontend sends proper fileType and reqContentType, before running it
-- dashboard side: /fleet/register, /fleet/v2/register, /driver/{driverId}/document/upload
-- UI side: /driver/register/validateImage, /driver/register/validateImageFile
UPDATE atlas_driver_offer_bpp.merchant SET file_extension_check = true WHERE short_id = 'MSIL_PARTNER'
