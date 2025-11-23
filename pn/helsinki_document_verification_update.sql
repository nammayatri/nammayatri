-- Update is_mandatory_for_enabling to true for DriverLicense, TaxiDriverPermit, and FinnishIDResidencePermit
-- for Helsinki city

UPDATE atlas_driver_offer_bpp.document_verification_config
SET 
  is_mandatory_for_enabling = true,
  updated_at = CURRENT_TIMESTAMP
WHERE 
  merchant_operating_city_id = (
    SELECT id 
    FROM atlas_driver_offer_bpp.merchant_operating_city 
    WHERE city = 'Helsinki'
  )
  AND document_type IN ('DriverLicense', 'TaxiDriverPermit', 'FinnishIDResidencePermit');

-- Update is_mandatory to true for FinnishIDResidencePermit for Helsinki city

UPDATE atlas_driver_offer_bpp.document_verification_config
SET 
  is_mandatory = true,
  updated_at = CURRENT_TIMESTAMP
WHERE 
  merchant_operating_city_id = (
    SELECT id 
    FROM atlas_driver_offer_bpp.merchant_operating_city 
    WHERE city = 'Helsinki'
  )
  AND document_type = 'FinnishIDResidencePermit';

-- Verification query to check the updates
SELECT 
  document_type,
  is_mandatory,
  is_mandatory_for_enabling
FROM atlas_driver_offer_bpp.document_verification_config
WHERE 
  merchant_operating_city_id = (
    SELECT id 
    FROM atlas_driver_offer_bpp.merchant_operating_city 
    WHERE city = 'Helsinki'
  )
  AND document_type IN ('DriverLicense', 'TaxiDriverPermit', 'FinnishIDResidencePermit')
ORDER BY document_type;

