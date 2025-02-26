SELECT *
FROM atlas_driver_offer_bpp.document_verification_config
WHERE
    merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE name = 'MSIL_PARTNER')
    AND is_mandatory
    AND NOT is_image_validation_required
    AND NOT is_default_enabled_on_manual_verification
    AND NOT do_strict_verifcation
    AND document_type IN ('VehicleFront', 'VehicleBack', 'VehicleRight', 'VehicleLeft',
                          'VehicleFrontInterior', 'VehicleBackInterior');