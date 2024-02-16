UPDATE atlas_driver_offer_bpp.driver_information AS di
SET driver_dob = dl.driver_dob
FROM atlas_driver_offer_bpp.driver_license AS dl
WHERE di.driver_id = dl.driver_id;