-- Run after release

UPDATE atlas_driver_offer_bpp.driver_information SET on_ride = false where driver_id in (SELECT distinct driver_id from atlas_driver_offer_bpp.driver_information where on_ride AND
driver_id NOT IN (SELECT driver_id from atlas_driver_offer_bpp.ride where status IN ('NEW', 'INPROGRESS')))