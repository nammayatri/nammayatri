UPDATE atlas_driver_offer_bpp.driver_referral SET activated_customer_count = (
    select COUNT(*) from atlas_driver_offer_bpp.driver_referral AS T1 INNER JOIN 
    atlas_driver_offer_bpp.rider_details AS T2 ON T1.driver_id = T2.referred_by_driver
    where T2.has_taken_valid_ride
);

UPDATE atlas_driver_offer_bpp.driver_referral SET referred_customer_count = (
    select COUNT(*) from atlas_driver_offer_bpp.driver_referral AS T1 INNER JOIN 
    atlas_driver_offer_bpp.rider_details AS T2 ON T1.driver_id = T2.referred_by_driver
);
