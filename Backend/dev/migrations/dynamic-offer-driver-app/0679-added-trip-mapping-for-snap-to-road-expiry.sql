UPDATE atlas_driver_offer_bpp.transporter_config
SET snap_to_road_expiry_by_category =
  '{
    "tripCategoryMap": {
      "OneWay_OneWayRideOtp": 21600,
      "OneWay_OneWayOnDemandStaticOffer": 21600,
      "OneWay_OneWayOnDemandDynamicOffer": 21600,
      "OneWay_MeterRide": 21600,
      "Rental_RideOtp": 108000,
      "Rental_OnDemandStaticOffer": 108000,
      "RideShare_RideOtp": 21600,
      "RideShare_OnDemandStaticOffer": 21600,
      "Ambulance_OneWayRideOtp": 21600,
      "Ambulance_OneWayOnDemandStaticOffer": 21600,
      "Ambulance_OneWayOnDemandDynamicOffer": 21600,
      "Ambulance_MeterRide": 21600,
      "Delivery_OneWayRideOtp": 21600,
      "Delivery_OneWayOnDemandStaticOffer": 21600,
      "Delivery_OneWayOnDemandDynamicOffer": 21600,
      "Delivery_MeterRide": 21600,
      "InterCity_OneWayRideOtp": 108000,
      "InterCity_OneWayOnDemandStaticOffer": 108000,
      "InterCity_OneWayOnDemandDynamicOffer": 108000,
      "InterCity_MeterRide": 108000,
      "CrossCity_OneWayRideOtp": 21600,
      "CrossCity_OneWayOnDemandStaticOffer": 21600,
      "CrossCity_OneWayOnDemandDynamicOffer": 21600,
      "CrossCity_MeterRide": 21600
    }
  }'::jsonb;