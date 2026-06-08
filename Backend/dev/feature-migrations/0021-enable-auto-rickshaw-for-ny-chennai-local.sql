-- Issue: NY Chennai BPP (MOCID f8e9db0a) has AUTO_RICKSHAW OneWay_OneWayOnDemandDynamicOffer
-- fare products disabled (enabled='f') in production/master config-sync.
-- Production intentionally disables auto dynamic-offer for Chennai, but local integration
-- tests need it enabled to exercise the NY Auto Ride Flow, Auto User Cancellation, and
-- Auto Driver Cancellation + Reallocation suites for the Chennai environment.
-- Fix: Enable the Default-area AUTO_RICKSHAW dynamic-offer fare products for Chennai locally.
UPDATE atlas_driver_offer_bpp.fare_product
SET enabled = true
WHERE merchant_operating_city_id = 'f8e9db0a-96c8-49e4-942a-3e3f7265d2da'
  AND vehicle_variant = 'AUTO_RICKSHAW'
  AND trip_category = 'OneWay_OneWayOnDemandDynamicOffer'
  AND area = 'Default';
