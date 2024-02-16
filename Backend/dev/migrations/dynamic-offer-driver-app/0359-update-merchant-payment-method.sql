UPDATE atlas_driver_offer_bpp.merchant_payment_method SET payment_type = 'ON_FULFILLMENT' WHERE payment_type = 'POSTPAID';
DELETE FROM atlas_driver_offer_bpp.merchant_payment_method WHERE payment_type = 'PREPAID';
