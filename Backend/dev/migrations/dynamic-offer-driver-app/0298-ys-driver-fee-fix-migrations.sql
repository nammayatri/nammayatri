
----- for master env -----

update atlas_driver_offer_bpp.driver_fee
set status = 'EXEMPTED'
where govt_charges + platform_fee + cgst + sgst = 0 and fee_type = 'RECURRING_EXECUTION_INVOICE' and merchant_id = '96dd7f78-787e-4a0b-8675-e9e6fe93bb8f' ;

update atlas_driver_offer_bpp.driver_fee
set fee_type = 'RECURRING_INVOICE',
    status = 'PAYMENT_PENDING'
where fee_type = 'RECURRING_EXECUTION_INVOICE' and merchant_id = '96dd7f78-787e-4a0b-8675-e9e6fe93bb8f' and pay_by >= now() and not (start_time = timestamp with time zone '2023-11-06 10:00:00.000Z') ;

update atlas_driver_offer_bpp.driver_fee
set fee_type = 'RECURRING_INVOICE',
    status = 'PAYMENT_OVERDUE'
where fee_type = 'RECURRING_EXECUTION_INVOICE' and merchant_id = '96dd7f78-787e-4a0b-8675-e9e6fe93bb8f' and pay_by < now() and not (start_time = timestamp with time zone '2023-11-06 10:00:00.000Z') ;

update atlas_driver_offer_bpp.driver_fee
set fee_type = 'RECURRING_INVOICE'
where fee_type = 'RECURRING_EXECUTION_INVOICE' and merchant_id = '96dd7f78-787e-4a0b-8675-e9e6fe93bb8f' and start_time = timestamp with time zone '2023-11-06 10:00:00.000Z' ;


---- for prod env ----

update atlas_driver_offer_bpp.driver_fee
set status = 'EXEMPTED'
where govt_charges + platform_fee + cgst + sgst = 0 and fee_type = 'RECURRING_EXECUTION_INVOICE' and merchant_id = 'd2929b92-8b12-4e21-9efd-d6203940c4c5';

update atlas_driver_offer_bpp.driver_fee
set fee_type = 'RECURRING_INVOICE',
    status = 'PAYMENT_PENDING'
where fee_type = 'RECURRING_EXECUTION_INVOICE' and merchant_id = 'd2929b92-8b12-4e21-9efd-d6203940c4c5' and pay_by >= now() and not (start_time = timestamp with time zone '2023-11-06 10:00:00.000Z') ;

update atlas_driver_offer_bpp.driver_fee
set fee_type = 'RECURRING_INVOICE',
    status = 'PAYMENT_OVERDUE'
where fee_type = 'RECURRING_EXECUTION_INVOICE' and merchant_id = 'd2929b92-8b12-4e21-9efd-d6203940c4c5' and pay_by < now() and not (start_time = timestamp with time zone '2023-11-06 10:00:00.000Z') ;

update atlas_driver_offer_bpp.driver_fee
set fee_type = 'RECURRING_INVOICE'
where fee_type = 'RECURRING_EXECUTION_INVOICE' and merchant_id = 'd2929b92-8b12-4e21-9efd-d6203940c4c5' and start_time = timestamp with time zone '2023-11-06 10:00:00.000Z' ;