WITH MerchantMessages AS (
  SELECT T1.id, 'SEND_PAYMENT_LINK',
'Dear User,

Your amount payable is Rs. {#amount#} plan, for taking rides on Namma Yatri.
Please find the autopay mandate registration link below.
{#paymentLink#}

- Juspay'
  FROM atlas_driver_offer_bpp.merchant AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message)
  (SELECT * FROM MerchantMessages);
