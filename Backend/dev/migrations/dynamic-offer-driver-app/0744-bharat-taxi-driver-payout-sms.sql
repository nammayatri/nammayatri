-- Insert DRIVER_PAYOUT merchant_message for BHARAT_TAXI / Delhi

WITH merchant_messages AS (
  SELECT
    moc.merchant_id,
    'DRIVER_PAYOUT'::varchar        AS message_key,
    -- Template text with {#numeric#} and {#url#} placeholders
    'नमस्ते सारथी, Rs. {#numeric#} आपके अकाउंट में क्रेडिट हो चुका है। रसीद के लिए यहां क्लिक करें: {#url#} -Sahakar Taxi'::text AS message,
    moc.id                          AS merchant_operating_city_id,
    '1107176959479333641'::varchar  AS template_id,
    json_build_object(
      'var1', 'https://moving.tech/bt?'
    )                               AS json_data,
    NULL::text                      AS sender_header,
    false                            AS contains_url_button
  FROM atlas_driver_offer_bpp.merchant_operating_city moc
  JOIN atlas_driver_offer_bpp.merchant m
    ON m.id = moc.merchant_id
  WHERE m.short_id = 'BHARAT_TAXI'
    AND moc.city = 'Delhi'
)
INSERT INTO atlas_driver_offer_bpp.merchant_message
  (merchant_id,
   message_key,
   message,
   merchant_operating_city_id,
   template_id,
   json_data,
   sender_header,
   contains_url_button)
SELECT
  merchant_id,
  message_key,
  message,
  merchant_operating_city_id,
  template_id,
  json_data,
  sender_header,
  contains_url_button
FROM merchant_messages;


update atlas_driver_offer_bpp.transporter_config
set link_fleet_to_un_verified_existing_rc = true
where merchant_operating_city_id = (
  select id from atlas_driver_offer_bpp.merchant_operating_city
  where merchant_short_id = 'BHARAT_TAXI'
    and city = 'Delhi'
)