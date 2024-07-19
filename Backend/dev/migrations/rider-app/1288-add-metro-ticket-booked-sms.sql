INSERT INTO atlas_app.merchant_message
    (merchant_id, merchant_operating_city_id, message, message_key, sender_header, created_at, updated_at)
SELECT
    merchant_id,
    id as merchant_operating_city_id,
    'Your metro {#TICKET_PLURAL#} booked! {#URL#} Click the link to view and manage your booking. Thank you for choosing Namma Yatri!',
    'METRO_TICKET_BOOKED',
    'NMYTRI',
    now() as created_at,
    now() as updated_at
FROM atlas_app.merchant_operating_city;
