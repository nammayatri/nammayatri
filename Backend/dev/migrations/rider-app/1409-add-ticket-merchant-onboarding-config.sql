INSERT INTO atlas_app.merchant_onboarding_step_config (
    onboarding_type,
    step_name_identifier,
    step_description,
    dependency,
    is_approval_required,
    created_at,
    updated_at
) VALUES (
    'TICKET_MERCHANT_ONBOARDING',
    'TICKET-MERCHANT-DETAILS',
    'Please provide merchant information',
    ARRAY[]::text[],
    true,
    NOW(),
    NOW()
);

WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'TICKET_MERCHANT_AGREEMENT_TEMPLATE',
'<!doctypehtml><html lang=en><meta charset=UTF-8><meta content="width=device-width,initial-scale=1"name=viewport><title>Letter from Merchant</title><style>body{font-family:Arial,sans-serif;line-height:1.6;margin:40px}.letter{border:1px solid #000;padding:20px;max-width:800px;margin:0 auto}.letter-header{margin-bottom:40px}.letter-footer{margin-top:40px}.signature{margin-top:60px}.letter-subject{font-weight:700}.container{width:600px;word-wrap:break-word}</style><div class=letter><div class=letter-header><p><strong>To,</strong><p>Date: {#date#}<p>Naveen Chaluvadi,<br>Chief Digital Officer, YES Bank<br>5th Floor, Digital Banking,<br>YES Bank House, Off Western Express Highway,<br>Santacruz (E), Mumbai - 400055</div><div class=letter-body><p class=letter-subject>Sub: Settlement of UPI Collections through Society for Natural Language Technology Research (SNLTR), Government of West Bengal, IT&E Department via YES Bank’s UPI payments setup<p>Dear Sir,<p>We hereby confirm that <strong>{#orgName#}</strong> is availing the services of UPI Collections offered by Society for Natural Language Technology Research (SNLTR), Government of West Bengal, IT&E Department through YES Bank UPI payments setup and also agrees to the terms laid down between Society for Natural Language Technology Research (SNLTR), Government of West Bengal, IT&E and YES Bank for the use case mentioned below:<p><strong>Use Case: &lt;Ticketing through Yatri Sathi Mobile Application.></strong><p>We request YES Bank to process the settlement for these collections to -<p><strong>Account Number:</strong> {#accountNumber#}<br><strong>IFSC:</strong> {#ifsc#}<p><strong>Other Details:</strong><p>GST Number: {#gstNumber#}<br>GST Address:<div class=container>{#gstAddress#}<div><br>PAN Number: {#pan#}</div><div class=letter-footer><p>Yours Sincerely,<div class=signature><p>Official Signatory<p>Designation<p>Address</div></div></div>'
  , T1.id
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);