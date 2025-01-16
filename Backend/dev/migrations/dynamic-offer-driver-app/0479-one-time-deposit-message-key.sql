INSERT INTO atlas_driver_offer_bpp.merchant_message
(merchant_id, message_key, template_id, json_data, message, contains_url_button, merchant_operating_city_id)
SELECT '0t75b0b5-4e13-4905-990f-c9dr5db806fe', 'WHATSAPP_SEND_ONE_TIME_SECURITY_PAYMENT_LINK', '7099537', null, '*Namma Yatri Mahila Shakti Programme*
*ನಮ್ಮ ಯಾತ್ರಿ ಮಹಿಳಾ ಶಕ್ತಿ ಕಾರ್ಯಕ್ರಮ*

Please click on the link to deposit one-time refundable security.

ಒಂದು-ಬಾರಿ ವಾಪಸು ಮಾಡಬಹುದಾದ ಸೆಕ್ಯೂರಿಟಿ ಡೆಪಾಸಿಟ್ ಮಾಡಲು ಲಿಂಕ್ ಅನ್ನು ಕ್ಲಿಕ್ ಮಾಡಿ.
Amount / ಡೆಪಾಸಿಟ್ ಮೊತ್ತ : Rs. {#var1#}

Pay Now - {#var1#}', true, m.id
FROM atlas_driver_offer_bpp.merchant_operating_city as m;
