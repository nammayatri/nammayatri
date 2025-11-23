
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'FIRST_RIDE_EVENT', 'FIRST_RIDE_EVENT', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Eerste Pakketbezorging', 'Gefeliciteerd! Je hebt je eerste pakket bezorgd.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByMerchant', 'Nieuwe chauffeur gezocht!', 'De rit van {#bookingStartTime#} is geannuleerd. We zoeken een nieuwe chauffeur.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FCM_CHAT_MESSAGE', 'FCM_CHAT_MESSAGE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Gemiste Oproep: Actie Vereist', 'Uw chauffeur probeerde u te bellen. Bel terug om de ophaaltijd af te stemmen.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FIRST_RIDE_EVENT', 'FIRST_RIDE_EVENT', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', '🎉 Gefeliciteerd met je 1e rit met Bridge!', 'Jouw keuze steunt chauffeurs direct en maakt echt een verschil.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_QUOTE_INCOMING', 'DRIVER_QUOTE_INCOMING', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Nieuwe chauffeursaanbiedingen!', 'Er zijn nieuwe chauffeursaanbiedingen! Bekijk de app voor details.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ASSIGNMENT', 'DRIVER_ASSIGNMENT', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Chauffeur toegewezen!', '{#driverName#} wordt uw chauffeur voor deze rit.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_STARTED', 'TRIP_STARTED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Je {#serviceTierName#}-rit is begonnen!', 'Je {#serviceTierName#}-rit met {#driverName#} is begonnen. Geniet van de rit!', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'EXPIRED_CASE', 'EXPIRED_CASE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Rit verlopen!', 'Je rit is verlopen omdat je geen aanbod hebt bevestigd. Boek opnieuw.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'REGISTRATION_APPROVED', 'REGISTRATION_APPROVED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Registratie voltooid!', 'Welkom bij Yatri. Klik hier om je eerste rit te boeken.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByUser', 'Rit geannuleerd!', 'Je hebt je rit voor {#bookingStartTime#} geannuleerd. Bekijk de app.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByMerchant', 'Rit geannuleerd!', 'Agentschap "{#orgName#}" moest de rit voor {#bookingStartTime#} annuleren. Boek alstublieft opnieuw voor een andere rit.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByDriver', 'Rit geannuleerd!', 'De chauffeur moest de rit annuleren. Boek alstublieft opnieuw.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByAllocator', 'Rit geannuleerd!', 'De rit is geannuleerd, we konden geen chauffeur vinden. Boek alstublieft opnieuw.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByApplication', 'Rit geannuleerd!', 'Sorry, je rit voor {#bookingStartTime#} is geannuleerd. Probeer opnieuw te boeken.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByUser', 'Rit geannuleerd!', 'Je hebt je rit voor {#bookingStartTime#} geannuleerd. Bekijk de app.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByMerchant', 'Rit niet beschikbaar!', 'Agentschap "{#orgName#}" moest de rit voor {#bookingStartTime#} annuleren. Boek alstublieft opnieuw voor een andere rit.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByDriver', 'Rit niet beschikbaar!', 'Sorry, we konden geen chauffeur vinden voor uw rit om {#bookingStartTime#}. Probeer opnieuw.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByAllocator', 'Rit niet beschikbaar!', 'De rit is geannuleerd, we konden geen chauffeur vinden. Boek alstublieft opnieuw.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByApplication', 'Rit niet beschikbaar!', 'Sorry, we konden geen chauffeur vinden voor uw rit om {#bookingStartTime#}. Probeer opnieuw.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'BOOKING_REALLOCATED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Rit geannuleerd! We zoeken een nieuwe chauffeur.', 'De chauffeur heeft de rit geannuleerd. We wijzen een andere chauffeur toe.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByUser', 'Nieuwe chauffeur gezocht!', 'Je hebt je rit geannuleerd. We zoeken een nieuwe chauffeur voor je.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByDriver', 'Nieuwe chauffeur gezocht!', 'De chauffeur heeft de rit geannuleerd. We zoeken een nieuwe chauffeur.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByAllocator', 'Nieuwe chauffeur gezocht!', 'De rit van {#bookingStartTime#} is geannuleerd. We zoeken een nieuwe chauffeur.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByApplication', 'Nieuwe chauffeur gezocht!', 'Sorry, uw rit van {#bookingStartTime#} is geannuleerd. We zoeken een nieuwe chauffeur voor u.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'QUOTE_RECEIVED', 'QUOTE_RECEIVED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Offerte ontvangen!', 'Nieuwe offerte ontvangen: {#quoteFareEstimate#}', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ON_THE_WAY', 'DRIVER_ON_THE_WAY', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Chauffeur Onderweg!', 'Chauffeur is onderweg', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_HAS_REACHED', 'DRIVER_HAS_REACHED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Chauffeur is Aangekomen!', 'Gebruik OTP {#otp#} om de rit te verifiëren met Voertuig Nr. {#vehicleNumber#}.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_REACHING', 'DRIVER_REACHING', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Chauffeur Komt Nu Aan!', 'Je chauffeur komt nu aan! Wees op de ophaallocatie.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'SAFETY_ALERT_DEVIATION', 'SAFETY_ALERT_DEVIATION', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Alles in orde?', 'We zien dat uw rit een andere route volgt. Voelt u zich veilig?', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_BIRTHDAY', 'DRIVER_BIRTHDAY', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Verjaardag Chauffeur!', 'Uw chauffeur {#driverName#} is vandaag jarig, uw wensen maken hun dag speciaal!', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FOLLOW_RIDE', 'FOLLOW_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Volg Rit', '{#name#} wil dat je hun rit volgt', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'STOP_REACHED', 'STOP_REACHED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Halte Bereikt!', '{#driverName#} heeft de halte bereikt. Voeg een nieuwe halte toe!', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'TICKET_CANCELLED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', '{#ticketBookingCategoryName#} Ticket Geannuleerd', 'Sorry, Ticket {#ticketBookingId#} ({#ticketBookingCategoryName#}) is geannuleerd en wordt terugbetaald. Check de app.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_UPDATED', 'TRIP_UPDATED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Bestemming & Prijs Bijgewerkt', 'Je wijzigingsverzoek is geaccepteerd door je chauffeur!', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_QUOTE_INCOMING', 'DRIVER_QUOTE_INCOMING', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Nieuwe bezorgaanbiedingen!', 'Nieuwe aanbiedingen voor het bezorgen van je pakket. Check de app.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ASSIGNMENT', 'DRIVER_ASSIGNMENT', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Chauffeur voor je pakket!', '{#driverName#} verzorgt de bezorging van je pakket.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_STARTED', 'TRIP_STARTED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Je pakket is onderweg!', 'Je pakket met {#driverName#} is onderweg. Volg het live in de app!', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_FINISHED', 'TRIP_FINISHED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Pakket bezorgd!', 'Je pakket is bezorgd door {#driverName#}. Totaalbedrag {#totalFare#}', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'EXPIRED_CASE', 'EXPIRED_CASE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Bezorgverzoek verlopen!', 'Je bezorgverzoek is verlopen, geen aanbod bevestigd. Boek opnieuw.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByUser', 'Pakketbezorging geannuleerd!', 'Je hebt de pakketbezorging voor {#bookingStartTime#} geannuleerd. Check de app.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByMerchant', 'Pakketbezorging geannuleerd!', 'Agentschap "{#orgName#}" moest de pakketbezorging voor {#bookingStartTime#} annuleren. Boek alstublieft opnieuw voor een andere rit.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByDriver', 'Pakketbezorging geannuleerd!', 'De chauffeur moest de bezorging annuleren. Boek opnieuw.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByAllocator', 'Pakketbezorging geannuleerd!', 'Pakketbezorging geannuleerd, geen chauffeur gevonden. Boek opnieuw.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByApplication', 'Pakketbezorging geannuleerd!', 'Sorry, je pakketbezorging is geannuleerd. Probeer opnieuw te boeken.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByUser', 'Pakketbezorging geannuleerd!', 'Je hebt de pakketbezorging voor {#bookingStartTime#} geannuleerd. Check de app.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByMerchant', 'Pakketbezorging geannuleerd!', 'Agentschap "{#orgName#}" moest de pakketbezorging voor {#bookingStartTime#} annuleren. Boek alstublieft opnieuw voor een andere rit.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByDriver', 'Pakketbezorging geannuleerd!', 'Sorry, geen chauffeur gevonden voor je pakket. Probeer opnieuw te boeken.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByAllocator', 'Pakketbezorging geannuleerd!', 'Pakketbezorging geannuleerd, geen chauffeur gevonden. Boek opnieuw.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByApplication', 'Pakketbezorging geannuleerd!', 'Sorry, geen chauffeur gevonden voor je pakket. Probeer opnieuw te boeken.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'BOOKING_REALLOCATED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Nieuwe bezorger wordt gezocht!', 'De chauffeur heeft de bezorging geannuleerd. We zoeken een nieuwe chauffeur.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByUser', 'Nieuwe chauffeur gezocht!', 'Je hebt je bezorging geannuleerd. We zoeken een nieuwe chauffeur.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByMerchant', 'Nieuwe chauffeur gezocht!', 'De pakketbezorging voor {#bookingStartTime#} is geannuleerd. We zoeken een nieuwe chauffeur.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByDriver', 'Nieuwe chauffeur gezocht!', 'De chauffeur heeft de bezorging geannuleerd. We zoeken een nieuwe chauffeur.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByAllocator', 'Nieuwe chauffeur gezocht!', 'De pakketbezorging voor {#bookingStartTime#} is geannuleerd. We zoeken een nieuwe chauffeur.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByApplication', 'Nieuwe chauffeur gezocht!', 'Sorry, je bezorging is geannuleerd. We zoeken een nieuwe chauffeur.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'QUOTE_RECEIVED', 'QUOTE_RECEIVED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Offerte bezorging!', 'Nieuwe offerte voor je pakket: {#quoteFareEstimate#}', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ON_THE_WAY', 'DRIVER_ON_THE_WAY', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Chauffeur Onderweg!', 'Chauffeur is onderweg naar de ophaallocatie van je pakket.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_HAS_REACHED', 'DRIVER_HAS_REACHED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Chauffeur op ophaallocatie!', 'Gebruik OTP {#otp#} voor verificatie bij Voertuig Nr. {#vehicleNumber#}.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_HAS_REACHED_DESTINATION', 'DRIVER_HAS_REACHED_DESTINATION', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Chauffeur op bestemming!', 'Chauffeur heeft de afleverlocatie bereikt.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_REACHING', 'DRIVER_REACHING', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Chauffeur Komt Nu Aan!', 'Je chauffeur is bijna op de ophaallocatie. Wees er klaar voor.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'SAFETY_ALERT_DEVIATION', 'SAFETY_ALERT_DEVIATION', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Veiligheidsalert pakket!', 'De route van je pakket is onverwacht gewijzigd. Check de app.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_BIRTHDAY', 'DRIVER_BIRTHDAY', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Verjaardag Chauffeur!', 'Je chauffeur {#driverName#} is vandaag jarig, wens ze een fijne dag bij de bezorging!', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'FOLLOW_RIDE', 'FOLLOW_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Volg Pakketbezorging', '{#name#} wil dat je hun pakketbezorging volgt', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'FILE_UPLOADED', 'PARCEL_IMAGE_UPLOADED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Pakketfoto geüpload', 'Je pakketfoto is geüpload door de chauffeur. Open de app om te zien.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'SCHEDULED_RIDE_NOTIFICATION', 'RIDE_START_REMINDER', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', '{#isRentalOrIntercity#} Herinnering', 'Rit start om {#rideStartTime#}. Details volgen spoedig.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'SCHEDULED_RIDE_NOTIFICATION', 'RIDE_START_END_OTP', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Start-OTP Rit', 'Beste gebruiker, uw start-OTP is {#rideStartOtp#}. De chauffeur is er 15 min. van tevoren.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'SAFETY_ALERT_RIDE_STOPPAGE', 'SAFETY_ALERT_RIDE_STOPPAGE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Alles in orde?', 'We zien dat uw chauffeur stilstaat. Voelt u zich veilig tijdens de rit?', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD', 'REFERRAL_REWARD_ADD_VPA', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', '💸 Tjing-tjing! Beloning verdiend', 'De eerste rit van je vriend is voltooid! Voeg UPI ID toe en verdien je beloning.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD', 'REFERRAL_REWARD', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', '💸 Tjing-tjing! Beloning verdiend', 'De eerste rit van je vriend is voltooid! Blijf doorverwijzen en verdien meer.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD', 'REFERRED_BY_REWARD_ADD_VPA', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', '💸 Tjing-tjing! Beloning verdiend', 'Voeg UPI ID toe en verdien de beloning. Bedankt voor het gebruiken van Bridge, blijf onze chauffeurs steunen!', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD', 'REFERRAL_BONUS_EARNED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', '💸 Tjing-tjing! Beloning verdiend', 'Je doorverwijsbeloning is bijgeschreven. Bedankt voor het gebruiken van Bridge!!', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD', 'REFERRED_BY_REWARD', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', '💸 Tjing-tjing! Beloning verdiend', 'Bedankt voor het gebruiken van Bridge, blijf onze chauffeurs steunen!', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'ACCOUNT_DELETED', 'ACCOUNT_DELETED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Account Verwijderd!', 'Je account is succesvol verwijderd.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'ACCOUNT_DELETED', 'ACCOUNT_DELETED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Account Verwijderd!', 'Je account is succesvol verwijderd.', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_FINISHED', 'TRIP_FINISHED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Rit voltooid!', 'Hopelijk heb je genoten van je rit met {#driverName#}. Totaalbedrag {#totalFare#}. Vergeet je spullen niet!', 'DUTCH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);
