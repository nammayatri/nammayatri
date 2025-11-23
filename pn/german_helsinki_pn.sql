INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'FIRST_RIDE_EVENT','FIRST_RIDE_EVENT','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Erste Paketzustellung','Glückwunsch! Sie haben Ihr erstes Paket zugestellt.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByMerchant',
  'Suchen neuen Fahrer!','Die Fahrt um {#bookingStartTime#} wurde storniert. Wir suchen einen neuen Fahrer.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'FCM_CHAT_MESSAGE','FCM_CHAT_MESSAGE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Verpasster Anruf: Handeln nötig','Ihr Fahrer hat versucht, Sie zu erreichen. Bitte rufen Sie zurück, um die Abholung zu koordinieren.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'FIRST_RIDE_EVENT','FIRST_RIDE_EVENT',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  '🎉 Glückwunsch zur 1. Fahrt mit Bridge!','Deine Wahl unterstützt Fahrer direkt und macht einen echten Unterschied.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_QUOTE_INCOMING','DRIVER_QUOTE_INCOMING',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Neue Fahrerangebote!','Neue Fahrerangebote! Details in der App.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ASSIGNMENT','DRIVER_ASSIGNMENT',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Fahrer zugewiesen!','{#driverName#} wird Ihr Fahrer für diese Fahrt sein.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_STARTED','TRIP_STARTED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Deine {#serviceTierName#}-Fahrt hat begonnen!','Deine {#serviceTierName#}-Fahrt mit {#driverName#} hat begonnen. Gute Fahrt!','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'EXPIRED_CASE','EXPIRED_CASE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Fahrt abgelaufen!','Deine Fahrt ist abgelaufen, da du kein Angebot bestätigt hast. Bitte buche erneut.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REGISTRATION_APPROVED','REGISTRATION_APPROVED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Registrierung abgeschlossen!','Willkommen bei Yatri. Klicke hier, um deine erste Fahrt zu buchen.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Cancellations with ride
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByUser',
  'Fahrt storniert!','Du hast deine Fahrt für {#bookingStartTime#} storniert. Details in der App.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByMerchant',
  'Fahrt storniert!','Die Agentur "{#orgName#}" musste die Fahrt für {#bookingStartTime#} stornieren. Bitte buchen Sie erneut, um eine andere Fahrt zu erhalten.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByDriver',
  'Fahrt storniert!','Der Fahrer musste die Fahrt stornieren. Bitte buche erneut.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByAllocator',
  'Fahrt storniert!','Fahrt storniert, kein Fahrer gefunden. Bitte buche erneut.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByApplication',
  'Fahrt storniert!','Entschuldigung, deine Fahrt für {#bookingStartTime#} wurde storniert. Bitte versuche es erneut.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Cancellations no ride
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByUser',
  'Fahrt storniert!','Du hast deine Fahrt für {#bookingStartTime#} storniert. Details in der App.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByMerchant',
  'Fahrt nicht verfügbar!','Die Agentur "{#orgName#}" musste die Fahrt für {#bookingStartTime#} stornieren. Bitte buchen Sie erneut, um eine andere Fahrt zu erhalten.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByDriver',
  'Fahrt nicht verfügbar!','Leider konnten wir keinen Fahrer für Ihre Fahrt um {#bookingStartTime#} finden. Bitte versuchen Sie es erneut.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByAllocator',
  'Fahrt nicht verfügbar!','Fahrt storniert, kein Fahrer gefunden. Bitte buche erneut.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByApplication',
  'Fahrt nicht verfügbar!','Leider konnten wir keinen Fahrer für Ihre Fahrt um {#bookingStartTime#} finden. Bitte versuchen Sie es erneut.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Reallocate general
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','BOOKING_REALLOCATED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Fahrt storniert! Wir weisen einen neuen Fahrer zu.','Der Fahrer hat die Fahrt storniert. Wir weisen einen neuen Fahrer zu.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByUser',
  'Suchen neuen Fahrer!','Du hast deine Fahrt storniert. Wir suchen einen neuen Fahrer für dich.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByDriver',
  'Suchen neuen Fahrer!','Der Fahrer hat die Fahrt storniert. Wir suchen einen neuen Fahrer.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByAllocator',
  'Suchen neuen Fahrer!','Die Fahrt um {#bookingStartTime#} wurde storniert. Wir suchen einen neuen Fahrer.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByApplication',
  'Suchen neuen Fahrer!','Entschuldigung, Ihre Fahrt für {#bookingStartTime#} wurde storniert. Wir suchen einen neuen Fahrer für Sie.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Quote received
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'QUOTE_RECEIVED','QUOTE_RECEIVED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Angebot erhalten!','Neues Angebot erhalten: {#quoteFareEstimate#}','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Driver statuses
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ON_THE_WAY','DRIVER_ON_THE_WAY',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Fahrer unterwegs!','Fahrer ist unterwegs','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_HAS_REACHED','DRIVER_HAS_REACHED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Fahrer ist da!','Nutze OTP {#otp#}, um die Fahrt mit Fahrzeug-Nr. {#vehicleNumber#} zu bestätigen.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_REACHING','DRIVER_REACHING',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Fahrer kommt jetzt an!','Dein Fahrer kommt jetzt an! Bitte sei am Abholort.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Safety deviation
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'SAFETY_ALERT_DEVIATION','SAFETY_ALERT_DEVIATION',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Alles okay?','Wir bemerken, dass deine Fahrt von der Route abweicht. Fühlst du dich sicher?','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Driver birthday
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_BIRTHDAY','DRIVER_BIRTHDAY',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Geburtstag des Fahrers!','Ihr Fahrer {#driverName#} hat heute Geburtstag, Ihre Wünsche machen den Tag besonders!','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Follow ride
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FOLLOW_RIDE','FOLLOW_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Fahrt verfolgen','{#name#} möchte, dass du die Fahrt verfolgst','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Stop reached
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'STOP_REACHED','STOP_REACHED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Halt erreicht!','{#driverName#} hat den Halt erreicht. Du kannst einen weiteren Halt hinzufügen!','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Ticket cancelled
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','TICKET_CANCELLED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  '{#ticketBookingCategoryName#} Ticket Storniert','Ticket {#ticketBookingId#} ({#ticketBookingCategoryName#}) storniert, Erstattung folgt. Details in der App.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Trip updated
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_UPDATED','TRIP_UPDATED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Ziel & Fahrpreis Aktualisiert','Deine Änderungsanfrage wurde vom Fahrer akzeptiert!','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Delivery: Dynamic Offer Parcel Delivery variants
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_QUOTE_INCOMING','DRIVER_QUOTE_INCOMING','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Neue Lieferangebote!','Neue Angebote für deine Paketzustellung. Details in der App.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ASSIGNMENT','DRIVER_ASSIGNMENT','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Fahrer für dein Paket!','{#driverName#} wird dein Paket zustellen.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_STARTED','TRIP_STARTED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Dein Paket ist unterwegs!','Dein Paket mit {#driverName#} ist unterwegs. Verfolge es live in der App!','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_FINISHED','TRIP_FINISHED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Paket zugestellt!','Dein Paket wurde von {#driverName#} zugestellt. Gesamtpreis {#totalFare#}','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'EXPIRED_CASE','EXPIRED_CASE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Lieferanfrage abgelaufen!','Deine Lieferanfrage ist abgelaufen, da kein Angebot bestätigt wurde. Buche erneut.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Delivery cancellations with ride
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByUser',
  'Paketzustellung storniert!','Du hast die Paketzustellung für {#bookingStartTime#} storniert. Details in der App.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByMerchant',
  'Paketzustellung storniert!','Die Agentur "{#orgName#}" musste die Paketzustellung für {#bookingStartTime#} stornieren. Bitte buchen Sie erneut, um eine andere Fahrt zu erhalten.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByDriver',
  'Paketzustellung storniert!','Der Fahrer musste die Zustellung stornieren. Bitte buche erneut.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByAllocator',
  'Paketzustellung storniert!','Zustellung storniert, kein Fahrer gefunden. Bitte buche erneut.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByApplication',
  'Paketzustellung storniert!','Entschuldigung, deine Paketzustellung wurde storniert. Bitte versuche es erneut.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Delivery cancellations no ride
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByUser',
  'Paketzustellung storniert!','Du hast die Paketzustellung für {#bookingStartTime#} storniert. Details in der App.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByMerchant',
  'Paketzustellung storniert!','Die Agentur "{#orgName#}" musste die Paketzustellung für {#bookingStartTime#} stornieren. Bitte buchen Sie erneut, um eine andere Fahrt zu erhalten.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByDriver',
  'Paketzustellung storniert!','Leider keinen Fahrer für dein Paket gefunden. Bitte versuche es erneut.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByAllocator',
  'Paketzustellung storniert!','Zustellung storniert, kein Fahrer gefunden. Bitte buche erneut.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByApplication',
  'Paketzustellung storniert!','Entschuldigung, kein Fahrer für Ihr Paket gefunden. Bitte versuchen Sie es erneut.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Delivery reallocate
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','BOOKING_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Neue Zustellung wird gesucht!','Der Fahrer hat die Zustellung storniert. Wir suchen einen neuen Fahrer.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Delivery EST/QUOTE reallocated
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByUser',
  'Suchen neuen Fahrer!','Du hast deine Zustellung storniert. Wir suchen einen neuen Fahrer.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByMerchant',
  'Suchen neuen Fahrer!','Die Paketzustellung für {#bookingStartTime#} ist storniert. Wir suchen einen neuen Fahrer.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByDriver',
  'Suchen neuen Fahrer!','Der Fahrer hat die Zustellung storniert. Wir suchen einen neuen Fahrer.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByAllocator',
  'Suchen neuen Fahrer!','Die Paketzustellung für {#bookingStartTime#} ist storniert. Wir suchen einen neuen Fahrer.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByApplication',
  'Suchen neuen Fahrer!','Entschuldigung, deine Zustellung wurde storniert. Wir suchen einen neuen Fahrer.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Delivery quote received
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'QUOTE_RECEIVED','QUOTE_RECEIVED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Lieferangebot erhalten!','Neues Angebot für dein Paket: {#quoteFareEstimate#}','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Delivery driver statuses
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ON_THE_WAY','DRIVER_ON_THE_WAY','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Fahrer unterwegs!','Fahrer ist auf dem Weg zum Abholort deines Pakets.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_HAS_REACHED','DRIVER_HAS_REACHED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Fahrer am Abholort!','Nutze OTP {#otp#} zur Bestätigung bei Fahrzeug-Nr. {#vehicleNumber#}.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_HAS_REACHED_DESTINATION','DRIVER_HAS_REACHED_DESTINATION','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Fahrer am Zielort!','Fahrer hat den Abgabeort erreicht.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_REACHING','DRIVER_REACHING','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Fahrer kommt jetzt an!','Dein Fahrer ist fast am Abholort. Bitte sei bereit.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Delivery safety
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'SAFETY_ALERT_DEVIATION','SAFETY_ALERT_DEVIATION','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Sicherheitsalarm Zustellung!','Die Route deines Pakets hat sich geändert. Details in der App.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Delivery driver birthday
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_BIRTHDAY','DRIVER_BIRTHDAY','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Geburtstag des Fahrers!','Dein Fahrer {#driverName#} hat heute Geburtstag, wünsche ihm alles Gute bei der Zustellung!','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Follow delivery
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FOLLOW_RIDE','FOLLOW_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Paketzustellung verfolgen','{#name#} möchte, dass du die Paketzustellung verfolgst','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Parcel image uploaded
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FILE_UPLOADED','PARCEL_IMAGE_UPLOADED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Paketbild hochgeladen','Dein Paketbild wurde vom Fahrer hochgeladen. Öffne die App, um es zu sehen.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Scheduled notifications
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'SCHEDULED_RIDE_NOTIFICATION','RIDE_START_REMINDER','{#isRentalOrIntercity#} Erinnerung','Fahrt beginnt um {#rideStartTime#}. Details folgen in Kürze.','GERMAN',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'SCHEDULED_RIDE_NOTIFICATION','RIDE_START_END_OTP','Fahrt-Start-OTP','Sehr geehrter Nutzer, Ihr Start-OTP lautet {#rideStartOtp#}. Der Fahrer ist 15 Min. vorher da.','GERMAN',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Safety stoppage
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'SAFETY_ALERT_RIDE_STOPPAGE','SAFETY_ALERT_RIDE_STOPPAGE','Alles okay?','Wir sehen, dass Ihr Fahrer eine Weile nicht gefahren ist. Fühlen Sie sich sicher?','GERMAN',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Payout/rewards
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD','REFERRAL_REWARD_ADD_VPA','💸 Kaching! Prämie erhalten','Die erste Fahrt deines Freundes ist abgeschlossen! Füge eine UPI-ID hinzu und erhalte deine Prämie.','GERMAN',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD','REFERRAL_REWARD','💸 Kaching! Prämie erhalten','Die erste Fahrt deines Freundes ist abgeschlossen! Werbe mehr Freunde und verdiene.','GERMAN',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD','REFERRED_BY_REWARD_ADD_VPA','💸 Kaching! Prämie erhalten','Füge eine UPI-ID hinzu und erhalte die Prämie. Danke, dass du Bridge nutzt. Unterstütze weiterhin unsere Fahrer!','GERMAN',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD','REFERRAL_BONUS_EARNED','💸 Kaching! Prämie erhalten','Deine Empfehlungsprämie wurde gutgeschrieben. Danke, dass du Bridge nutzt!!','GERMAN',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD','REFERRED_BY_REWARD','💸 Kaching! Prämie erhalten','Danke, dass du Bridge nutzt. Unterstütze weiterhin unsere Fahrer!','GERMAN',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Account deleted
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'ACCOUNT_DELETED','ACCOUNT_DELETED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Konto gelöscht!','Dein Konto wurde erfolgreich gelöscht.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'ACCOUNT_DELETED','ACCOUNT_DELETED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Konto gelöscht!','Dein Konto wurde erfolgreich gelöscht.','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Trip finished
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_FINISHED','TRIP_FINISHED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Fahrt beendet!','Wir hoffen, du hattest eine gute Fahrt mit {#driverName#}. Gesamtpreis {#totalFare#}. Vergiss deine Sachen nicht!','GERMAN',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);


