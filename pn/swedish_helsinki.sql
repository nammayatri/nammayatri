
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'FIRST_RIDE_EVENT', 'FIRST_RIDE_EVENT', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Första paketleveransen', 'Grattis! Du har gjort din första paketleverans med oss.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByMerchant', 'Söker efter en ny förare!', 'Resan för {#bookingStartTime#} har avbokats. Vänta medan vi hittar en ny förare åt dig.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FCM_CHAT_MESSAGE', 'FCM_CHAT_MESSAGE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Missat samtal: Åtgärd krävs', 'Din förare försökte nå dig men kom inte fram. Ring tillbaka för att samordna upphämtningen.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FIRST_RIDE_EVENT', 'FIRST_RIDE_EVENT', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', '🎉 Grattis till din första resa med Lynx!', 'Ditt val stöttar förare direkt och gör verklig skillnad.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_QUOTE_INCOMING', 'DRIVER_QUOTE_INCOMING', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Nya förarerbjudanden på ingång!', 'Det finns nya förarerbjudanden! Se appen för detaljer.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ASSIGNMENT', 'DRIVER_ASSIGNMENT', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Förare tilldelad!', '{#driverName#} kommer att vara din förare på denna resa.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_STARTED', 'TRIP_STARTED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Din {#serviceTierName#}-resa har börjat!', 'Din {#serviceTierName#}-resa med {#driverName#} har börjat. Trevlig resa!', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'EXPIRED_CASE', 'EXPIRED_CASE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Resan har löpt ut!', 'Din resa har löpt ut då du inte bekräftade något erbjudande. Boka igen för att fortsätta.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'REGISTRATION_APPROVED', 'REGISTRATION_APPROVED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Registrering slutförd!', 'Välkommen till Lynx. Klicka här för att boka din första resa med oss.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByUser', 'Resa avbokad!', 'Du har avbokat din resa för {#bookingStartTime#}. Se appen för detaljer.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByMerchant', 'Resa avbokad!', '{#orgName#} var tvungna att avboka resan för {#bookingStartTime#}. Boka igen för att få en ny resa.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByDriver', 'Resa avbokad!', 'Föraren var tvungen att avboka resan för {#bookingStartTime#}. Boka igen för att få en ny resa.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByAllocator', 'Resa avbokad!', 'Resan för {#bookingStartTime#} avbokades då vi inte hittade någon förare. Boka igen för att få en ny resa.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByApplication', 'Resa avbokad!', 'Tyvärr avbokades din resa för {#bookingStartTime#}. Försök boka igen.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByUser', 'Resa avbokad!', 'Du har avbokat din resa för {#bookingStartTime#}. Se appen för detaljer.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByMerchant', 'Resa ej tillgänglig!', '{#orgName#} var tvungna att avboka resan för {#bookingStartTime#}. Boka igen för att få en ny resa.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByDriver', 'Resa ej tillgänglig!', 'Tyvärr kunde vi inte hitta någon förare för din resa {#bookingStartTime#}. Försök boka igen.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByAllocator', 'Resa ej tillgänglig!', 'Resan för {#bookingStartTime#} avbokades då vi inte hittade någon förare. Boka igen för att få en ny resa.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByApplication', 'Resa ej tillgänglig!', 'Tyvärr kunde vi inte hitta någon förare för din resa {#bookingStartTime#}. Försök boka igen.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'BOOKING_REALLOCATED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Resa avbokad! Vi söker en ny förare', 'Föraren var tvungen att avboka resan för {#bookingStartTime#}. Vänta medan vi hittar en ny förare.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByUser', 'Söker efter en ny förare!', 'Du har avbokat din resa för {#bookingStartTime#}. Vänta medan vi hittar en ny förare åt dig.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByDriver', 'Söker efter en ny förare!', 'Föraren har avbokat resan för {#bookingStartTime#}. Vänta medan vi hittar en ny förare åt dig.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByAllocator', 'Söker efter en ny förare!', 'Resan för {#bookingStartTime#} har avbokats. Vänta medan vi hittar en ny förare åt dig.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByApplication', 'Söker efter en ny förare!', 'Tyvärr avbokades din resa för {#bookingStartTime#}. Vänta medan vi hittar en ny förare åt dig.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'QUOTE_RECEIVED', 'QUOTE_RECEIVED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Prisuppgift mottagen!', 'Ny prisuppgift mottagen: {#quoteFareEstimate#}', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ON_THE_WAY', 'DRIVER_ON_THE_WAY', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Föraren är på väg!', 'Föraren är på väg', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_HAS_REACHED', 'DRIVER_HAS_REACHED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Föraren har kommit fram!', 'Använd kod {#otp#} för att verifiera resan med fordon {#vehicleNumber#}', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_REACHING', 'DRIVER_REACHING', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Föraren anländer nu!', 'Din förare anländer nu! Vänligen var på upphämtningsplatsen.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'SAFETY_ALERT_DEVIATION', 'SAFETY_ALERT_DEVIATION', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Är allt okej?', 'Vi märkte att din resa tar en annan rutt. Känner du dig trygg?', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_BIRTHDAY', 'DRIVER_BIRTHDAY', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Förarens födelsedag!', 'Idag fyller din förare {#driverName#} år. En gratulation skulle göra deras dag!', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FOLLOW_RIDE', 'FOLLOW_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Följ resan', '{#name#} vill att du följer deras resa', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'STOP_REACHED', 'STOP_REACHED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Stopp nått!', '{#driverName#} har nått stoppet. Du kan lägga till ett nytt stopp!', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'TICKET_CANCELLED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', '{#ticketBookingCategoryName#}-tjänst avbokad', 'Tyvärr har biljettbokning {#ticketBookingId#} ({#ticketBookingCategoryName#}) avbokats och återbetalas. Se appen för detaljer.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_UPDATED', 'TRIP_UPDATED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Destination och pris uppdaterat', 'Din ändringsbegäran godkändes av föraren!', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_QUOTE_INCOMING', 'DRIVER_QUOTE_INCOMING', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Nya leveranserbjudanden!', 'Det finns nya förarerbjudanden för din paketleverans. Se appen för detaljer.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ASSIGNMENT', 'DRIVER_ASSIGNMENT', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Förare tilldelad ditt paket!', '{#driverName#} kommer att hantera leveransen av ditt paket.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_STARTED', 'TRIP_STARTED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Ditt paket är på väg!', 'Ditt paket med {#driverName#} är nu på väg. Spåra det live i appen!', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_FINISHED', 'TRIP_FINISHED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Paket levererat!', 'Ditt paket har levererats av {#driverName#}. Totalt pris {#totalFare#}', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'EXPIRED_CASE', 'EXPIRED_CASE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Leveransförfrågan utlöpt!', 'Din leveransförfrågan löpte ut då inga erbjudanden bekräftades. Boka igen för att fortsätta.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByUser', 'Paketleverans avbokad', 'Du avbokade paketleveransen för {#bookingStartTime#}. Se appen för detaljer.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByMerchant', 'Paketleverans avbokad!', '{#orgName#} var tvungna att avboka paketleveransen för {#bookingStartTime#}. Boka ny leverans.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByDriver', 'Paketleverans avbokad!', 'Föraren var tvungen att avboka paketleveransen för {#bookingStartTime#}. Boka ny leverans.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByAllocator', 'Paketleverans avbokad!', 'Paketleveransen för {#bookingStartTime#} avbokades då vi inte hittade någon förare. Boka ny leverans.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByApplication', 'Paketleverans avbokad!', 'Tyvärr avbokades din paketleverans för {#bookingStartTime#}. Försök boka igen.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByUser', 'Paketleverans avbokad!', 'Du avbokade paketleveransen för {#bookingStartTime#}. Se appen för detaljer.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByMerchant', 'Paketleverans avbokad!', '{#orgName#} var tvungna att avboka paketleveransen för {#bookingStartTime#}. Boka ny leverans.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByDriver', 'Paketleverans avbokad!', 'Tyvärr kunde vi inte hitta någon förare för din paketleverans {#bookingStartTime#}. Försök boka igen.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByAllocator', 'Paketleverans avbokad!', 'Paketleveransen för {#bookingStartTime#} avbokades då vi inte hittade någon förare. Boka ny leverans.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByApplication', 'Paketleverans avbokad!', 'Tyvärr kunde vi inte hitta någon förare för din paketleverans {#bookingStartTime#}. Försök boka igen.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'BOOKING_REALLOCATED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Omfördelar din paketleverans!', 'Föraren var tvungen att avboka paketleveransen för {#bookingStartTime#}. Vänta medan vi hittar en ny förare.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByUser', 'Söker efter en ny förare!', 'Du har avbokat din paketleverans för {#bookingStartTime#}. Vänta medan vi hittar en ny förare.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByMerchant', 'Söker efter en ny förare!', 'Paketleveransen för {#bookingStartTime#} har avbokats. Vänta medan vi hittar en ny förare.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByDriver', 'Söker efter en ny förare!', 'Föraren har avbokat paketleveransen för {#bookingStartTime#}. Vänta medan vi hittar en ny förare.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByAllocator', 'Söker efter en ny förare!', 'Paketleveransen för {#bookingStartTime#} har avbokats. Vänta medan vi hittar en ny förare.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByApplication', 'Söker efter en ny förare!', 'Tyvärr avbokades din paketleverans för {#bookingStartTime#}. Vänta medan vi hittar en ny förare.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'QUOTE_RECEIVED', 'QUOTE_RECEIVED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Leveranspris mottaget!', 'Du har fått en ny prisuppgift för paketleverans: {#quoteFareEstimate#}', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ON_THE_WAY', 'DRIVER_ON_THE_WAY', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Föraren är på väg!', 'Föraren är på väg till upphämtningsplatsen för ditt paket.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_HAS_REACHED', 'DRIVER_HAS_REACHED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Föraren vid upphämtning!', 'Använd kod {#otp#} för att verifiera upphämtning med fordon {#vehicleNumber#}', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_HAS_REACHED_DESTINATION', 'DRIVER_HAS_REACHED_DESTINATION', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Föraren framme vid målet!', 'Föraren har nått paketets avlämningsplats.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_REACHING', 'DRIVER_REACHING', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Föraren anländer nu!', 'Din förare är snart vid upphämtningsplatsen. Var redo.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'SAFETY_ALERT_DEVIATION', 'SAFETY_ALERT_DEVIATION', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Säkerhetsvarning för paket!', 'Paketets leveransrutt har ändrats oväntat. Se appen för detaljer.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_BIRTHDAY', 'DRIVER_BIRTHDAY', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Förarens födelsedag!', 'Idag fyller din förare {#driverName#} år. Gratta dem gärna när de lämnar paketet!', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'FOLLOW_RIDE', 'FOLLOW_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Följ paketleverans', '{#name#} vill att du följer deras paketleverans', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'FILE_UPLOADED', 'PARCEL_IMAGE_UPLOADED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Bild på paket uppladdad', 'Föraren har laddat upp en bild på paketet. Öppna appen för att se den.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'SCHEDULED_RIDE_NOTIFICATION', 'RIDE_START_REMINDER', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', '{#isRentalOrIntercity#} Bokningspåminnelse', 'Resan startar kl. {#rideStartTime#}. Detaljer kommer strax.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'SCHEDULED_RIDE_NOTIFICATION', 'RIDE_START_END_OTP', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Startkod för resa', 'Kära kund, din startkod är {#rideStartOtp#}. Föraren anländer 15 minuter före starttiden.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'SAFETY_ALERT_RIDE_STOPPAGE', 'SAFETY_ALERT_RIDE_STOPPAGE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Är allt okej?', 'Vi märkte att din resa tar en annan rutt. Känner du dig trygg?', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD', 'REFERRAL_REWARD_ADD_VPA', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', '💸 Ka-ching! Du har precis tjänat en belöning', 'Din väns första resa är klar! Lägg till betalningsuppgifter för att få din belöning.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD', 'REFERRAL_REWARD', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', '💸 Ka-ching! Du har precis tjänat en belöning', 'Din väns första resa är klar! Värva fler vänner och tjäna pengar för varje ny passagerare.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD', 'REFERRED_BY_REWARD_ADD_VPA', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', '💸 Ka-ching! Du har precis tjänat en belöning', 'Lägg till betalningsuppgifter för att få belöningen. Tack för att du använder Lynx!', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD', 'REFERRAL_BONUS_EARNED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', '💸 Ka-ching! Du har precis tjänat en belöning', 'Din värvningsbelöning har satts in på ditt konto. Tack för att du använder Lynx!', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD', 'REFERRED_BY_REWARD', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', '💸 Ka-ching! Du har precis tjänat en belöning', 'Tack för att du använder Lynx och stöttar våra förare!', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'ACCOUNT_DELETED', 'ACCOUNT_DELETED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Konto raderat!', 'Ditt konto har raderats framgångsrikt.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'ACCOUNT_DELETED', 'ACCOUNT_DELETED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Konto raderat!', 'Ditt konto har raderats framgångsrikt.', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_FINISHED', 'TRIP_FINISHED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Resa avslutad!', 'Hoppas du hade en trevlig resa med {#driverName#}. Totalt pris {#totalFare#}. Kontrollera att du fått med dig allt!', 'SWEDISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);
