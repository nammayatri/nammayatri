
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'FIRST_RIDE_EVENT', 'FIRST_RIDE_EVENT', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Ensimmäinen pakettikuljetus', 'Onnittelut! Ensimmäinen pakettikuljetuksesi on tehty.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByMerchant', 'Etsitään uutta kuljettajaa!', 'Kyyti ajalle {#bookingStartTime#} on peruttu. Odota, etsimme sinulle uutta kuljettajaa.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FCM_CHAT_MESSAGE', 'FCM_CHAT_MESSAGE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Vastaamaton puhelu: Toimi heti', 'Kuljettajasi yritti tavoittaa sinua. Soita hänelle takaisin sopiaksesi noudosta.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FIRST_RIDE_EVENT', 'FIRST_RIDE_EVENT', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', '🎉 Onnittelut 1. matkasta Lynxillä!', 'Valintasi tukee kuljettajia suoraan ja sillä on todellista merkitystä.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_QUOTE_INCOMING', 'DRIVER_QUOTE_INCOMING', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Uusia kuljettajatarjouksia!', 'Uusia kuljettajatarjouksia saatavilla! Katso tiedot sovelluksesta.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ASSIGNMENT', 'DRIVER_ASSIGNMENT', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Kuljettaja nimetty!', '{#driverName#} on kuljettajasi tällä matkalla.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_STARTED', 'TRIP_STARTED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', '{#serviceTierName#} -matkasi on alkanut!', '{#serviceTierName#} -matkasi kuljettajan {#driverName#} kanssa on alkanut. Hyvää matkaa!', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'EXPIRED_CASE', 'EXPIRED_CASE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Kyytipyyntö vanhentui!', 'Kyytipyyntösi vanheni, koska et hyväksynyt tarjousta. Tilaa uudelleen jatkaaksesi.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'REGISTRATION_APPROVED', 'REGISTRATION_APPROVED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Rekisteröinti valmis!', 'Tervetuloa Lynxiin. Tilaa ensimmäinen kyytisi napauttamalla tästä.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByUser', 'Kyyti peruttu!', 'Olet perunut kyydin ajalle {#bookingStartTime#}. Katso tiedot sovelluksesta.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByMerchant', 'Kyyti peruttu!', '{#orgName#} joutui perumaan kyydin ajalle {#bookingStartTime#}. Tilaa uusi kyyti.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByDriver', 'Kyyti peruttu!', 'Kuljettaja joutui perumaan kyydin ajalle {#bookingStartTime#}. Tilaa uusi kyyti.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByAllocator', 'Kyyti peruttu!', 'Kyyti ajalle {#bookingStartTime#} peruttiin, koska kuljettajaa ei löytynyt. Tilaa uusi kyyti.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByApplication', 'Kyyti peruttu!', 'Pahoittelut, kyytisi ajalle {#bookingStartTime#} on peruttu. Tilaa uusi kyyti.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByUser', 'Kyyti peruttu!', 'Olet perunut kyydin ajalle {#bookingStartTime#}. Katso tiedot sovelluksesta.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByMerchant', 'Kyyti ei saatavilla!', '{#orgName#} joutui perumaan kyydin ajalle {#bookingStartTime#}. Tilaa uusi kyyti.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByDriver', 'Kyyti ei saatavilla!', 'Pahoittelut, emme löytäneet kuljettajaa ajalle {#bookingStartTime#}. Tilaa uusi kyyti.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByAllocator', 'Kyyti ei saatavilla!', 'Kyyti ajalle {#bookingStartTime#} peruttiin, koska kuljettajaa ei löytynyt. Tilaa uusi kyyti.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByApplication', 'Kyyti ei saatavilla!', 'Pahoittelut, emme löytäneet kuljettajaa ajalle {#bookingStartTime#}. Tilaa uusi kyyti.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'BOOKING_REALLOCATED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Kyyti peruttu! Etsitään uutta kuljettajaa', 'Kuljettaja joutui perumaan kyydin ajalle {#bookingStartTime#}. Odota, etsimme uutta kuljettajaa.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByUser', 'Etsitään uutta kuljettajaa!', 'Olet perunut kyydin ajalle {#bookingStartTime#}. Odota, etsimme sinulle uutta kuljettajaa.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByDriver', 'Etsitään uutta kuljettajaa!', 'Kuljettaja on perunut kyydin ajalle {#bookingStartTime#}. Odota, etsimme sinulle uutta kuljettajaa.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByAllocator', 'Etsitään uutta kuljettajaa!', 'Kyyti ajalle {#bookingStartTime#} on peruttu. Odota, etsimme sinulle uutta kuljettajaa.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByApplication', 'Etsitään uutta kuljettajaa!', 'Pahoittelut, kyytisi ajalle {#bookingStartTime#} on peruttu. Odota, etsimme sinulle uutta kuljettajaa.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'QUOTE_RECEIVED', 'QUOTE_RECEIVED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Tarjous saatu!', 'Uusi hintatarjous saatu: {#quoteFareEstimate#}', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ON_THE_WAY', 'DRIVER_ON_THE_WAY', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Kuljettaja on matkalla!', 'Kuljettaja on matkalla', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_HAS_REACHED', 'DRIVER_HAS_REACHED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Kuljettaja on saapunut!', 'Vahvista kyyti ajoneuvolle {#vehicleNumber#} koodilla {#otp#}', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_REACHING', 'DRIVER_REACHING', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Kuljettaja saapuu!', 'Kuljettajasi saapuu nyt! Olethan valmiina noutopaikalla.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'SAFETY_ALERT_DEVIATION', 'SAFETY_ALERT_DEVIATION', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Onko kaikki hyvin?', 'Huomasimme, että matkasi reitti on muuttunut. Oletko turvassa?', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_BIRTHDAY', 'DRIVER_BIRTHDAY', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Kuljettajan syntymäpäivä!', 'Tänään on kuljettajasi {#driverName#} syntymäpäivä. Onnittelut ilahduttaisivat häntä varmasti!', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FOLLOW_RIDE', 'FOLLOW_RIDE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Seuraa matkaa', '{#name#} haluaa sinun seuraavan matkaansa', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'STOP_REACHED', 'STOP_REACHED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Pysähdyspaikka saavutettu!', '{#driverName#} on saapunut pysähdyspaikalle. Voit lisätä uuden pysähdyksen!', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'TICKET_CANCELLED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', '{#ticketBookingCategoryName#} -palvelu peruttu', 'Pahoittelut, lippuvaraus {#ticketBookingId#} ({#ticketBookingCategoryName#}) on peruttu ja hyvitetään. Katso tiedot sovelluksesta.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_UPDATED', 'TRIP_UPDATED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Määränpää ja hinta päivitetty', 'Kuljettaja hyväksyi muutospyyntösi!', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_QUOTE_INCOMING', 'DRIVER_QUOTE_INCOMING', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Uusia kuljetustarjouksia!', 'Uusia tarjouksia pakettisi kuljetukselle. Katso tiedot sovelluksesta.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ASSIGNMENT', 'DRIVER_ASSIGNMENT', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Kuljettaja nimetty!', '{#driverName#} hoitaa pakettisi kuljetuksen.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_STARTED', 'TRIP_STARTED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Pakettisi on liikkeellä!', 'Pakettisi on matkalla kuljettajan {#driverName#} kyydissä. Seuraa lähetystä sovelluksessa!', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_FINISHED', 'TRIP_FINISHED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Paketti toimitettu!', 'Kuljettaja {#driverName#} on toimittanut pakettisi onnistuneesti. Hinta yhteensä {#totalFare#}', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'EXPIRED_CASE', 'EXPIRED_CASE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Kuljetuspyyntö vanhentui!', 'Kuljetuspyyntö vanheni, koska tarjouksia ei hyväksytty. Tilaa uudelleen jatkaaksesi.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByUser', 'Pakettikuljetus peruttu', 'Olet perunut pakettikuljetuksen ajalle {#bookingStartTime#}. Katso tiedot sovelluksesta.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByMerchant', 'Pakettikuljetus peruttu!', '{#orgName#} joutui perumaan pakettikuljetuksen ajalle {#bookingStartTime#}. Tilaa uusi kuljetus.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByDriver', 'Pakettikuljetus peruttu!', 'Kuljettaja joutui perumaan pakettikuljetuksen ajalle {#bookingStartTime#}. Tilaa uusi kuljetus.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByAllocator', 'Pakettikuljetus peruttu!', 'Pakettikuljetus ajalle {#bookingStartTime#} peruttiin, koska kuljettajaa ei löytynyt. Tilaa uusi kuljetus.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByApplication', 'Pakettikuljetus peruttu!', 'Pahoittelut, pakettikuljetuksesi ajalle {#bookingStartTime#} on peruttu. Tilaa uusi kuljetus.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByUser', 'Pakettikuljetus peruttu!', 'Olet perunut pakettikuljetuksen ajalle {#bookingStartTime#}. Katso tiedot sovelluksesta.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByMerchant', 'Pakettikuljetus peruttu!', '{#orgName#} joutui perumaan pakettikuljetuksen ajalle {#bookingStartTime#}. Tilaa uusi kuljetus.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByDriver', 'Pakettikuljetus peruttu!', 'Pahoittelut, emme löytäneet kuljettajaa pakettikuljetukselle ajalle {#bookingStartTime#}. Tilaa uusi kuljetus.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByAllocator', 'Pakettikuljetus peruttu!', 'Pakettikuljetus ajalle {#bookingStartTime#} peruttiin, koska kuljettajaa ei löytynyt. Tilaa uusi kuljetus.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT', 'BOOKING_CANCEL_WITH_NO_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByApplication', 'Pakettikuljetus peruttu!', 'Pahoittelut, emme löytäneet kuljettajaa pakettikuljetukselle ajalle {#bookingStartTime#}. Tilaa uusi kuljetus.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'BOOKING_REALLOCATED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Etsitään uutta kuljettajaa!', 'Kuljettaja joutui perumaan pakettikuljetuksen ajalle {#bookingStartTime#}. Odota, etsimme uutta kuljettajaa.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByUser', 'Etsitään uutta kuljettajaa!', 'Olet perunut pakettikuljetuksen ajalle {#bookingStartTime#}. Odota, etsimme uutta kuljettajaa.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByMerchant', 'Etsitään uutta kuljettajaa!', 'Pakettikuljetus ajalle {#bookingStartTime#} on peruttu. Odota, etsimme uutta kuljettajaa.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByDriver', 'Etsitään uutta kuljettajaa!', 'Kuljettaja on perunut pakettikuljetuksen ajalle {#bookingStartTime#}. Odota, etsimme uutta kuljettajaa.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByAllocator', 'Etsitään uutta kuljettajaa!', 'Pakettikuljetus ajalle {#bookingStartTime#} on peruttu. Odota, etsimme uutta kuljettajaa.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT', 'EST_OR_QUOTE_REALLOCATED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'ByApplication', 'Etsitään uutta kuljettajaa!', 'Pahoittelut, pakettikuljetuksesi ajalle {#bookingStartTime#} on peruttu. Odota, etsimme uutta kuljettajaa.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'QUOTE_RECEIVED', 'QUOTE_RECEIVED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Kuljetustarjous saatu!', 'Sait uuden hintatarjouksen pakettikuljetukselle: {#quoteFareEstimate#}', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ON_THE_WAY', 'DRIVER_ON_THE_WAY', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Kuljettaja on matkalla!', 'Kuljettaja on matkalla noutamaan pakettia.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_HAS_REACHED', 'DRIVER_HAS_REACHED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Kuljettaja noutopaikalla!', 'Vahvista nouto ajoneuvolle {#vehicleNumber#} koodilla {#otp#}', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_HAS_REACHED_DESTINATION', 'DRIVER_HAS_REACHED_DESTINATION', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Kuljettaja määränpäässä!', 'Kuljettaja on saapunut paketin jättöpaikkaan.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_REACHING', 'DRIVER_REACHING', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Kuljettaja saapuu!', 'Kuljettaja on melkein noutopaikalla. Olethan valmiina.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'SAFETY_ALERT_DEVIATION', 'SAFETY_ALERT_DEVIATION', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Paketin turvallisuusilmoitus!', 'Pakettikuljetuksen reitti on muuttunut odottamattomasti. Tarkista tiedot sovelluksesta.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_BIRTHDAY', 'DRIVER_BIRTHDAY', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Kuljettajan syntymäpäivä!', 'Tänään on kuljettajasi {#driverName#} syntymäpäivä. Onnittelut ilahduttaisivat häntä!', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'FOLLOW_RIDE', 'FOLLOW_RIDE', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Seuraa pakettikuljetusta', '{#name#} haluaa sinun seuraavan pakettikuljetustaan', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'FILE_UPLOADED', 'PARCEL_IMAGE_UPLOADED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Paketin kuva ladattu', 'Kuljettaja on ladannut kuvan paketista. Avaa sovellus nähdäksesi sen.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'SCHEDULED_RIDE_NOTIFICATION', 'RIDE_START_REMINDER', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', '{#isRentalOrIntercity#} Varausmuistutus', 'Kyyti alkaa klo {#rideStartTime#}. Lisätiedot toimitetaan pian.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'SCHEDULED_RIDE_NOTIFICATION', 'RIDE_START_END_OTP', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Matkan aloituskoodi', 'Hyvä asiakas, matkan aloituskoodisi on {#rideStartOtp#}. Kuljettaja saapuu 15 min ennen aloitusaikaa.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'SAFETY_ALERT_RIDE_STOPPAGE', 'SAFETY_ALERT_RIDE_STOPPAGE', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Onko kaikki hyvin?', 'Huomasimme, ettei kuljettaja ole liikkunut hetkeen. Oletko turvassa?', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD', 'REFERRAL_REWARD_ADD_VPA', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', '💸 Ka-ching! Ansaitsit juuri palkkion', 'Ystäväsi ensimmäinen matka on tehty! Lisää maksutiedot ja lunasta palkkio.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD', 'REFERRAL_REWARD', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', '💸 Ka-ching! Ansaitsit juuri palkkion', 'Ystäväsi ensimmäinen matka on tehty! Kutsu lisää kavereita ja tienaa jokaisesta uudesta matkustajasta.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD', 'REFERRED_BY_REWARD_ADD_VPA', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', '💸 Ka-ching! Ansaitsit juuri palkkion', 'Lisää maksutiedot ja lunasta palkkio. Kiitos, että käytät Lynxiä ja tuet kuljettajiamme!', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD', 'REFERRAL_BONUS_EARNED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', '💸 Ka-ching! Ansaitsit juuri palkkion', 'Suosittelupalkkio on maksettu tilillesi. Kiitos, että käytät Lynxiä!', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD', 'REFERRED_BY_REWARD', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', '💸 Ka-ching! Ansaitsit juuri palkkion', 'Kiitos, että käytät Lynxiä ja tuet kuljettajiamme!', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'ACCOUNT_DELETED', 'ACCOUNT_DELETED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Tili poistettu!', 'Tilisi on poistettu onnistuneesti.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'ACCOUNT_DELETED', 'ACCOUNT_DELETED', 'Delivery_OneWayOnDemandDynamicOffer', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', NULL, 'Tili poistettu!', 'Tilisi on poistettu onnistuneesti.', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_FINISHED', 'TRIP_FINISHED', 'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f', 'f9903ef6-f595-428e-b5ac-e8816cbdf979', 'Matka päättynyt!', 'Toivottavasti viihdyit kuljettajan {#driverName#} kyydissä. Hinta yhteensä {#totalFare#}. Muistithan ottaa kaikki tavarasi mukaan!', 'FINNISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
);
