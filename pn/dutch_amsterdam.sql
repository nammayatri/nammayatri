
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByUser',
  'Rit geannuleerd!','Je hebt je rit voor {#bookingStartTime#} geannuleerd. Bekijk de app.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByUser',
  'Rit geannuleerd!','Je hebt je rit voor {#bookingStartTime#} geannuleerd. Bekijk de app.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByMerchant',
  'Rit geannuleerd!','Agentschap "{#orgName#}" moest de rit voor {#bookingStartTime#} annuleren. Boek alstublieft opnieuw voor een andere rit.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByDriver',
  'Rit geannuleerd!','De chauffeur moest de rit annuleren. Boek alstublieft opnieuw.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByAllocator',
  'Rit geannuleerd!','De rit is geannuleerd, we konden geen chauffeur vinden. Boek alstublieft opnieuw.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByApplication',
  'Rit geannuleerd!','Sorry, je rit voor {#bookingStartTime#} is geannuleerd. Probeer opnieuw te boeken.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);


INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByMerchant',
  'Rit niet beschikbaar!','Agentschap "{#orgName#}" moest de rit voor {#bookingStartTime#} annuleren. Boek alstublieft opnieuw voor een andere rit.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByDriver',
  'Rit niet beschikbaar!','Sorry, we konden geen chauffeur vinden voor uw rit om {#bookingStartTime#}. Probeer opnieuw.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByAllocator',
  'Rit niet beschikbaar!','De rit is geannuleerd, we konden geen chauffeur vinden. Boek alstublieft opnieuw.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByApplication',
  'Rit niet beschikbaar!','Sorry, we konden geen chauffeur vinden voor uw rit om {#bookingStartTime#}. Probeer opnieuw.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','BOOKING_REALLOCATED',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  NULL,
  'Rit geannuleerd! We zoeken een nieuwe chauffeur.','De chauffeur heeft de rit geannuleerd. We wijzen een andere chauffeur toe.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByUser',
  'Nieuwe chauffeur gezocht!','Je hebt je rit geannuleerd. We zoeken een nieuwe chauffeur voor je.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByDriver',
  'Nieuwe chauffeur gezocht!','De chauffeur heeft de rit geannuleerd. We zoeken een nieuwe chauffeur.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByAllocator',
  'Nieuwe chauffeur gezocht!','De rit van {#bookingStartTime#} is geannuleerd. We zoeken een nieuwe chauffeur.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByApplication',
  'Nieuwe chauffeur gezocht!','Sorry, uw rit van {#bookingStartTime#} is geannuleerd. We zoeken een nieuwe chauffeur voor u.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','TICKET_CANCELLED',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  '{#ticketBookingCategoryName#} Ticket Geannuleerd','Sorry, Ticket {#ticketBookingId#} ({#ticketBookingCategoryName#}) is geannuleerd en wordt terugbetaald. Check de app.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- issue here --
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByUser',
  'Pakketbezorging geannuleerd!','Je hebt de pakketbezorging voor {#bookingStartTime#} geannuleerd. Check de app.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByMerchant',
  'Pakketbezorging geannuleerd!','Agentschap "{#orgName#}" moest de pakketbezorging voor {#bookingStartTime#} annuleren. Boek alstublieft opnieuw voor een andere rit.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByDriver',
  'Pakketbezorging geannuleerd!','De chauffeur moest de bezorging annuleren. Boek opnieuw.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByAllocator',
  'Pakketbezorging geannuleerd!','Pakketbezorging geannuleerd, geen chauffeur gevonden. Boek opnieuw.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByApplication',
  'Pakketbezorging geannuleerd!','Sorry je pakketbezorging is geannuleerd. Probeer opnieuw te boeken.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByUser',
  'Pakketbezorging geannuleerd!','Je hebt de pakketbezorging voor {#bookingStartTime#} geannuleerd. Check de app.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByMerchant',
  'Pakketbezorging geannuleerd!','Agentschap "{#orgName#}" moest de pakketbezorging voor {#bookingStartTime#} annuleren. Boek alstublieft opnieuw voor een andere rit.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByDriver',
  'Pakketbezorging geannuleerd!','Sorry, geen chauffeur gevonden voor je pakket. Probeer opnieuw te boeken.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByAllocator',
  'Pakketbezorging geannuleerd!','Pakketbezorging geannuleerd, geen chauffeur gevonden. Boek opnieuw.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByApplication',
  'Pakketbezorging geannuleerd!','Sorry, geen chauffeur gevonden voor je pakket. Probeer opnieuw te boeken.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','BOOKING_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  NULL,
  'Nieuwe bezorger wordt gezocht!','De chauffeur heeft de bezorging geannuleerd. We zoeken een nieuwe chauffeur.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByUser',
  'Nieuwe chauffeur gezocht!','Je hebt je bezorging geannuleerd. We zoeken een nieuwe chauffeur.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByMerchant',
  'Nieuwe chauffeur gezocht!','De pakketbezorging voor {#bookingStartTime#} is geannuleerd. We zoeken een nieuwe chauffeur.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByDriver',
  'Nieuwe chauffeur gezocht!','De chauffeur heeft de bezorging geannuleerd. We zoeken een nieuwe chauffeur.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByAllocator',
  'Nieuwe chauffeur gezocht!','De pakketbezorging voor {#bookingStartTime#} is geannuleerd. We zoeken een nieuwe chauffeur.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByApplication',
  'Nieuwe chauffeur gezocht!','Sorry, je bezorging is geannuleerd. We zoeken een nieuwe chauffeur.','DUTCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);