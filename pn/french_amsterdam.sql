INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FIRST_RIDE_EVENT','FIRST_RIDE_EVENT','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Première Livraison','Félicitations ! Vous avez fait votre première livraison de colis.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Recherche d''un nouveau chauffeur !','La course de {#bookingStartTime#} est annulée. Nous cherchons un nouveau chauffeur.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FCM_CHAT_MESSAGE','FCM_CHAT_MESSAGE',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Appel manqué : Action requise','Votre chauffeur a tenté de vous joindre. Veuillez le rappeler pour organiser la prise en charge.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FIRST_RIDE_EVENT','FIRST_RIDE_EVENT',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  '🎉 Félicitations pour votre 1er trajet avec Bridge !','Votre choix soutient directement les chauffeurs et fait une réelle différence.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_QUOTE_INCOMING','DRIVER_QUOTE_INCOMING',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Nouvelles offres chauffeurs !','Nouvelles offres chauffeurs ! Consultez l''app pour les détails.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ASSIGNMENT','DRIVER_ASSIGNMENT',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Chauffeur assigné !','{#driverName#} sera votre chauffeur pour ce trajet.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_STARTED','TRIP_STARTED',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Votre trajet {#serviceTierName#} a commencé !','Votre trajet {#serviceTierName#} avec {#driverName#} a commencé. Bon voyage !','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'EXPIRED_CASE','EXPIRED_CASE',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Course expirée !','Votre course a expiré car vous n''avez confirmé aucune offre. Veuillez réserver à nouveau.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'REGISTRATION_APPROVED','REGISTRATION_APPROVED',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Inscription terminée !','Bienvenue sur Yatri. Cliquez ici pour réserver votre premier trajet.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Cancellations (with ride)
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Course annulée !','Vous avez annulé votre course pour {#bookingStartTime#}. Consultez l''app.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Course annulée !','L''agence "{#orgName#}" a dû annuler la course pour {#bookingStartTime#}. Veuillez réserver à nouveau pour un autre trajet.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Course annulée !','Le chauffeur a dû annuler la course. Veuillez réserver à nouveau.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Course annulée !','Course annulée, aucun chauffeur trouvé. Veuillez réserver à nouveau.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Course annulée !','Désolé, votre course de {#bookingStartTime#} a été annulée. Veuillez réessayer.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Cancellations (no ride)
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Course annulée !','Vous avez annulé votre course pour {#bookingStartTime#}. Consultez l''app.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Course indisponible !','L''agence "{#orgName#}" a dû annuler la course pour {#bookingStartTime#}. Veuillez réserver à nouveau pour un autre trajet.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Course indisponible !','Désolé, nous n''avons trouvé aucun chauffeur pour {#bookingStartTime#}. Veuillez réessayer.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Course indisponible !','Course annulée, aucun chauffeur trouvé. Veuillez réserver à nouveau.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Add missing explicit subcategory variants for general no-ride cancellations
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByAllocator',
  'Course indisponible !','Course annulée, aucun chauffeur trouvé. Veuillez réserver à nouveau.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByApplication',
  'Course indisponible !','Désolé, nous avons trouvé aucun chauffeur pour {#bookingStartTime#}. Veuillez réessayer.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Note: row 20 duplicates the "no driver" case; include once per key variant as needed.

-- Reallocation
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','BOOKING_REALLOCATED',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Course annulée ! Nous vous cherchons un nouveau chauffeur.','Le chauffeur a annulé la course. Nous vous assignons un autre chauffeur.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Reallocate EST/QUOTE
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Recherche d''un nouveau chauffeur !','Vous avez annulé votre course. Nous cherchons un nouveau chauffeur.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Recherche d''un nouveau chauffeur !','Le chauffeur a annulé la course. Nous cherchons un nouveau chauffeur.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Recherche d''un nouveau chauffeur !','La course de {#bookingStartTime#} est annulée. Nous cherchons un nouveau chauffeur.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Add missing explicit ByMerchant variant for general reallocation
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'ByMerchant',
  'Recherche d''un nouveau chauffeur !','La course de {#bookingStartTime#} est annulée. Nous cherchons un nouveau chauffeur.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Recherche d''un nouveau chauffeur !','Désolé, votre course de {#bookingStartTime#} a été annulée. Nous vous cherchons un nouveau chauffeur.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Quote received
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'QUOTE_RECEIVED','QUOTE_RECEIVED',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Devis reçu !','Nouveau devis reçu : {#quoteFareEstimate#}','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Driver statuses
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ON_THE_WAY','DRIVER_ON_THE_WAY',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Chauffeur en route !','Chauffeur en route','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_HAS_REACHED','DRIVER_HAS_REACHED',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Chauffeur Arrivé !','Utilisez l''OTP {#otp#} pour vérifier la course avec le Véhicule n° {#vehicleNumber#}.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_REACHING','DRIVER_REACHING',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Chauffeur Arrive !','Votre chauffeur arrive ! Soyez au lieu de prise en charge.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Safety deviation
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'SAFETY_ALERT_DEVIATION','SAFETY_ALERT_DEVIATION',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Tout va bien ?','Nous voyons que votre trajet dévie. Vous sentez-vous en sécurité ?','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Driver birthday
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_BIRTHDAY','DRIVER_BIRTHDAY',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Anniversaire du Chauffeur !','C''est l''anniversaire de votre chauffeur {#driverName#}, vos vœux rendront sa journée spéciale !','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Follow ride
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FOLLOW_RIDE','FOLLOW_RIDE',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Suivre Trajet','{#name#} souhaite que vous suiviez son trajet','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Stop reached
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'STOP_REACHED','STOP_REACHED',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Arrêt Atteint !','{#driverName#} a atteint l''arrêt. Vous pouvez en ajouter un autre !','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Ticket cancelled
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','TICKET_CANCELLED',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Ticket {#ticketBookingCategoryName#} Annulé','Désolé, le Ticket {#ticketBookingId#} ({#ticketBookingCategoryName#}) est annulé et sera remboursé. Voir l''app.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Trip updated
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_UPDATED','TRIP_UPDATED',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Destination & Tarif Mis à Jour','Votre demande de modification a été acceptée !','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Delivery-specific (with trip_category)
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_QUOTE_INCOMING','DRIVER_QUOTE_INCOMING','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Nouvelles offres de livraison !','Nouvelles offres pour livrer votre colis. Voir l''app.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ASSIGNMENT','DRIVER_ASSIGNMENT','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Chauffeur pour votre colis !','{#driverName#} s''occupera de la livraison de votre colis.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_STARTED','TRIP_STARTED','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Votre colis est en route !','Votre colis avec {#driverName#} est en route. Suivez-le en direct dans l''app !','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_FINISHED','TRIP_FINISHED','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Colis livré !','Votre colis a été livré par {#driverName#}. Prix total {#totalFare#}','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'EXPIRED_CASE','EXPIRED_CASE','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Demande de livraison expirée !','Votre demande de livraison a expiré, aucune offre confirmée. Réservez à nouveau.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Delivery cancellations (with ride)
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Livraison de colis annulée !','Vous avez annulé la livraison pour {#bookingStartTime#}. Voir l''app.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Livraison de colis annulée !','L''agence "{#orgName#}" a dû annuler la livraison de colis pour {#bookingStartTime#}. Veuillez réserver à nouveau pour un autre trajet.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Livraison de colis annulée !','Le chauffeur a dû annuler la livraison. Réservez à nouveau.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Livraison de colis annulée !','Livraison annulée, aucun chauffeur trouvé. Réservez à nouveau.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Delivery cancellations (no ride)
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Livraison de colis annulée !','Vous avez annulé la livraison pour {#bookingStartTime#}. Voir l''app.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Livraison de colis annulée !','L''agence "{#orgName#}" a dû annuler la livraison de colis pour {#bookingStartTime#}. Veuillez réserver à nouveau pour un autre trajet.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Livraison de colis annulée !','Désolé, aucun chauffeur trouvé pour votre colis. Veuillez réessayer.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Reallocation (delivery)
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','BOOKING_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Réattribution de votre livraison !','Le chauffeur a annulé la livraison. Nous cherchons un nouveau livreur.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Reallocate EST/QUOTE (delivery)
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Recherche d''un nouveau chauffeur !','Vous avez annulé votre livraison. Nous cherchons un nouveau chauffeur.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Recherche d''un nouveau chauffeur !','La livraison pour {#bookingStartTime#} est annulée. Nous cherchons un nouveau chauffeur.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Recherche d''un nouveau chauffeur !','Le chauffeur a annulé la livraison. Nous cherchons un nouveau chauffeur.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Quote received (delivery)
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'QUOTE_RECEIVED','QUOTE_RECEIVED','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Devis de livraison reçu !','Nouveau devis pour votre colis : {#quoteFareEstimate#}','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Driver statuses (delivery)
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ON_THE_WAY','DRIVER_ON_THE_WAY','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Chauffeur en route !','Le chauffeur se dirige vers le lieu de collecte de votre colis.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_HAS_REACHED','DRIVER_HAS_REACHED','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Chauffeur au lieu de collecte !','Utilisez l''OTP {#otp#} pour vérifier la collecte avec le Véhicule n° {#vehicleNumber#}.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_HAS_REACHED_DESTINATION','DRIVER_HAS_REACHED_DESTINATION','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Chauffeur à destination !','Le chauffeur a atteint le lieu de livraison.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_REACHING','DRIVER_REACHING','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Chauffeur Arrive !','Votre chauffeur est presque au lieu de collecte. Soyez prêt.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Safety (delivery)
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'SAFETY_ALERT_DEVIATION','SAFETY_ALERT_DEVIATION','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Alerte sécurité livraison !','L''itinéraire de votre colis a changé. Voir l''app.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Driver birthday (delivery)
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_BIRTHDAY','DRIVER_BIRTHDAY','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Anniversaire du Chauffeur !','C''est l''anniversaire de votre livreur {#driverName#}, souhaitez-le-lui à la livraison !','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Follow delivery
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FOLLOW_RIDE','FOLLOW_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Suivre la livraison','{#name#} souhaite que vous suiviez sa livraison de colis','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Parcel image uploaded
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FILE_UPLOADED','PARCEL_IMAGE_UPLOADED','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Photo du colis chargée','La photo de votre colis a été chargée. Ouvrez l''app pour la voir.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Scheduled notifications
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'SCHEDULED_RIDE_NOTIFICATION','RIDE_START_REMINDER',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Rappel {#isRentalOrIntercity#}','Trajet à {#rideStartTime#}. Détails à venir.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'SCHEDULED_RIDE_NOTIFICATION','RIDE_START_END_OTP',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'OTP de Départ','Cher utilisateur, votre OTP de départ est {#rideStartOtp#}. Le chauffeur arrivera 15 min avant.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Safety stoppage
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'SAFETY_ALERT_RIDE_STOPPAGE','SAFETY_ALERT_RIDE_STOPPAGE',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Tout va bien ?','Nous voyons que votre chauffeur ne bouge plus. Vous sentez-vous en sécurité ?','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Payout/rewards
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD','REFERRAL_REWARD_ADD_VPA',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  '💸 Super ! Récompense gagnée','Le 1er trajet de votre ami est terminé ! Ajoutez un ID UPI et gagnez votre récompense.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD','REFERRAL_REWARD',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  '💸 Super ! Récompense gagnée','Le 1er trajet de votre ami est terminé ! Parrainez plus d''amis pour gagner plus.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD','REFERRED_BY_REWARD_ADD_VPA',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  '💸 Super ! Récompense gagnée','Ajoutez un ID UPI et gagnez la récompense. Merci d''utiliser Bridge, continuez à soutenir nos chauffeurs !','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD','REFERRAL_BONUS_EARNED',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  '💸 Super ! Récompense gagnée','Votre récompense de parrainage a été créditée. Merci d''utiliser Bridge !!','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD','REFERRED_BY_REWARD',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  '💸 Super ! Récompense gagnée','Merci d''utiliser Bridge, continuez à soutenir nos chauffeurs !','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Account deleted (no trip and delivery)
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'ACCOUNT_DELETED','ACCOUNT_DELETED',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Compte supprimé !','Votre compte a été supprimé avec succès.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'ACCOUNT_DELETED','ACCOUNT_DELETED','Delivery_OneWayOnDemandDynamicOffer',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Compte supprimé !','Votre compte a été supprimé avec succès.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Trip finished
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_FINISHED','TRIP_FINISHED',
  'e39cb491-03a3-4341-831b-b256ef3c95c9','704fbdc3-7cc6-4efd-9380-27a72f4f9a5e',
  'Trajet terminé !','Nous espérons que votre trajet avec {#driverName#} vous a plu. Prix total {#totalFare#}. N''oubliez pas vos affaires !','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);