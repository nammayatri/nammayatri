
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'FIRST_RIDE_EVENT','FIRST_RIDE_EVENT','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Première Livraison','Félicitations ! Vous avez fait votre première livraison de colis.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByMerchant',
  'Recherche d''un nouveau chauffeur !','La course de {#bookingStartTime#} est annulée. Nous cherchons un nouveau chauffeur.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FCM_CHAT_MESSAGE','FCM_CHAT_MESSAGE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Appel manqué : Action requise','Votre chauffeur a tenté de vous joindre. Veuillez le rappeler pour organiser la prise en charge.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FIRST_RIDE_EVENT','FIRST_RIDE_EVENT',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  '🎉 Félicitations pour votre 1er trajet avec Bridge !','Votre choix soutient directement les chauffeurs et fait une réelle différence.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_QUOTE_INCOMING','DRIVER_QUOTE_INCOMING',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Nouvelles offres chauffeurs !','Nouvelles offres chauffeurs ! Consultez l''app pour les détails.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ASSIGNMENT','DRIVER_ASSIGNMENT',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Chauffeur assigné !','{#driverName#} sera votre chauffeur pour ce trajet.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_STARTED','TRIP_STARTED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Votre trajet {#serviceTierName#} a commencé !','Votre trajet {#serviceTierName#} avec {#driverName#} a commencé. Bon voyage !','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'EXPIRED_CASE','EXPIRED_CASE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Course expirée !','Votre course a expiré car vous n''avez confirmé aucune offre. Veuillez réserver à nouveau.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'REGISTRATION_APPROVED','REGISTRATION_APPROVED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Inscription terminée !','Bienvenue sur Yatri. Cliquez ici pour réserver votre premier trajet.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Cancellations with ride (general)
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByUser',
  'Course annulée !','Vous avez annulé votre course pour {#bookingStartTime#}. Consultez l''app.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByMerchant',
  'Course annulée !','L''agence "{#orgName#}" a dû annuler la course pour {#bookingStartTime#}. Veuillez réserver à nouveau pour un autre trajet.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByDriver',
  'Course annulée !','Le chauffeur a dû annuler la course. Veuillez réserver à nouveau.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByAllocator',
  'Course annulée !','Course annulée, aucun chauffeur trouvé. Veuillez réserver à nouveau.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByApplication',
  'Course annulée !','Désolé, votre course de {#bookingStartTime#} a été annulée. Veuillez réessayer.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Cancellations no ride (general)
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByUser',
  'Course annulée !','Vous avez annulé votre course pour {#bookingStartTime#}. Consultez l''app.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByMerchant',
  'Course indisponible !','L''agence "{#orgName#}" a dû annuler la course pour {#bookingStartTime#}. Veuillez réserver à nouveau pour un autre trajet.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByDriver',
  'Course indisponible !','Désolé, nous n''avons trouvé aucun chauffeur pour {#bookingStartTime#}. Veuillez réessayer.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByAllocator',
  'Course indisponible !','Course annulée, aucun chauffeur trouvé. Veuillez réserver à nouveau.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByApplication',
  'Course indisponible !','Désolé, nous n''avons trouvé aucun chauffeur pour {#bookingStartTime#}. Veuillez réessayer.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Reallocation (general)
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','BOOKING_REALLOCATED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Course annulée ! Nous vous cherchons un nouveau chauffeur.','Le chauffeur a annulé la course. Nous vous assignons un autre chauffeur.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByUser',
  'Recherche d''un nouveau chauffeur !','Vous avez annulé votre course. Nous cherchons un nouveau chauffeur.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByDriver',
  'Recherche d''un nouveau chauffeur !','Le chauffeur a annulé la course. Nous cherchons un nouveau chauffeur.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByAllocator',
  'Recherche d''un nouveau chauffeur !','La course de {#bookingStartTime#} est annulée. Nous cherchons un nouveau chauffeur.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByApplication',
  'Recherche d''un nouveau chauffeur !','Désolé, votre course de {#bookingStartTime#} a été annulée. Nous vous cherchons un nouveau chauffeur.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Quote received (general)
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'QUOTE_RECEIVED','QUOTE_RECEIVED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Devis reçu !','Nouveau devis reçu : {#quoteFareEstimate#}','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Driver statuses (general)
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ON_THE_WAY','DRIVER_ON_THE_WAY',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Chauffeur en route !','Chauffeur en route','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_HAS_REACHED','DRIVER_HAS_REACHED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Chauffeur Arrivé !','Utilisez l''OTP {#otp#} pour vérifier la course avec le Véhicule n° {#vehicleNumber#}.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_REACHING','DRIVER_REACHING',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Chauffeur Arrive !','Votre chauffeur arrive ! Soyez au lieu de prise en charge.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Safety deviation (general)
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'SAFETY_ALERT_DEVIATION','SAFETY_ALERT_DEVIATION',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Tout va bien ?','Nous voyons que votre trajet dévie. Vous sentez-vous en sécurité ?','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Driver birthday (general)
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_BIRTHDAY','DRIVER_BIRTHDAY',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Anniversaire du Chauffeur !','C''est l''anniversaire de votre chauffeur {#driverName#}, vos vœux rendront sa journée spéciale !','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Follow ride (general)
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FOLLOW_RIDE','FOLLOW_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Suivre Trajet','{#name#} souhaite que vous suiviez son trajet','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Stop reached (general)
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'STOP_REACHED','STOP_REACHED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Arrêt Atteint !','{#driverName#} a atteint l''arrêt. Vous pouvez en ajouter un autre !','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Ticket cancelled (general)
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','TICKET_CANCELLED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Ticket {#ticketBookingCategoryName#} Annulé','Désolé, le Ticket {#ticketBookingId#} ({#ticketBookingCategoryName#}) est annulé et sera remboursé. Voir l''app.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Trip updated (general)
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_UPDATED','TRIP_UPDATED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Destination & Tarif Mis à Jour','Votre demande de modification a été acceptée !','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Delivery: Dynamic Offer Parcel Delivery variants
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_QUOTE_INCOMING','DRIVER_QUOTE_INCOMING','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Nouvelles offres de livraison !','Nouvelles offres pour livrer votre colis. Voir l''app.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ASSIGNMENT','DRIVER_ASSIGNMENT','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Chauffeur pour votre colis !','{#driverName#} s''occupera de la livraison de votre colis.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_STARTED','TRIP_STARTED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Votre colis est en route !','Votre colis avec {#driverName#} est en route. Suivez-le en direct dans l''app !','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_FINISHED','TRIP_FINISHED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Colis livré !','Votre colis a été livré par {#driverName#}. Prix total {#totalFare#}','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'EXPIRED_CASE','EXPIRED_CASE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Demande de livraison expirée !','Votre demande de livraison a expiré, aucune offre confirmée. Réservez à nouveau.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Delivery cancellations with ride
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByUser',
  'Livraison de colis annulée !','Vous avez annulé la livraison pour {#bookingStartTime#}. Voir l''app.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByMerchant',
  'Livraison de colis annulée !','L''agence "{#orgName#}" a dû annuler la livraison de colis pour {#bookingStartTime#}. Veuillez réserver à nouveau pour un autre trajet.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByDriver',
  'Livraison de colis annulée !','Le chauffeur a dû annuler la livraison. Réservez à nouveau.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByAllocator',
  'Livraison de colis annulée !','Livraison annulée, aucun chauffeur trouvé. Réservez à nouveau.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByApplication',
  'Livraison de colis annulée !','Désolé, votre livraison a été annulée. Veuillez réessayer.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Delivery cancellations no ride
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByUser',
  'Livraison de colis annulée !','Vous avez annulé la livraison pour {#bookingStartTime#}. Voir l''app.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByMerchant',
  'Livraison de colis annulée !','L''agence "{#orgName#}" a dû annuler la livraison de colis pour {#bookingStartTime#}. Veuillez réserver à nouveau pour un autre trajet.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByDriver',
  'Livraison de colis annulée !','Désolé, aucun chauffeur trouvé pour votre colis. Veuillez réessayer.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByAllocator',
  'Livraison de colis annulée !','Livraison annulée, aucun chauffeur trouvé. Réservez à nouveau.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByApplication',
  'Livraison de colis annulée !','Désolé, aucun chauffeur trouvé pour votre colis. Veuillez réessayer.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Delivery reallocate general
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','BOOKING_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Réattribution de votre livraison !','Le chauffeur a annulé la livraison. Nous cherchons un nouveau livreur.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Delivery EST/QUOTE reallocated
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByUser',
  'Recherche d''un nouveau chauffeur !','Vous avez annulé votre livraison. Nous cherchons un nouveau chauffeur.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByMerchant',
  'Recherche d''un nouveau chauffeur !','La livraison pour {#bookingStartTime#} est annulée. Nous cherchons un nouveau chauffeur.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByDriver',
  'Recherche d''un nouveau chauffeur !','Le chauffeur a annulé la livraison. Nous cherchons un nouveau chauffeur.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByAllocator',
  'Recherche d''un nouveau chauffeur !','La livraison pour {#bookingStartTime#} est annulée. Nous cherchons un nouveau chauffeur.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByApplication',
  'Recherche d''un nouveau chauffeur !','Désolé, votre livraison est annulée. Nous cherchons un nouveau chauffeur.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Delivery quote received
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'QUOTE_RECEIVED','QUOTE_RECEIVED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Devis de livraison reçu !','Nouveau devis pour votre colis : {#quoteFareEstimate#}','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Delivery driver statuses
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ON_THE_WAY','DRIVER_ON_THE_WAY','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Chauffeur en route !','Le chauffeur se dirige vers le lieu de collecte de votre colis.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_HAS_REACHED','DRIVER_HAS_REACHED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Chauffeur au lieu de collecte !','Utilisez l''OTP {#otp#} pour vérifier la collecte avec le Véhicule n° {#vehicleNumber#}.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_HAS_REACHED_DESTINATION','DRIVER_HAS_REACHED_DESTINATION','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Chauffeur à destination !','Le chauffeur a atteint le lieu de livraison.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_REACHING','DRIVER_REACHING','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Chauffeur Arrive !','Votre chauffeur est presque au lieu de collecte. Soyez prêt.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Delivery safety
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'SAFETY_ALERT_DEVIATION','SAFETY_ALERT_DEVIATION','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Alerte sécurité livraison !','L''itinéraire de votre colis a changé. Voir l''app.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Delivery driver birthday
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_BIRTHDAY','DRIVER_BIRTHDAY','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Anniversaire du Chauffeur !','C''est l''anniversaire de votre livreur {#driverName#}, souhaitez-le-lui à la livraison !','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Follow delivery
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FOLLOW_RIDE','FOLLOW_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Suivre la livraison','{#name#} souhaite que vous suiviez sa livraison de colis','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Parcel image uploaded
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FILE_UPLOADED','PARCEL_IMAGE_UPLOADED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Photo du colis chargée','La photo de votre colis a été chargée. Ouvrez l''app pour la voir.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Scheduled notifications
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'SCHEDULED_RIDE_NOTIFICATION','RIDE_START_REMINDER','Rappel {#isRentalOrIntercity#}','Trajet à {#rideStartTime#}. Détails à venir.','FRENCH',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'SCHEDULED_RIDE_NOTIFICATION','RIDE_START_END_OTP','OTP de Départ','Cher utilisateur, votre OTP de départ est {#rideStartOtp#}. Le chauffeur arrivera 15 min avant.','FRENCH',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Safety stoppage
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'SAFETY_ALERT_RIDE_STOPPAGE','SAFETY_ALERT_RIDE_STOPPAGE','Tout va bien ?','Nous voyons que votre chauffeur ne bouge plus. Vous sentez-vous en sécurité ?','FRENCH',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Payout/rewards
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD','REFERRAL_REWARD_ADD_VPA','💸 Super ! Récompense gagnée','Le 1er trajet de votre ami est terminé ! Ajoutez un ID UPI et gagnez votre récompense.','FRENCH',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD','REFERRAL_REWARD','💸 Super ! Récompense gagnée','Le 1er trajet de votre ami est terminé ! Parrainez plus d''amis pour gagner plus.','FRENCH',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD','REFERRED_BY_REWARD_ADD_VPA','💸 Super ! Récompense gagnée','Ajoutez un ID UPI et gagnez la récompense. Merci d''utiliser Bridge, continuez à soutenir nos chauffeurs !','FRENCH',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD','REFERRAL_BONUS_EARNED','💸 Super ! Récompense gagnée','Votre récompense de parrainage a été créditée. Merci d''utiliser Bridge !!','FRENCH',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD','REFERRED_BY_REWARD','💸 Super ! Récompense gagnée','Merci d''utiliser Bridge, continuez à soutenir nos chauffeurs !','FRENCH',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Account deleted (general and delivery)
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'ACCOUNT_DELETED','ACCOUNT_DELETED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Compte supprimé !','Votre compte a été supprimé avec succès.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'ACCOUNT_DELETED','ACCOUNT_DELETED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Compte supprimé !','Votre compte a été supprimé avec succès.','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Trip finished (general)
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_FINISHED','TRIP_FINISHED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Trajet terminé !','Nous espérons que votre trajet avec {#driverName#} vous a plu. Prix total {#totalFare#}. N''oubliez pas vos affaires !','FRENCH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);


