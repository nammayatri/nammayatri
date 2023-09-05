CREATE TABLE atlas_driver_offer_bpp.kiosk_location (
  id CHARACTER(36),
  merchant_id CHARACTER(36) NOT NULL,
  address TEXT NOT NULL,
  landmark TEXT NOT NULL,
  contact CHARACTER VARYING(15),
  longitude DOUBLE PRECISION NOT NULL,
  latitude DOUBLE PRECISION NOT NULL,
  PRIMARY KEY (id)
);

INSERT INTO atlas_driver_offer_bpp.kiosk_location (id, merchant_id, landmark, address, contact, longitude, latitude) VALUES
    ( 'f63ec21a-06b8-3fc4-4697-5fd671dfed26', 'favorit0-0000-0000-0000-00000favorit' , 'Hyderbad biryani, Arekere main road', 'Arekere Main Rd, Bengaluru, Karnataka 560076', '06363496571', 77.60114684, 12.88963386),
    ( '14dfd71a-6ea1-0df6-53d7-ced6186203c8', 'favorit0-0000-0000-0000-00000favorit' , 'GBJ park, Marathahalli bridge', 'Jawahar Nagar, Marathahalli, Bengaluru, Karnataka 560037', '09743914738', 77.6931, 12.960529),
    ( 'c64ed661-4821-37f0-c3eb-964d125a8e85', 'favorit0-0000-0000-0000-00000favorit' , 'Magadi underpass, Magadi toll', 'Rajaji Nagar Industrial Town, Rajajinagar, Bengaluru, Karnataka', '09035815859', 77.5492877, 12.9781507),
    ( 'fa03e867-3d3f-9e81-a81f-67222b640602', 'favorit0-0000-0000-0000-00000favorit' , 'Sri shaneshwara temple, Hebbal', 'lake, Bellary Rd, below fly over, near to hebbal, Subramani Nagar, Hebbal, Bengaluru, Karnataka 560024', '07349065850', 77.59055828, 13.0425668),
    ( '80820a44-48b7-8d25-d53d-13124249c45b', 'favorit0-0000-0000-0000-00000favorit' , 'Yelahanka police station', 'SH 9, Ambedkar Colony, Yelahanka New Town, Bengaluru, Karnataka 560065', '07892611615', 77.593378, 13.09603);
-- change the merchant Id to the merchantId of nammayatri in all 5 rows above