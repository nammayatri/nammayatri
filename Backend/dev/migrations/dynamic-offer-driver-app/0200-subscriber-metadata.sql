CREATE TABLE atlas_driver_offer_bpp.bap_metadata (
    id text NOT NULL PRIMARY KEY,
    name text NOT NULL,
    logo_url text NOT NULL
);

INSERT INTO atlas_driver_offer_bpp.bap_metadata VALUES ('localhost/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a51', 'YATRI', 'https://assets.juspay.in/nammayatri/images/common/ic_lost_and_found.png');
INSERT INTO atlas_driver_offer_bpp.bap_metadata VALUES ('localhost/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'NAMMA YATRI', 'https://assets.juspay.in/nammayatri/images/common/ic_lost_and_found.png');