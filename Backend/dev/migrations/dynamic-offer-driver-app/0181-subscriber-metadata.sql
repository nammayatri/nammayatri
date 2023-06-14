CREATE TABLE atlas_driver_offer_bpp.subscriber_metadata (
    id text NOT NULL PRIMARY KEY,
    "name" text NOT NULL,
    logo_url text NOT NULL
);

INSERT INTO atlas_driver_offer_bpp.subscriber_metadata VALUES ('beckn/cab/v1/NAMMA_YATRI', 'NAMMA_YATRI', 'https://assets.juspay.in/nammayatri/images/common/ic_lost_and_found.png')