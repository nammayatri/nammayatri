CREATE TABLE atlas_app.special_zone_quote (
    id character(36) NOT NULL PRIMARY KEY,
    quote_id character varying(100) NOT NULL
);

ALTER TABLE
    atlas_app.quote
ADD
    COLUMN special_zone_quote_id character(36) REFERENCES atlas_app.special_zone_quote (id);

ALTER TABLE
    atlas_app.booking
ADD
    COLUMN otp_code character(4);

