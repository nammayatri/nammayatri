ALTER TABLE
    atlas_app.quote
ADD
    COLUMN special_zone_quote_id character(36) REFERENCES atlas_app.special_zone_quote (id);

ALTER TABLE
    atlas_app.booking
ADD
    COLUMN otp_code character(4);

