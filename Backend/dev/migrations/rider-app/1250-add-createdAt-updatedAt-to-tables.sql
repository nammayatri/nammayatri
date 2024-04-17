--- Please run these queries while doing deployment

ALTER TABLE atlas_app.black_list_org ADD COLUMN created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;
ALTER TABLE atlas_app.black_list_org ADD COLUMN updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;

ALTER TABLE atlas_app.white_list_org ADD COLUMN created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;
ALTER TABLE atlas_app.white_list_org ADD COLUMN updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;

ALTER TABLE atlas_app.trip_terms ADD COLUMN created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;
ALTER TABLE atlas_app.trip_terms ADD COLUMN updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;

ALTER TABLE atlas_app.special_zone_quote ADD COLUMN created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;
ALTER TABLE atlas_app.special_zone_quote ADD COLUMN updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;

ALTER TABLE atlas_app.quote ADD COLUMN updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;

ALTER TABLE atlas_app.person_disability ADD COLUMN created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;

ALTER TABLE atlas_app.special_zone_quote ADD COLUMN created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;
ALTER TABLE atlas_app.special_zone_quote ADD COLUMN updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;

ALTER TABLE atlas_app.driver_offer ADD COLUMN created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;

ALTER TABLE atlas_app.cancellation_reason ADD COLUMN created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;
ALTER TABLE atlas_app.cancellation_reason ADD COLUMN updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;

ALTER TABLE atlas_app.booking_cancellation_reason ADD COLUMN created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;
ALTER TABLE atlas_app.booking_cancellation_reason ADD COLUMN updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;