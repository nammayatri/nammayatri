------------- Issue Category Table ----------------------------------
ALTER TABLE atlas_app.issue_category ADD COLUMN category_type text NOT NULL DEFAULT 'Category';
ALTER TABLE atlas_app.issue_category ADD COLUMN created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;
ALTER TABLE atlas_app.issue_category ADD COLUMN updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;
ALTER TABLE atlas_app.issue_category ADD COLUMN is_ride_required Boolean DEFAULT false NOT NULL;
ALTER TABLE atlas_app.issue_category ADD COLUMN max_allowed_ride_age int;
ALTER TABLE atlas_app.issue_category ADD COLUMN is_active boolean DEFAULT true NOT NULL;
ALTER TABLE atlas_app.issue_category ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_app.merchant_operating_city(id);
ALTER TABLE atlas_app.issue_category ADD COLUMN label text;
ALTER TABLE atlas_app.issue_category ADD COLUMN is_ticket_required Boolean DEFAULT false NOT NULL;

-- QUERY FOR PROD AND MASTER
-- UPDATE atlas_app.issue_category
-- SET merchant_operating_city_id = (
--     SELECT id
--     FROM atlas_app.merchant_operating_city
--     WHERE city = 'Bangalore' AND merchant_short_id = 'NAMMA_YATRI'
-- );

ALTER TABLE atlas_app.issue_category
ALTER COLUMN merchant_operating_city_id SET NOT NULL;

------------- Issue Option Table ----------------------------------
ALTER TABLE atlas_app.issue_option ADD COLUMN restricted_variants text[] DEFAULT '{}';
ALTER TABLE atlas_app.issue_option ADD COLUMN is_active boolean DEFAULT true NOT NULL;
ALTER TABLE atlas_app.issue_option ADD COLUMN created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;
ALTER TABLE atlas_app.issue_option ADD COLUMN updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;
ALTER TABLE atlas_app.issue_option ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_app.merchant_operating_city(id);
ALTER TABLE atlas_app.issue_option ADD COLUMN show_only_when_user_blocked boolean DEFAULT false NOT NULL;

-- QUERY FOR PROD AND MASTER
-- UPDATE atlas_app.issue_option
-- SET merchant_operating_city_id = (
--     SELECT id
--     FROM atlas_app.merchant_operating_city
--     WHERE city = 'Bangalore' AND merchant_short_id = 'NAMMA_YATRI'
-- );

ALTER TABLE atlas_app.issue_option
ALTER COLUMN merchant_operating_city_id SET NOT NULL;

------------- Issue Config Table ----------------------------------
ALTER TABLE atlas_app.issue_config
ADD COLUMN merchant_operating_city_id character(36);

ALTER TABLE atlas_app.issue_config DROP CONSTRAINT issue_config_pkey;

ALTER TABLE atlas_app.issue_config
ADD CONSTRAINT pk_merchant_operating_city_id PRIMARY KEY (merchant_operating_city_id);

ALTER TABLE atlas_app.issue_config
ADD CONSTRAINT fk_merchant_operating_city_id
FOREIGN KEY (merchant_operating_city_id)
REFERENCES atlas_app.merchant_operating_city(id);

ALTER TABLE atlas_app.issue_config ADD COLUMN message_transformation_config json;

--------------- Issue Message Table ----------------------------------
ALTER TABLE atlas_app.issue_message ADD COLUMN reference_option_id character(36) REFERENCES atlas_app.issue_option(id);
ALTER TABLE atlas_app.issue_message ADD COLUMN reference_category_id character(36) REFERENCES atlas_app.issue_option(id);
ALTER TABLE atlas_app.issue_message ADD COLUMN media_files text[] NOT NULL DEFAULT ARRAY[]::text[];
ALTER TABLE atlas_app.issue_message ADD COLUMN created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP  NOT NULL;
ALTER TABLE atlas_app.issue_message ADD COLUMN updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP  NOT NULL;
ALTER TABLE atlas_app.issue_message ADD COLUMN message_title text;
ALTER TABLE atlas_app.issue_message ADD COLUMN message_action text;
ALTER TABLE atlas_app.issue_message ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_app.merchant_operating_city(id);
ALTER TABLE atlas_app.issue_message ADD COLUMN message_type text;
ALTER TABLE atlas_app.issue_message ADD COLUMN is_active boolean DEFAULT true NOT NULL;

-- QUERY FOR PROD AND MASTER
-- UPDATE atlas_app.issue_message
-- SET merchant_operating_city_id = (
--     SELECT id
--     FROM atlas_app.merchant_operating_city
--     WHERE city = 'Bangalore' AND merchant_short_id = 'NAMMA_YATRI'
-- );

ALTER TABLE atlas_app.issue_message
ALTER COLUMN merchant_operating_city_id SET NOT NULL;

--------------- Issue Translation Table ----------------------------------
ALTER TABLE atlas_app.issue_translation ADD COLUMN created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP  NOT NULL;
ALTER TABLE atlas_app.issue_translation ADD COLUMN updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP  NOT NULL;