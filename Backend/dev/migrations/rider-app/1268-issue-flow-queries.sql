------------- Issue Category Table ----------------------------------
ALTER TABLE atlas_app.issue_category ADD COLUMN category_type text NOT NULL DEFAULT 'Category';
ALTER TABLE atlas_app.issue_category ADD COLUMN created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;
ALTER TABLE atlas_app.issue_category ADD COLUMN updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;
ALTER TABLE atlas_app.issue_category ADD COLUMN max_allowed_ride_age int;
ALTER TABLE atlas_app.issue_category ADD COLUMN is_active boolean DEFAULT true NOT NULL;

UPDATE atlas_app.issue_category SET max_allowed_ride_age = 259200;
UPDATE atlas_app.issue_category SET max_allowed_ride_age = 86400 where category = 'lost and found';

------------- Issue Option Table ----------------------------------
ALTER TABLE atlas_app.issue_option ADD COLUMN restricted_variants text[] DEFAULT '{}';
ALTER TABLE atlas_app.issue_option ADD COLUMN is_active boolean DEFAULT true NOT NULL;
ALTER TABLE atlas_app.issue_option ADD COLUMN created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;
ALTER TABLE atlas_app.issue_option ADD COLUMN updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;

------------- Issue Config Table ----------------------------------
ALTER TABLE atlas_app.issue_config
ADD COLUMN merchant_operating_city_id character(36);

INSERT INTO atlas_app.issue_config (
    id,
    merchant_operating_city_id,
    auto_mark_issue_closed_duration,
    on_auto_mark_issue_cls_msgs,
    on_create_issue_msgs,
    on_issue_reopen_msgs,
    on_kapt_mark_issue_res_msgs
)
SELECT
    moc.id,
    moc.id AS merchant_operating_city_id,
    ic.auto_mark_issue_closed_duration,
    ic.on_auto_mark_issue_cls_msgs,
    ic.on_create_issue_msgs,
    ic.on_issue_reopen_msgs,
    ic.on_kapt_mark_issue_res_msgs
FROM atlas_app.merchant_operating_city moc
CROSS JOIN (
    SELECT * FROM atlas_app.issue_config LIMIT 1
) ic;

DELETE FROM atlas_app.issue_config WHERE merchant_operating_city_id IS NULL;

ALTER TABLE atlas_app.issue_config DROP CONSTRAINT issue_config_pkey;

ALTER TABLE atlas_app.issue_config
ADD CONSTRAINT pk_merchant_operating_city_id PRIMARY KEY (merchant_operating_city_id);

ALTER TABLE atlas_app.issue_config
ADD CONSTRAINT fk_merchant_operating_city_id
FOREIGN KEY (merchant_operating_city_id)
REFERENCES atlas_app.merchant_operating_city(id);

ALTER TABLE atlas_app.issue_config ADD COLUMN message_transformation_config json;

UPDATE atlas_app.issue_config ic
SET message_transformation_config =
    JSON_BUILD_OBJECT(
        'merchantName', INITCAP(REPLACE(SUBSTRING(moc.merchant_short_id FROM 1 FOR 1) || LOWER(SUBSTRING(moc.merchant_short_id FROM 2)), '_', ' ')),
        'supportEmail', 'support@nammayatri.in'
    )
FROM atlas_app.merchant_operating_city moc
WHERE ic.merchant_operating_city_id = moc.id;

--------------- Issue Message Table ----------------------------------
ALTER TABLE atlas_app.issue_message ADD COLUMN reference_option_id character(36) REFERENCES atlas_app.issue_option(id);
ALTER TABLE atlas_app.issue_message ADD COLUMN reference_category_id character(36) REFERENCES atlas_app.issue_option(id);
ALTER TABLE atlas_app.issue_message ADD COLUMN media_files text[] NOT NULL DEFAULT ARRAY[]::text[];
ALTER TABLE atlas_app.issue_message ADD COLUMN created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP  NOT NULL;
ALTER TABLE atlas_app.issue_message ADD COLUMN updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP  NOT NULL;
ALTER TABLE atlas_app.issue_message ADD COLUMN message_title text;
ALTER TABLE atlas_app.issue_message ADD COLUMN message_action text;

--------------- Issue Translation Table ----------------------------------
ALTER TABLE atlas_app.issue_translation ADD COLUMN created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP  NOT NULL;
ALTER TABLE atlas_app.issue_translation ADD COLUMN updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP  NOT NULL;