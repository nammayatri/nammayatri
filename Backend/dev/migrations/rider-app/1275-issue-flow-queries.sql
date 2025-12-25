------------- Issue Category Table ----------------------------------
ALTER TABLE atlas_app.issue_category ADD COLUMN category_type text NOT NULL DEFAULT 'Category';
ALTER TABLE atlas_app.issue_category ADD COLUMN created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;
ALTER TABLE atlas_app.issue_category ADD COLUMN updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;
ALTER TABLE atlas_app.issue_category ADD COLUMN is_ride_required Boolean DEFAULT false NOT NULL;
ALTER TABLE atlas_app.issue_category ADD COLUMN max_allowed_ride_age int;
ALTER TABLE atlas_app.issue_category ADD COLUMN is_active boolean DEFAULT true NOT NULL;
ALTER TABLE atlas_app.issue_category ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_app.merchant_operating_city(id);
ALTER TABLE atlas_app.issue_category ADD COLUMN label text;

-- QUERIES TO UPDATE is_ride_required IN PROD AND MASTER
UPDATE atlas_app.issue_category SET is_ride_required = true
WHERE id = 'abcdefgh-1234-5678-90ab-1234567890ab';

UPDATE atlas_app.issue_category SET is_ride_required = false
WHERE id = '1w4xjumg-2bvq-x6ez-voqj-ufc2m8ip2sxa';

UPDATE atlas_app.issue_category SET is_ride_required = false
WHERE id = 'xu5jlqrn-ogn5-l8vx-d6f2-fi1fr1q3u8zh';

UPDATE atlas_app.issue_category SET is_ride_required = true
WHERE id = 'ziig3kxh-v0xc-kh0t-q6p1-f1v2n8ucs0kj';

UPDATE atlas_app.issue_category SET is_ride_required = true
WHERE id = 'nkm5pqj4-56hq-prdt-3s2y-9yuc1zgdy79w';

UPDATE atlas_app.issue_category SET is_ride_required = true
WHERE id = 'f01lail9-0hrg-elpj-skkm-2omgyhk3c2h0';

UPDATE atlas_app.issue_category SET is_ride_required = false
WHERE id = 's5fkl23p-gr2l-z5qy-gjn5-gjpjvk6lp4u2';

UPDATE atlas_app.issue_category SET is_ride_required = true
WHERE id = 'jdir4kkp-49fb-4x75-mqgf-3ig3mm2d7ecy';

-- QUERY FOR LOCAL
UPDATE atlas_app.issue_category
SET merchant_operating_city_id = (
    SELECT id
    FROM atlas_app.merchant_operating_city
    WHERE city = 'Kochi' AND merchant_short_id = 'NAMMA_YATRI'
);

-- QUERY FOR PROD AND MASTER
-- UPDATE atlas_app.issue_category
-- SET merchant_operating_city_id = (
--     SELECT id
--     FROM atlas_app.merchant_operating_city
--     WHERE city = 'Bangalore' AND merchant_short_id = 'NAMMA_YATRI'
-- );

ALTER TABLE atlas_app.issue_category
ALTER COLUMN merchant_operating_city_id SET NOT NULL;

UPDATE atlas_app.issue_category SET max_allowed_ride_age = 259200;
UPDATE atlas_app.issue_category SET max_allowed_ride_age = 86400 where category = 'lost and found';

UPDATE atlas_app.issue_category SET label = 'APP_RELATED' WHERE category = 'app related';

------------- Issue Option Table ----------------------------------
ALTER TABLE atlas_app.issue_option ADD COLUMN restricted_variants text[] DEFAULT '{}';
ALTER TABLE atlas_app.issue_option ADD COLUMN is_active boolean DEFAULT true NOT NULL;
ALTER TABLE atlas_app.issue_option ADD COLUMN created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;
ALTER TABLE atlas_app.issue_option ADD COLUMN updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;
ALTER TABLE atlas_app.issue_option ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_app.merchant_operating_city(id);
ALTER TABLE atlas_app.issue_option ADD COLUMN show_only_when_user_blocked boolean DEFAULT false NOT NULL;

-- QUERY FOR LOCAL
UPDATE atlas_app.issue_option
SET merchant_operating_city_id = (
    SELECT id
    FROM atlas_app.merchant_operating_city
    WHERE city = 'Kochi' AND merchant_short_id = 'NAMMA_YATRI'
);

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

INSERT INTO atlas_app.issue_config (
    id,
    merchant_id,
    merchant_operating_city_id,
    auto_mark_issue_closed_duration,
    on_auto_mark_issue_cls_msgs,
    on_create_issue_msgs,
    on_issue_reopen_msgs,
    on_kapt_mark_issue_res_msgs
)
SELECT
    moc.id,
    moc.merchant_id as merchant_id,
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
ALTER TABLE atlas_app.issue_message ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_app.merchant_operating_city(id);
ALTER TABLE atlas_app.issue_message ADD COLUMN message_type text;
ALTER TABLE atlas_app.issue_message ADD COLUMN is_active boolean DEFAULT true NOT NULL;

-- QUERY FOR LOCAL
UPDATE atlas_app.issue_message
SET merchant_operating_city_id = (
    SELECT id
    FROM atlas_app.merchant_operating_city
    WHERE city = 'Kochi' AND merchant_short_id = 'NAMMA_YATRI'
);

-- QUERY FOR PROD AND MASTER
-- UPDATE atlas_app.issue_message
-- SET merchant_operating_city_id = (
--     SELECT id
--     FROM atlas_app.merchant_operating_city
--     WHERE city = 'Bangalore' AND merchant_short_id = 'NAMMA_YATRI'
-- );

ALTER TABLE atlas_app.issue_message
ALTER COLUMN merchant_operating_city_id SET NOT NULL;

UPDATE atlas_app.issue_message im
SET message_type =
    CASE
        WHEN im.label ILIKE '%CREATE_TICKET%'
            OR im.label ILIKE '%END_FLOW%'
            OR im.label ILIKE '%WARNING_MESSAGE%'
        THEN 'Terminal'

        WHEN EXISTS (
            SELECT 1
            FROM atlas_app.issue_option io
            WHERE io.issue_message_id = im.id
        )
        THEN 'Terminal'

        ELSE 'Intermediate'
    END;

--------------- Issue Translation Table ----------------------------------
ALTER TABLE atlas_app.issue_translation ADD COLUMN created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP  NOT NULL;
ALTER TABLE atlas_app.issue_translation ADD COLUMN updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP  NOT NULL;