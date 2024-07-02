-- issue category

ALTER TABLE atlas_app.issue_category ADD COLUMN merchant_id TEXT;
UPDATE atlas_app.issue_category SET merchant_id = (SELECT id FROM atlas_app.merchant LIMIT 1);

INSERT INTO atlas_app.issue_category (id, category, logo_url, priority, merchant_id)
SELECT CONCAT('', gen_random_uuid()), i.category, i.logo_url, i.priority, m.id
FROM atlas_app.issue_category i
CROSS JOIN atlas_app.merchant m
WHERE m.id NOT IN (SELECT DISTINCT merchant_id FROM atlas_app.issue_category);


-- issue config
ALTER TABLE atlas_app.issue_config ADD COLUMN merchant_id TEXT;
UPDATE atlas_app.issue_config SET merchant_id = (SELECT id FROM atlas_app.merchant LIMIT 1);

INSERT INTO atlas_app.issue_config (id, auto_mark_issue_closed_duration, on_auto_mark_issue_cls_msgs, on_create_issue_msgs, on_issue_reopen_msgs, on_kapt_mark_issue_res_msgs, merchant_id)
SELECT CONCAT('', gen_random_uuid()), i.auto_mark_issue_closed_duration, i.on_auto_mark_issue_cls_msgs, i.on_create_issue_msgs, i.on_issue_reopen_msgs, i.on_kapt_mark_issue_res_msgs, m.id
FROM atlas_app.issue_config i
CROSS JOIN atlas_app.merchant m
WHERE m.id NOT IN (SELECT DISTINCT merchant_id FROM atlas_app.issue_config);


-- issue message
ALTER TABLE atlas_app.issue_message ADD COLUMN merchant_id TEXT;
UPDATE atlas_app.issue_message SET merchant_id = (SELECT id FROM atlas_app.merchant LIMIT 1);

INSERT INTO atlas_app.issue_message (id, option_id, category_id, message, label, priority, merchant_id)
SELECT CONCAT('', gen_random_uuid()), i.option_id, i.category_id, i.message, i.label, i.priority, m.id
FROM atlas_app.issue_message i
CROSS JOIN atlas_app.merchant m
WHERE m.id NOT IN (SELECT DISTINCT merchant_id FROM atlas_app.issue_message);


-- issue option
ALTER TABLE atlas_app.issue_option ADD COLUMN merchant_id TEXT;
UPDATE atlas_app.issue_option SET merchant_id = (SELECT id FROM atlas_app.merchant LIMIT 1);

INSERT INTO atlas_app.issue_option (id, issue_category_id, issue_message_id, option, label, priority, merchant_id)
SELECT CONCAT('', gen_random_uuid()), i.issue_category_id, i.issue_message_id, i.option, i.label, i.priority, m.id
FROM atlas_app.issue_option i
CROSS JOIN atlas_app.merchant m
WHERE m.id NOT IN (SELECT DISTINCT merchant_id FROM atlas_app.issue_option);

-- issue report
ALTER TABLE atlas_app.issue_report ADD COLUMN merchant_id TEXT;
UPDATE atlas_app.issue_report SET merchant_id = (SELECT id FROM atlas_app.merchant LIMIT 1);

INSERT INTO atlas_app.issue_report (id, person_id, ride_id, description, assignee, status, category_id, option_id, deleted, media_files, ticket_id, chats, created_at, updated_at, driver_id, merchant_operating_city_id, short_id,  merchant_id)
SELECT CONCAT('', gen_random_uuid()), i.person_id, i.ride_id, i.description, i.assignee, i.status, i.category_id, i.option_id, i.deleted, i.media_files, i.ticket_id, i.chats, i.created_at, i.updated_at, i.driver_id, i.merchant_operating_city_id, i.short_id,  m.id
FROM atlas_app.issue_report i
CROSS JOIN atlas_app.merchant m
WHERE m.id NOT IN (SELECT DISTINCT merchant_id FROM atlas_app.issue_report);


-- issue translation
ALTER TABLE atlas_app.issue_translation ADD COLUMN merchant_id TEXT;
UPDATE atlas_app.issue_translation SET merchant_id = (SELECT id FROM atlas_app.merchant LIMIT 1);

INSERT INTO atlas_app.issue_translation ( id, sentence, translation, language, merchant_id)
SELECT CONCAT('', gen_random_uuid()), i.sentence, i.translation, i.language, m.id
FROM atlas_app.issue_translation i
CROSS JOIN atlas_app.merchant m
WHERE m.id NOT IN (SELECT DISTINCT merchant_id FROM atlas_app.issue_translation);


-- comment
ALTER TABLE atlas_app.comment ADD COLUMN merchant_id TEXT;
UPDATE atlas_app.comment SET merchant_id = (SELECT id FROM atlas_app.merchant LIMIT 1);

INSERT INTO atlas_app.comment (id, issue_report_id, author_id, comment, created_at,  merchant_id)
SELECT CONCAT('', gen_random_uuid()), i.issue_report_id, i.author_id, i.comment, i.created_at,  m.id
FROM atlas_app.comment i
CROSS JOIN atlas_app.merchant m
WHERE m.id NOT IN (SELECT DISTINCT merchant_id FROM atlas_app.comment);
