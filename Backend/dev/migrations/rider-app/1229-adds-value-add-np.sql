-- NOTE: DON'T RUN IN MASTER & PROD
INSERT INTO atlas_app.value_add_np (enabled, subscriber_id)
SELECT
    true AS enabled,
    subscriber_id
FROM atlas_app.white_list_org;