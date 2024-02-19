-- NOTE: DON'T RUN IN MASTER & PROD
INSERT INTO atlas_driver_offer_bpp.value_add_np (enabled, subscriber_id)
SELECT
    true AS enabled,
    subscriber_id
FROM atlas_driver_offer_bpp.white_list_org;