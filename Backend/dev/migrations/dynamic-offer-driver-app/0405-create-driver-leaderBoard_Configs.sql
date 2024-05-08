-- DON'T RUN THIS FILE IN MASTER OR PROD, ONLY FOR LOCAL:
INSERT INTO atlas_driver_offer_bpp.leader_board_configs (id, leader_board_type, number_of_sets, leader_board_expiry, z_score_base, leader_board_length_limit, merchant_id, is_enabled, merchant_operating_city_id)
SELECT
    '1' AS id,
    'DAILY' AS leader_board_type,
    7 AS number_of_sets,
    86400 AS leader_board_expiry,
    100000000 AS z_score_base,
    10 AS leader_board_length_limit,
    'favorit0-0000-0000-0000-00000favorit' AS merchant_id,
    true AS is_enabled,
    moc.id AS merchant_operating_city_id
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc
WHERE
    moc.merchant_id = 'favorit0-0000-0000-0000-00000favorit';

INSERT INTO atlas_driver_offer_bpp.leader_board_configs (id, leader_board_type, number_of_sets, leader_board_expiry, z_score_base, leader_board_length_limit, merchant_id, is_enabled, merchant_operating_city_id)
SELECT
    '2' AS id,
    'WEEKLY' AS leader_board_type,
    4 AS number_of_sets,
    604800 AS leader_board_expiry,
    1000000000 AS z_score_base,
    10 AS leader_board_length_limit,
    'favorit0-0000-0000-0000-00000favorit' AS merchant_id,
    true AS is_enabled,
    moc.id AS merchant_operating_city_id
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc
WHERE
    moc.merchant_id = 'favorit0-0000-0000-0000-00000favorit';

INSERT INTO atlas_driver_offer_bpp.leader_board_configs (id, leader_board_type, number_of_sets, leader_board_expiry, z_score_base, leader_board_length_limit, merchant_id, is_enabled, merchant_operating_city_id)
SELECT
    '3' AS id,
    'MONTHLY' AS leader_board_type,
    3 AS number_of_sets,
    2419200 AS leader_board_expiry,
    1000000000 AS z_score_base,
    10 AS leader_board_length_limit,
    'favorit0-0000-0000-0000-00000favorit' AS merchant_id,
    true AS is_enabled,
    moc.id AS merchant_operating_city_id
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc
WHERE
    moc.merchant_id = 'favorit0-0000-0000-0000-00000favorit';