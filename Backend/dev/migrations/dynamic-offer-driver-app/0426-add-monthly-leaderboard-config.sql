-- THIS QUERY SHOULD BE RUN IN BOTH MASTER AND PROD FOR MONTHLY LEADERBOARD CONFIGS
WITH unique_merchant_cities AS (
    SELECT DISTINCT
        merchant_id,
        merchant_operating_city_id
    FROM atlas_driver_offer_bpp.leader_board_configs
)
INSERT INTO atlas_driver_offer_bpp.leader_board_configs (
    id,
    leader_board_type,
    number_of_sets,
    leader_board_expiry,
    z_score_base,
    leader_board_length_limit,
    merchant_id,
    is_enabled,
    merchant_operating_city_id,
    created_at,
    updated_at,
    use_operating_city_based_leader_board
)
SELECT
    atlas_driver_offer_bpp.uuid_generate_v4() as id,
    'MONTHLY' AS leader_board_type,
    3 AS number_of_sets,
    2419200 AS leader_board_expiry,
    1000000000 AS z_score_base,
    10 AS leader_board_length_limit,
    merchant_id,
    TRUE AS is_enabled,
    merchant_operating_city_id,
    CURRENT_TIMESTAMP AS created_at,
    CURRENT_TIMESTAMP AS updated_at,
    TRUE AS use_operating_city_based_leader_board
FROM unique_merchant_cities;

-- UPDATING THE ZSCOREBASE FOR DAILY LEADERBOARD
UPDATE atlas_driver_offer_bpp.leader_board_configs
SET z_score_base = 1000000000
WHERE leader_board_type = 'DAILY';