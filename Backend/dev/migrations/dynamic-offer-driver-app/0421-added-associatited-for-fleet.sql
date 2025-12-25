------------- Query to backfill data in fleet_rc_association table ----------------------------

INSERT INTO atlas_driver_offer_bpp.fleet_rc_association (id, rc_id, fleet_owner_id, merchant_id, merchant_operating_city_id, associated_on, associated_till, created_at, updated_at)
SELECT
    md5(random()::text || clock_timestamp()::text)::uuid AS id,
    vrc.Id AS rc_id,
    vrc.fleet_owner_id,
    vrc.merchant_id,
    vrc.merchant_operating_city_id,
    NOW() AS associated_on,
    to_timestamp('2099-12-12', 'YYYY-MM-DD') AT TIME ZONE 'UTC' AS associated_till,
    NOW() AS created_at,
    NOW() AS updated_at
FROM
    atlas_driver_offer_bpp.vehicle_registration_certificate AS vrc
WHERE
    vrc.fleet_owner_id IS NOT NULL;

------------------------------ This query will ensure that fleet vehicle will be only linked to fleet drivers ----------------------------
WITH temp1 AS (
    SELECT driver_id, rc_id
    FROM atlas_driver_offer_bpp.driver_rc_association
    WHERE rc_id IN (SELECT rc_id FROM atlas_driver_offer_bpp.fleet_rc_association)
),
temp2 AS (
    SELECT driver_id, rc_id
    FROM temp1
    WHERE driver_id NOT IN (SELECT driver_id FROM atlas_driver_offer_bpp.fleet_driver_association where is_active = true)
)

UPDATE atlas_driver_offer_bpp.driver_rc_association AS f
SET associated_till =  NOW(), is_rc_active = false
WHERE (f.driver_id, f.rc_id) IN (SELECT driver_id, rc_id FROM temp2);


----------- changing unique  constraints for person table ------------------------------------------------------

ALTER TABLE atlas_driver_offer_bpp.person drop CONSTRAINT person_unique_mobile_number_country_code;
ALTER TABLE atlas_driver_offer_bpp.person ADD CONSTRAINT person_unique_mobile_number_country_code_role  UNIQUE (merchant_id, mobile_country_code, mobile_number_hash, role);

-------- Adding Unique constraint in driver_pan table ----------------------------

ALTER TABLE atlas_driver_offer_bpp.driver_pan_card ADD CONSTRAINT unique_pan_card UNIQUE (pan_card_number_hash);