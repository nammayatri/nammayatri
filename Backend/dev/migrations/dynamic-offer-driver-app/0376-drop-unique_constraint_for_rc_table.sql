ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate
DROP CONSTRAINT unique_rc_id;


------------- These are the update queries

WITH list_of_certificate_number_hash_with_maximum_fitness_expiry AS (
    SELECT certificate_number_hash, MAX(fitness_expiry) as fe
    FROM atlas_driver_offer_bpp.vehicle_registration_certificate
    GROUP BY certificate_number_hash
    HAVING COUNT(*) > 1
), id_mapping AS (
    SELECT
        a.id AS new_id,
        b.id AS old_id
    FROM atlas_driver_offer_bpp.vehicle_registration_certificate a
    JOIN atlas_driver_offer_bpp.vehicle_registration_certificate b ON a.certificate_number_hash = b.certificate_number_hash
    WHERE  (a.certificate_number_hash, a.fitness_expiry) IN (SELECT certificate_number_hash, fe FROM list_of_certificate_number_hash_with_maximum_fitness_expiry)
)
UPDATE atlas_driver_offer_bpp.driver_rc_association
SET rc_id = id_mapping.new_id
FROM id_mapping
WHERE atlas_driver_offer_bpp.driver_rc_association.rc_id = id_mapping.old_id;


------------- These are delete queries

WITH CTE AS (
    SELECT
        certificate_number_hash,
        fitness_expiry,
        ROW_NUMBER() OVER (PARTITION BY certificate_number_hash ORDER BY fitness_expiry DESC) AS rn
    FROM
        atlas_driver_offer_bpp.vehicle_registration_certificate
)
DELETE FROM
    atlas_driver_offer_bpp.vehicle_registration_certificate
WHERE
    (certificate_number_hash, fitness_expiry) IN (SELECT certificate_number_hash, fitness_expiry FROM CTE WHERE rn > 1);




ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate ADD CONSTRAINT unique_rc_id UNIQUE (certificate_number_hash);

