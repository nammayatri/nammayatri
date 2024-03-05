ALTER TABLE atlas_app.booking ADD COLUMN num_stops int;

UPDATE atlas_app.booking SET num_stops = 0 where to_location_id is NULL;

UPDATE atlas_app.booking AS b
SET num_stops = subquery.max_order
FROM (
    SELECT entity_id, MAX("order") AS max_order
    FROM atlas_app.location_mapping
    WHERE entity_id IN (
        SELECT id
        FROM atlas_app.booking
        WHERE fare_product_type = 'RENTAL'
    )
    AND "order" > 1
    AND version = 'LATEST'
    GROUP BY entity_id
) AS subquery
WHERE b.id = subquery.entity_id;

UPDATE atlas_app.booking SET num_stops = 1 where num_stops is NULL;