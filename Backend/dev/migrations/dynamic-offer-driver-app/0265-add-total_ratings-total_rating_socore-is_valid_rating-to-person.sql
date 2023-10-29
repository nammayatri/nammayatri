ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN total_ratings int NOT NULL DEFAULT 0;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN total_rating_score int NOT NULL DEFAULT 0;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN is_valid_rating boolean NOT NULL DEFAULT FALSE;



---- BACKFILL
WITH ratings_summary AS (
  SELECT
    driver_id,
    COALESCE(SUM(rating_value), 0) AS total_rating_score,
    COALESCE(COUNT(*), 0) AS total_ratings
  FROM
    atlas_driver_offer_bpp.rating
  GROUP BY
    driver_id
)

UPDATE atlas_driver_offer_bpp.person AS p
SET
  total_rating_score = rs.total_rating_score,
  total_ratings = rs.total_ratings
FROM ratings_summary AS rs
WHERE
  p.id = rs.driver_id;