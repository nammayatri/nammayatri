WITH CompletedEstimates AS (
  SELECT T1.id FROM atlas_app.estimate AS T1
  JOIN atlas_app.driver_offer AS T2
  ON T1.id = T2.estimate_id
  JOIN atlas_app.quote AS T3
  ON T2.id = T3.driver_offer_id
  JOIN atlas_app.booking AS T4
  ON T3.id = T4.quote_id
)
UPDATE atlas_app.estimate AS T1 SET status = 'COMPLETED' WHERE T1.id IN (SELECT * FROM CompletedEstimates);

UPDATE atlas_app.estimate AS T1 SET status = 'CANCELLED' WHERE T1.status IS NULL;
ALTER TABLE atlas_app.estimate ALTER COLUMN status SET NOT NULL;