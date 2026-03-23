-- FOR LOCAL USE ONLY
UPDATE atlas_app.sos
SET status = 'NotResolved',
    updated_at = NOW()
WHERE id IN (
  SELECT id
  FROM atlas_app.sos
  WHERE status = 'Pending'
  ORDER BY created_at
  LIMIT 2000
);
