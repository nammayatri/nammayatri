UPDATE atlas_app.feedback_form
SET should_default = true
WHERE should_default IS NULL;

UPDATE atlas_app.feedback_form f
SET badges = (
  SELECT jsonb_agg(
           CASE WHEN elem ? 'shouldDefault' THEN elem
                ELSE elem || '{"shouldDefault": true}'::jsonb
           END
           ORDER BY ord)
  FROM jsonb_array_elements(f.badges) WITH ORDINALITY AS t(elem, ord)
)
WHERE f.badges IS NOT NULL
  AND jsonb_typeof(f.badges) = 'array'
  AND jsonb_array_length(f.badges) > 0
  AND EXISTS (
    SELECT 1 FROM jsonb_array_elements(f.badges) AS e(elem)
    WHERE NOT (e.elem ? 'shouldDefault')
  );
