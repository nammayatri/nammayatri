INSERT INTO atlas_app.namma_tag_trigger (event, tag_name, created_at, updated_at)
  SELECT event, name, now(), now()
  FROM atlas_app.namma_tag
  WHERE event IS NOT NULL;
