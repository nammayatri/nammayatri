DELETE FROM atlas_app.organization
  WHERE type = 'APP';

ALTER TABLE atlas_app.organization
  DROP COLUMN name,
  DROP COLUMN description,
  DROP COLUMN mobile_number,
  DROP COLUMN mobile_country_code,
  DROP COLUMN gstin,
  DROP COLUMN domain,
  DROP COLUMN from_time,
  DROP COLUMN to_time,
  DROP COLUMN head_count,
  DROP COLUMN status,
  DROP COLUMN verified,
  DROP COLUMN enabled,
  DROP COLUMN api_key,
  DROP COLUMN callback_url,
  DROP COLUMN created_at,
  DROP COLUMN updated_at,
  DROP COLUMN callback_api_key,
  DROP COLUMN info;
