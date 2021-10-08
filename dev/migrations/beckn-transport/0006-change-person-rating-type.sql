ALTER TABLE atlas_transporter.person ADD COLUMN rating1 float;

UPDATE atlas_transporter.person AS T1 
	SET rating1 = CAST (rating AS float) 
  WHERE rating IS NOT NULL;

ALTER TABLE atlas_transporter.person DROP COLUMN rating;
ALTER TABLE atlas_transporter.person RENAME COLUMN rating1 TO rating;