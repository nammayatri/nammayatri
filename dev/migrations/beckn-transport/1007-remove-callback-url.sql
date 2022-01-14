ALTER TABLE atlas_transporter.ride_booking ADD COLUMN bap_uri character varying(255);
UPDATE atlas_transporter.ride_booking AS T1 
   SET bap_uri = (SELECT bap_uri FROM atlas_transporter.search_request AS T2 WHERE T1.request_id = T2.id);
ALTER TABLE atlas_transporter.ride_booking ALTER COLUMN bap_uri SET NOT NULL;

ALTER TABLE atlas_transporter.organization DROP COLUMN callback_url;
ALTER TABLE atlas_transporter.organization DROP COLUMN callback_api_key;