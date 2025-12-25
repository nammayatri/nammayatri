CREATE OR REPLACE FUNCTION atlas_app.uuid_generate_v4() RETURNS character (36) AS $uuid_generate_v4$
    BEGIN
        RETURN (uuid_in((md5((random())::text))::cstring));
    END;
$uuid_generate_v4$ LANGUAGE plpgsql;

ALTER TABLE ONLY atlas_app.ride_cancellation_reason
  ADD COLUMN id character(36);

ALTER TABLE ONLY atlas_app.ride_cancellation_reason
  ADD COLUMN ride_id character(36);

UPDATE atlas_app.ride_cancellation_reason AS T1
  SET id = atlas_app.uuid_generate_v4()
  FROM atlas_app.booking AS T2
  WHERE T2.id = T1.ride_booking_id;

ALTER TABLE atlas_app.ride_cancellation_reason
  ALTER COLUMN id SET NOT NULL;

UPDATE atlas_app.ride_cancellation_reason AS T1
  SET ride_id = T2.id
  FROM atlas_app.ride AS T2
  WHERE T2.booking_id = T1.ride_booking_id;

ALTER TABLE atlas_app.ride_cancellation_reason
   DROP CONSTRAINT ride_cancellation_reason_pkey
 , ADD  CONSTRAINT ride_booking_cancellation_reason_pkey PRIMARY KEY (id);

ALTER TABLE atlas_app.ride_cancellation_reason RENAME TO ride_booking_cancellation_reason;
