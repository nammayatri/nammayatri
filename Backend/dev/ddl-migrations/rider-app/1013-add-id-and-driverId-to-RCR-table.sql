CREATE OR REPLACE FUNCTION atlas_app.uuid_generate_v4() RETURNS character (36) AS $uuid_generate_v4$
    BEGIN
        RETURN (uuid_in((md5((random())::text))::cstring));
    END;
$uuid_generate_v4$ LANGUAGE plpgsql;

ALTER TABLE ONLY atlas_app.ride_cancellation_reason
  ADD COLUMN id character(36);

ALTER TABLE ONLY atlas_app.ride_cancellation_reason
  ADD COLUMN ride_id character(36);

ALTER TABLE atlas_app.ride_cancellation_reason
  ALTER COLUMN id SET NOT NULL;

ALTER TABLE atlas_app.ride_cancellation_reason
   DROP CONSTRAINT ride_cancellation_reason_pkey
 , ADD  CONSTRAINT ride_booking_cancellation_reason_pkey PRIMARY KEY (id);

ALTER TABLE atlas_app.ride_cancellation_reason RENAME TO ride_booking_cancellation_reason;
