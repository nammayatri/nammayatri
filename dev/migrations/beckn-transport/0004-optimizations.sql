CREATE INDEX IF NOT EXISTS ride_request_ride_id_idx ON atlas_transporter.ride_request (ride_id);

CREATE INDEX IF NOT EXISTS notification_status_ride_id_idx ON atlas_transporter.notification_status (ride_id);
CREATE INDEX IF NOT EXISTS notification_status_driver_id_idx ON atlas_transporter.notification_status (driver_id);
