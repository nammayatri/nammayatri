ALTER TABLE atlas_transporter.booking_cancellation_reason DROP COLUMN id;
ALTER TABLE atlas_transporter.booking_cancellation_reason ADD PRIMARY KEY (booking_id)