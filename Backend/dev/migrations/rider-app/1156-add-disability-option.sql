ALTER TABLE atlas_app.person ADD COLUMN disability_id character varying(36);

CREATE TABLE atlas_app.disability_type (
  id character varying(36) NOT NULL PRIMARY KEY,
  tag character varying(255) NOT NULL,
  subtag character varying(255) NOT NULL,
  description character varying(1056),
  onBookingMessage character varying(1056),
  onArrivalMessage character varying(1056),
  onRideStartMessage character varying(1056),
  created_at timestamp with time zone DEFAULT now() NOT NULL
);


INSERT INTO atlas_app.disability_type (id , tag, subtag, description, onBookingMessage, onArrivalMessage, onRideStartMessage, created_at) VALUES
    ('4d019461d2b0-28ba-456d-8cca-18911beb', 'BLIND_LOW_VISION' , '', 'Blind / Low Vision', 'Customer has poor vision Please call and avoid chats' , 'Customer has poor vision Sound horn once at pickup' ,'Please help them as you can', '2023-08-23 15:15:42.233691+00'),
    ('5d019461d2b0-28ba-456d-8cca-18911beb', 'HEAR_IMPAIRMENT' , '', 'Hearing Impairment (Deaf / Mute)', 'Customer has Hearing Impairment Please chat and avoid calls' , 'Customer has Hearing Impairment dont horn once at pickup' ,'Please help them as you can', '2023-08-23 15:15:42.233691+00'),
    ('6d019461d2b0-28ba-456d-8cca-18911beb', 'LOCOMOTOR_DISABILITY' , '', 'Locomotor Diability', 'Customer has Locomotor Diability Please help' , 'Customer has Locomotor Diability Please help at pickup' ,'Please help them as you can', '2023-08-23 15:15:42.233691+00');