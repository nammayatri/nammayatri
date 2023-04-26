CREATE TABLE atlas_transporter.rating_category (
  id character varying(255) NOT NULL,
  category character varying(255) NOT NULL,
  CONSTRAINT rating_category_pkey PRIMARY KEY (id)
);

INSERT INTO atlas_transporter.rating_category (id, category) VALUES (atlas_transporter.uuid_generate_v4(), 'RIDE');