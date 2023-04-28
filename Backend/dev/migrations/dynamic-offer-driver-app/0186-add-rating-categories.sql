CREATE TABLE atlas_driver_offer_bpp.rating_category (
  id character(36) NOT NULL,
  category character varying(255) NOT NULL,
  CONSTRAINT rating_category_pkey PRIMARY KEY (id)
);

INSERT INTO atlas_driver_offer_bpp.rating_category (id, category) VALUES (atlas_driver_offer_bpp.uuid_generate_v4(), 'RIDE');

CREATE TABLE atlas_driver_offer_bpp.feedback_form (
  id character(36) NOT NULL,
  category_id character varying(255) NOT NULL REFERENCES atlas_driver_offer_bpp.rating_category (id),
  rating_value integer NOT NULL,
  question text NOT NULL,
  answer_type character varying(255) NOT NULL,
  CONSTRAINT feedback_form_pkey PRIMARY KEY (id)
);

WITH a as (select id from atlas_driver_offer_bpp.rating_category)
INSERT INTO atlas_driver_offer_bpp.feedback_form (id,category_id, rating_value, question, answer_type)
  VALUES (atlas_driver_offer_bpp.uuid_generate_v4(), (select id from a), 1, 'first question', 'TEXT'),
         (atlas_driver_offer_bpp.uuid_generate_v4(), (select id from a), 2, 'second question', 'TEXT'),
         (atlas_driver_offer_bpp.uuid_generate_v4(), (select id from a), 3, 'third question', 'TEXT'),
         (atlas_driver_offer_bpp.uuid_generate_v4(), (select id from a), 4, 'fourth question', 'TEXT'),
         (atlas_driver_offer_bpp.uuid_generate_v4(), (select id from a), 5, 'fifth question', 'TEXT');
