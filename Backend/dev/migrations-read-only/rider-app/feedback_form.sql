CREATE TABLE atlas_app.feedback_form ();

ALTER TABLE atlas_app.feedback_form ADD COLUMN answer text[] NOT NULL;
ALTER TABLE atlas_app.feedback_form ADD COLUMN answer_type character varying(255) NOT NULL;
ALTER TABLE atlas_app.feedback_form ADD COLUMN category_name character varying(255) NOT NULL;
ALTER TABLE atlas_app.feedback_form ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.feedback_form ADD COLUMN question character varying(255) NOT NULL;
ALTER TABLE atlas_app.feedback_form ADD COLUMN rating INT ;
ALTER TABLE atlas_app.feedback_form ADD PRIMARY KEY ( id);