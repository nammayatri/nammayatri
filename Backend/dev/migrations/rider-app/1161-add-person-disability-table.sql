CREATE TABLE atlas_app.person_disability (
    person_id character(36) NOT NULL PRIMARY KEY REFERENCES atlas_app.person (id),
    disability_id character(36) NOT NULL,
    tag character varying(255) NOT NULL,
    description character varying(255) ,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
ALTER TABLE atlas_app.person_disability OWNER TO atlas_app_user;


ALTER TABLE atlas_app.search_request ADD COLUMN disability_tag character(255);

ALTER TABLE atlas_app.person ADD COLUMN has_disability boolean DEFAULT NULL;