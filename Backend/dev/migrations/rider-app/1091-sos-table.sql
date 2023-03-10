CREATE TABLE atlas_app.sos (
    id character(36) NOT NULL PRIMARY KEY,
    flow character varying(255),
    status character varying(36)  NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    person_id character (36) NOT NULL,
    ride_id character (36) NOT NULL,
    CONSTRAINT person_id_sos_fkey FOREIGN KEY (person_id) REFERENCES atlas_app.person(id),
    CONSTRAINT ride_id_sos_fkey FOREIGN KEY (ride_id) REFERENCES atlas_app.ride(id)
);