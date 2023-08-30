CREATE TABLE atlas_driver_offer_bpp.driver_home_location(
    id character(36) NOT NULL PRIMARY KEY,
    driver_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.person (id),
    lat float NOT NULL,
    lon float NOT NULL,
    home_address Text NOT NULL,
    tag Text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);

CREATE TABLE atlas_driver_offer_bpp.driver_go_home_request(
    id character(36) NOT NULL PRIMARY KEY,
    driver_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.person (id),
    lat float NOT NULL,
    lon float NOT NULL,
    point public.geography(Point,4326) NOT NULL,
    status character varying(36) NOT NULL,
    num_cancellation integer NOT NULL DEFAULT 0,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN go_home_request_id character(36);
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN go_home_request_id character(36);
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN driver_go_home_req_id character(36);
CREATE INDEX idx_goHome_request ON atlas_driver_offer_bpp.ride USING btree (driver_go_home_req_id);

