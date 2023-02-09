CREATE TABLE atlas_app.merchant_service_usage_config (
    merchant_id character(36) NOT NULL PRIMARY KEY REFERENCES atlas_app.merchant (id),
    get_distances character varying(30) NOT NULL,
    get_routes character varying(30) NOT NULL,
    snap_to_road character varying(30) NOT NULL,
    get_place_name character varying(30) NOT NULL,
    get_place_details character varying(30) NOT NULL,
    auto_complete character varying(30) NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE TABLE atlas_app.merchant_service_config (
    merchant_id character(36) NOT NULL REFERENCES atlas_app.merchant (id),
    service_name character varying(30) NOT NULL,
    config_json json NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY (merchant_id, service_name)
);

WITH MerchantMapsConfigs AS (
  SELECT T1.id, 'Google', 'Google', 'Google', 'Google', 'Google', 'Google'
  FROM atlas_app.merchant AS T1
)
INSERT INTO atlas_app.merchant_service_usage_config (merchant_id, get_distances, get_routes, snap_to_road, get_place_name, get_place_details, auto_complete)
  (SELECT * FROM MerchantMapsConfigs);

WITH MerchantMapsServiceConfigs AS (
  SELECT T1.id, 'Maps_Google', CAST ('{
   "googleMapsUrl":"https://maps.googleapis.com/maps/api/",
   "googleRoadsUrl":"https://roads.googleapis.com/",
   "googleKey":"0.1.0|2|S34+Lq69uC/hNeYSXr4YSjwwmaTS0jO/1ZGlAAwl72hBhgD9AAZhgI4o/6x3oi99KyJkQdt5UvgjlHyeEOuf1Z3xzOBqWBYVQM/RBggZ7NggTyIsDgiG5b3p"
  }' AS json)
  FROM atlas_app.merchant AS T1
)
INSERT INTO atlas_app.merchant_service_config (merchant_id, service_name, config_json)
  (SELECT * FROM MerchantMapsServiceConfigs);