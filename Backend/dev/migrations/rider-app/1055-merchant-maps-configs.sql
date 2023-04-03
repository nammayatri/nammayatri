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

-- googleKey = "mock-google-key"
WITH MerchantMapsServiceConfigs AS (
  SELECT T1.id, 'Maps_Google', CAST ('{
   "googleMapsUrl":"http://localhost:8019/",
   "googleRoadsUrl":"http://localhost:8019/",
   "googleKey":"0.1.0|0|Cb8TKe1UWD9IkUDfVg4eE+moqGGHZIlhvXmWDqG/kS1RgdpRQF5Wxigi762BJhW5dKS9iHNnFKrZ2dm+vpuT61Bp"
  }' AS json)
  FROM atlas_app.merchant AS T1
)
INSERT INTO atlas_app.merchant_service_config (merchant_id, service_name, config_json)
  (SELECT * FROM MerchantMapsServiceConfigs);
