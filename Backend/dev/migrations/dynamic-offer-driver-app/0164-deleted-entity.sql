CREATE TABLE atlas_driver_offer_bpp.deleted_entity (
  id character (36) NOT NULL PRIMARY KEY,
  primary_id varchar(255),
  table_name varchar(255) NOT NULL,
  row_data text NOT NULL,
  deleted_by varchar(255) NOT NULL,
  deleted_on timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
