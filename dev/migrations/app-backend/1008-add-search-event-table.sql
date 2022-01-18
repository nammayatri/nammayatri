CREATE TABLE atlas_app.on_search_event (
  id             character(36) PRIMARY KEY NOT NULL,
  bpp_id         character varying(255) NOT NULL,
  transaction_id character varying(255) NOT NULL,
  error_code     character varying(255),
  error_type     character varying(255),
  error_message  text,
  created_at     timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
