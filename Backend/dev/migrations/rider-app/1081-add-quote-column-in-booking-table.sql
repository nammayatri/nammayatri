ALTER TABLE atlas_app.booking ADD COLUMN quote_id character(36) REFERENCES atlas_app.quote (id) ;
