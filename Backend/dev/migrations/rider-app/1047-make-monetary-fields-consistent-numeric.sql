ALTER TABLE atlas_app.quote ALTER COLUMN discount TYPE numeric(30,2);
ALTER TABLE atlas_app.quote ALTER COLUMN distance_to_nearest_driver TYPE numeric(30,2);

ALTER TABLE atlas_app.booking ALTER COLUMN estimated_fare TYPE numeric(30,2);
ALTER TABLE atlas_app.booking ALTER COLUMN discount TYPE numeric(30,2);
ALTER TABLE atlas_app.booking ALTER COLUMN distance TYPE numeric(30,2);

ALTER TABLE atlas_app.search_request ALTER COLUMN distance TYPE numeric(30,2);
