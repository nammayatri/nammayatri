ALTER TABLE  atlas_app.ticket_place ADD COLUMN short_desc VARCHAR(255) DEFAULT 'dummy' NOT NULL ;
ALTER TABLE  atlas_app.ticket_place ADD COLUMN icon_url VARCHAR(255);
ALTER TABLE  atlas_app.ticket_place ADD COLUMN map_image_url VARCHAR(255);


-- Dont run the below query for master / prod insteadupdate actual data that should be there
UPDATE atlas_app.ticket_place SET short_desc = 'Nice Place', icon_url = 'http://localhost', map_image_url = 'http://localhost';