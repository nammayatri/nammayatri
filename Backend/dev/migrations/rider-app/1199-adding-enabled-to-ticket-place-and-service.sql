ALTER TABLE atlas_app.ticket_place ADD COLUMN enabled boolean ;
ALTER TABLE atlas_app.ticket_service ADD COLUMN enabled boolean ;

UPDATE atlas_app.ticket_place SET enabled = true;
UPDATE atlas_app.ticket_service SET enabled = true;