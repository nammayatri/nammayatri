CREATE TABLE atlas_app.incentive_journey ();

ALTER TABLE atlas_app.incentive_journey ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.incentive_journey ADD COLUMN description text ;
ALTER TABLE atlas_app.incentive_journey ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.incentive_journey ADD COLUMN journey_type text NOT NULL default 'Daily';
ALTER TABLE atlas_app.incentive_journey ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.incentive_journey ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.incentive_journey ADD PRIMARY KEY ( id);
