CREATE TABLE yudhishthira.namma_tag ();

ALTER TABLE yudhishthira.namma_tag ADD COLUMN category text NOT NULL;
ALTER TABLE yudhishthira.namma_tag ADD COLUMN description text ;
ALTER TABLE yudhishthira.namma_tag ADD COLUMN chakra text ;
ALTER TABLE yudhishthira.namma_tag ADD COLUMN event text ;
ALTER TABLE yudhishthira.namma_tag ADD COLUMN tag_type text NOT NULL;
ALTER TABLE yudhishthira.namma_tag ADD COLUMN validity text ;
ALTER TABLE yudhishthira.namma_tag ADD COLUMN name text NOT NULL;
ALTER TABLE yudhishthira.namma_tag ADD COLUMN range_end double precision ;
ALTER TABLE yudhishthira.namma_tag ADD COLUMN range_start double precision ;
ALTER TABLE yudhishthira.namma_tag ADD COLUMN tags text[] ;
ALTER TABLE yudhishthira.namma_tag ADD COLUMN rule text NOT NULL;
ALTER TABLE yudhishthira.namma_tag ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE yudhishthira.namma_tag ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE yudhishthira.namma_tag ADD PRIMARY KEY ( name);