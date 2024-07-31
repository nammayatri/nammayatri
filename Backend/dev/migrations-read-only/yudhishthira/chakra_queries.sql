CREATE TABLE yudhishthira.chakra_queries ();

ALTER TABLE yudhishthira.chakra_queries ADD COLUMN chakra text NOT NULL;
ALTER TABLE yudhishthira.chakra_queries ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE yudhishthira.chakra_queries ADD COLUMN query_results text[] NOT NULL;
ALTER TABLE yudhishthira.chakra_queries ADD COLUMN query_text text NOT NULL;
ALTER TABLE yudhishthira.chakra_queries ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE yudhishthira.chakra_queries ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE yudhishthira.chakra_queries ADD PRIMARY KEY ( id);