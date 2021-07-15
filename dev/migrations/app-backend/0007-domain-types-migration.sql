UPDATE atlas_app.product_instance AS T1 
	SET case_id = (SELECT case_id FROM atlas_app.product_instance AS T2 WHERE T2.id = T1.parent_id)
	WHERE T1.type <> 'RIDESEARCH';

DELETE FROM atlas_app."case" AS T1 WHERE T1.type <> 'RIDESEARCH';

ALTER TABLE atlas_app."case" RENAME TO search_request;

ALTER TABLE atlas_app.product_instance RENAME COLUMN case_id TO request_id;

ALTER TABLE atlas_app.search_request DROP COLUMN parent_case_id;