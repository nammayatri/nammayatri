DELETE FROM atlas_transporter.product_instance AS T1 WHERE T1.type = 'LOCATIONTRACKER';

UPDATE atlas_transporter.product_instance AS T1 
	SET case_id = (SELECT case_id FROM atlas_transporter.product_instance AS T2 WHERE T2.id = T1.parent_id)
	WHERE T1.type <> 'RIDESEARCH';

DELETE FROM atlas_transporter."case" AS T1 WHERE T1.type <> 'RIDESEARCH';