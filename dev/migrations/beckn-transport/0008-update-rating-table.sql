ALTER TABLE atlas_transporter.rating ADD COLUMN driver_id character(36);

UPDATE atlas_transporter.rating set driver_id = product_instance.person_id from atlas_transporter.rating r join atlas_transporter.product_instance on r.product_instance_id=product_instance.id;

ALTER TABLE atlas_transporter.rating ALTER COLUMN driver_id SET NOT NULL;