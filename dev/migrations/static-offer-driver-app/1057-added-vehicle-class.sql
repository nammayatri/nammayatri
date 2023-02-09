ALTER TABLE atlas_transporter.vehicle ADD COLUMN vehicle_class CHARACTER varying(255);
UPDATE atlas_transporter.vehicle SET vehicle_class = '3WT';
ALTER TABLE atlas_transporter.vehicle ALTER COLUMN vehicle_class SET NOT NULL;
