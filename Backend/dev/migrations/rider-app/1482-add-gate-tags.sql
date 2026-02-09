-- Add gateTags column to gate_info table
ALTER TABLE atlas_app.gate_info ADD COLUMN gate_tags text[];
