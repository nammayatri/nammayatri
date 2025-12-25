ALTER TABLE atlas_bpp_dashboard.person
ADD COLUMN password_updated_at TIMESTAMP WITH TIME ZONE DEFAULT now();