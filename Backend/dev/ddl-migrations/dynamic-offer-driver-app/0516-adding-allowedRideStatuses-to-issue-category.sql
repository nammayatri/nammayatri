ALTER TABLE atlas_driver_offer_bpp.issue_category
ADD COLUMN allowed_ride_statuses text[];

ALTER TABLE atlas_driver_offer_bpp.issue_option
ADD COLUMN restricted_ride_statuses text[] DEFAULT '{}';