ALTER TABLE atlas_app.quote ADD COLUMN is_customer_preffered_search_route boolean;
ALTER TABLE atlas_app.quote ADD COLUMN is_blocked_route boolean;

ALTER TABLE atlas_app.estimate ADD COLUMN is_customer_preffered_search_route boolean;
ALTER TABLE atlas_app.estimate ADD COLUMN is_blocked_route boolean;