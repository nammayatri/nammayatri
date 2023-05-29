CREATE TABLE atlas_driver_offer_bpp.search_try AS (
  SELECT * FROM atlas_driver_offer_bpp.search_request as T1
  WHERE T1.created_at > now () - interval '6 hour');
ALTER TABLE atlas_driver_offer_bpp.search_try ADD PRIMARY KEY (id);