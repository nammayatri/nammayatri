ALTER TABLE atlas_driver_offer_bpp.ride
DROP COLUMN number_of_deviation;


ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN number_of_deviation BOOL;

-- in prod run only 2nd SQL statement.
-- in master run the both SQL statement