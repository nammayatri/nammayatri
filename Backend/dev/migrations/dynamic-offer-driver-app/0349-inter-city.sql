-- Update state based on city
UPDATE atlas_driver_offer_bpp.merchant_operating_city
SET state = 'Karnataka'
WHERE city = 'Bangalore';

UPDATE atlas_driver_offer_bpp.merchant_operating_city
SET state = 'WestBengal'
WHERE city = 'Kolkata';

UPDATE atlas_driver_offer_bpp.merchant_operating_city
SET state = 'Kerala'
WHERE city = 'Kochi';

UPDATE atlas_driver_offer_bpp.merchant_operating_city
SET state = 'Delhi'
WHERE city = 'Delhi';

UPDATE atlas_driver_offer_bpp.merchant_operating_city
SET state = 'Telangana'
WHERE city = 'Hyderabad';

UPDATE atlas_driver_offer_bpp.merchant_operating_city
SET state = 'TamilNadu'
WHERE city = 'Chennai';

UPDATE atlas_driver_offer_bpp.merchant_operating_city
SET state = 'Karnataka'
WHERE city = 'Mysore';

UPDATE atlas_driver_offer_bpp.merchant_operating_city
SET state = 'Puducherry'
WHERE city = 'Pondicherry';

UPDATE atlas_driver_offer_bpp.merchant_operating_city
SET state = 'Karnataka'
WHERE city = 'Tumakuru';

UPDATE atlas_driver_offer_bpp.merchant_operating_city
SET state = 'UttarPradesh'
WHERE city = 'Noida';

UPDATE atlas_driver_offer_bpp.merchant_operating_city
SET state = 'Haryana'
WHERE city = 'Gurugram';

ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ALTER COLUMN state SET NOT NULL;

-- Update state based on city
UPDATE atlas_driver_offer_bpp.merchant
SET state = 'Karnataka'
WHERE city = 'Bangalore';

UPDATE atlas_driver_offer_bpp.merchant
SET state = 'WestBengal'
WHERE city = 'Kolkata';

UPDATE atlas_driver_offer_bpp.merchant
SET state = 'Kerala'
WHERE city = 'Kochi';

UPDATE atlas_driver_offer_bpp.merchant
SET state = 'Delhi'
WHERE city = 'Delhi';

UPDATE atlas_driver_offer_bpp.merchant
SET state = 'Telangana'
WHERE city = 'Hyderabad';

UPDATE atlas_driver_offer_bpp.merchant
SET state = 'TamilNadu'
WHERE city = 'Chennai';

UPDATE atlas_driver_offer_bpp.merchant
SET state = 'Karnataka'
WHERE city = 'Mysore';

UPDATE atlas_driver_offer_bpp.merchant
SET state = 'Puducherry'
WHERE city = 'Pondicherry';

UPDATE atlas_driver_offer_bpp.merchant
SET state = 'Karnataka'
WHERE city = 'Tumakuru';

UPDATE atlas_driver_offer_bpp.merchant
SET state = 'UttarPradesh'
WHERE city = 'Noida';

UPDATE atlas_driver_offer_bpp.merchant
SET state = 'Haryana'
WHERE city = 'Gurugram';

ALTER TABLE atlas_driver_offer_bpp.merchant ALTER COLUMN state SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.geometry ADD COLUMN state text;

-- Update state based on city
UPDATE atlas_driver_offer_bpp.geometry
SET state = 'Karnataka'
WHERE city = 'Bangalore';

UPDATE atlas_driver_offer_bpp.geometry
SET state = 'WestBengal'
WHERE city = 'Kolkata';

UPDATE atlas_driver_offer_bpp.geometry
SET state = 'Kerala'
WHERE city = 'Kochi';

UPDATE atlas_driver_offer_bpp.geometry
SET state = 'Delhi'
WHERE city = 'Delhi';

UPDATE atlas_driver_offer_bpp.geometry
SET state = 'Telangana'
WHERE city = 'Hyderabad';

UPDATE atlas_driver_offer_bpp.geometry
SET state = 'TamilNadu'
WHERE city = 'Chennai';

UPDATE atlas_driver_offer_bpp.geometry
SET state = 'Karnataka'
WHERE city = 'Mysore';

UPDATE atlas_driver_offer_bpp.geometry
SET state = 'Puducherry'
WHERE city = 'Pondicherry';

UPDATE atlas_driver_offer_bpp.geometry
SET state = 'Karnataka'
WHERE city = 'Tumakuru';

UPDATE atlas_driver_offer_bpp.geometry
SET state = 'UttarPradesh'
WHERE city = 'Noida';

UPDATE atlas_driver_offer_bpp.geometry
SET state = 'Haryana'
WHERE city = 'Gurugram';

ALTER TABLE atlas_driver_offer_bpp.geometry ALTER COLUMN state SET NOT NULL;

INSERT INTO atlas_driver_offer_bpp.merchant_state (allowed_destination_states, merchant_id, state)
SELECT ARRAY_AGG(DISTINCT state), merchant_id, state
FROM atlas_driver_offer_bpp.merchant_operating_city
GROUP BY merchant_id, state;
