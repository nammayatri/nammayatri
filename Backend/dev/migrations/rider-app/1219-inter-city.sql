-- Update state based on city
UPDATE atlas_app.merchant_operating_city
SET state = 'Karnataka'
WHERE city = 'Bangalore';

UPDATE atlas_app.merchant_operating_city
SET state = 'WestBengal'
WHERE city = 'Kolkata';

UPDATE atlas_app.merchant_operating_city
SET state = 'Kerala'
WHERE city = 'Kochi';

UPDATE atlas_app.merchant_operating_city
SET state = 'Delhi'
WHERE city = 'Delhi';

UPDATE atlas_app.merchant_operating_city
SET state = 'Telangana'
WHERE city = 'Hyderabad';

UPDATE atlas_app.merchant_operating_city
SET state = 'TamilNadu'
WHERE city = 'Chennai';

UPDATE atlas_app.merchant_operating_city
SET state = 'Karnataka'
WHERE city = 'Mysore';

UPDATE atlas_app.merchant_operating_city
SET state = 'Puducherry'
WHERE city = 'Pondicherry';

UPDATE atlas_app.merchant_operating_city
SET state = 'Karnataka'
WHERE city = 'Tumakuru';

UPDATE atlas_app.merchant_operating_city
SET state = 'UttarPradesh'
WHERE city = 'Noida';

UPDATE atlas_app.merchant_operating_city
SET state = 'Haryana'
WHERE city = 'Gurugram';

-- Update state based on city
UPDATE atlas_app.merchant
SET state = 'Karnataka'
WHERE city = 'Bangalore';

UPDATE atlas_app.merchant
SET state = 'WestBengal'
WHERE city = 'Kolkata';

UPDATE atlas_app.merchant
SET state = 'Kerala'
WHERE city = 'Kochi';

UPDATE atlas_app.merchant
SET state = 'Delhi'
WHERE city = 'Delhi';

UPDATE atlas_app.merchant
SET state = 'Telangana'
WHERE city = 'Hyderabad';

UPDATE atlas_app.merchant
SET state = 'TamilNadu'
WHERE city = 'Chennai';

UPDATE atlas_app.merchant
SET state = 'Karnataka'
WHERE city = 'Mysore';

UPDATE atlas_app.merchant
SET state = 'Puducherry'
WHERE city = 'Pondicherry';

UPDATE atlas_app.merchant
SET state = 'Karnataka'
WHERE city = 'Tumakuru';

UPDATE atlas_app.merchant
SET state = 'UttarPradesh'
WHERE city = 'Noida';

UPDATE atlas_app.merchant
SET state = 'Haryana'
WHERE city = 'Gurugram';

ALTER TABLE atlas_app.merchant ALTER COLUMN state SET NOT NULL;

ALTER TABLE atlas_app.geometry ADD COLUMN state text;

-- Update state based on city
UPDATE atlas_app.geometry
SET state = 'Karnataka'
WHERE city = 'Bangalore';

UPDATE atlas_app.geometry
SET state = 'WestBengal'
WHERE city = 'Kolkata';

UPDATE atlas_app.geometry
SET state = 'Kerala'
WHERE city = 'Kochi';

UPDATE atlas_app.geometry
SET state = 'Delhi'
WHERE city = 'Delhi';

UPDATE atlas_app.geometry
SET state = 'Telangana'
WHERE city = 'Hyderabad';

UPDATE atlas_app.geometry
SET state = 'TamilNadu'
WHERE city = 'Chennai';

UPDATE atlas_app.geometry
SET state = 'Karnataka'
WHERE city = 'Mysore';

UPDATE atlas_app.geometry
SET state = 'Puducherry'
WHERE city = 'Pondicherry';

UPDATE atlas_app.geometry
SET state = 'Karnataka'
WHERE city = 'Tumakuru';

UPDATE atlas_app.geometry
SET state = 'UttarPradesh'
WHERE city = 'Noida';

UPDATE atlas_app.geometry
SET state = 'Haryana'
WHERE city = 'Gurugram';

ALTER TABLE atlas_app.geometry ALTER COLUMN state SET NOT NULL;

-- CHECK IF ANY CITY LEFT
update atlas_app.merchant_operating_city set lat=28.457523, long=77.026344 where city = 'Gurugram';
update atlas_app.merchant_operating_city set lat=12.971599, long=77.594566 where city = 'Bangalore';
update atlas_app.merchant_operating_city set lat=12.971599, long=77.594566 where city = 'Kolkata';
update atlas_app.merchant_operating_city set lat=12.971599, long=77.594566 where city = 'Kochi';
update atlas_app.merchant_operating_city set lat=12.971599, long=77.594566 where city = 'Hyderabad';
update atlas_app.merchant_operating_city set lat=12.971599, long=77.594566 where city = 'Chennai';
update atlas_app.merchant_operating_city set lat=12.971599, long=77.594566 where city = 'Delhi';
update atlas_app.merchant_operating_city set lat=12.971599, long=77.594566 where city = 'Pondicherry';
update atlas_app.merchant_operating_city set lat=12.971599, long=77.594566 where city = 'Tumakuru';
update atlas_app.merchant_operating_city set lat=12.971599, long=77.594566 where city = 'Mysore';
update atlas_app.merchant_operating_city set lat=28.535517, long=77.391029 where city = 'Noida';

INSERT INTO atlas_app.merchant_state (allowed_destination_states, merchant_id, state)
SELECT ARRAY_AGG(DISTINCT state), merchant_id, state
FROM atlas_app.merchant_operating_city
GROUP BY merchant_id, state;