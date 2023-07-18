CREATE TABLE atlas_app.special_location (
id character(36) NOT NULL PRIMARY KEY,
location_name character varying(255) NOT NULL,
category character varying(255) NOT NULL,
gates text[] NOT NULL,
geom public.geometry(MultiPolygon),
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE TABLE atlas_app.tag_category_mapping (
id character(36) NOT NULL,
tag character varying(255) NOT NULL PRIMARY KEY,
category character varying(255) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


CREATE OR REPLACE FUNCTION add_to_array(input_array text[])
RETURNS text[]
AS $$
DECLARE
    output_array text[];
    i integer;
BEGIN

    -- Loop through each element in the input_array and concatenate 'A'
    FOR i IN 1..array_length(input_array, 1) LOOP
       IF position('address' IN input_array[i]) = 0 THEN
	      input_array[i] := trim(input_array[i]);
        input_array[i] := substring(input_array[i],1,length(input_array[i]) - 1) || ',address = Nothing}' ;
       End IF;
    END LOOP;

    RETURN input_array;
END;
$$
LANGUAGE plpgsql;




INSERT INTO atlas_app.special_location (id, location_name, category, gates, geom, created_at)
VALUES
  ( '1h8016d0-f9cd-4f9f-886f-bc4cbh6a8612'
  , 'Bangalore Airport'
  , 'SureAirport'
  , '{"GatesInfo{point = LatLong {lat = 10.155964166203688, lon = 76.389383717393}, name = \"Domestic Terminal\"}"}'
  , '01060000000100000001030000000100000013000000B4608E3E446B5340C8C9FABD1F6C2A4074CDBEC1846B5340000D098D14602A408A862A854A6D53407408A29AEF5F2A407FF5F8FE916D53403CB8DA9807602A4074C08F9EE96D53400434E9F4B25F2A407974C3755A6E534054974421175F2A40CAC438DD986E534055165D92DB5E2A40B6FA0F06156F53401A0435D1045E2A403674F00A436F5340A085F718AB5D2A40B5DA5EA8506F534076FF8CA8AF5D2A4096FD518F546F5340430EE3021E5E2A40DB6B54755B6F5340D64D4BC948602A4038C195D8766F5340602074892D612A40AE4D89A6496F5340283EE6ED35652A402CAD16C8226F534088543211016A2A4030E9746C086E5340889658540D6C2A4054F56E39326C534008E4B2D27B6C2A40F023E2EC5C6B5340B84A8C67696C2A40B4608E3E446B5340C8C9FABD1F6C2A40'
  , now()
  );

UPDATE atlas_app.special_location
SET gates = add_to_array(gates)