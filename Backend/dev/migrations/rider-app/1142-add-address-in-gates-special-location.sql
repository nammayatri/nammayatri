CREATE OR REPLACE FUNCTION add_gates_to_array_rider(input_array text[])
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

UPDATE atlas_app.special_location
SET gates = add_gates_to_array_rider(gates)
Where array_length(gates, 1) > 0