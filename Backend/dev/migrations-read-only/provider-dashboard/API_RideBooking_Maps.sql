-- {"api":"PostMapsAutoComplete","migration":"endpoint","param":"MapAPI AutoCompleteEndPoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/MAPS/POST_MAPS_AUTO_COMPLETE'
  WHERE endpoint = 'MapAPI AutoCompleteEndPoint';

-- {"api":"PostMapsGetPlaceName","migration":"endpoint","param":"MapAPI GetPlaceNameEndPoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/MAPS/POST_MAPS_GET_PLACE_NAME'
  WHERE endpoint = 'MapAPI GetPlaceNameEndPoint';
