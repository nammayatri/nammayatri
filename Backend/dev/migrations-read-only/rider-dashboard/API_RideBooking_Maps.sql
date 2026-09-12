-- {"api":"PostMapsAutoComplete","migration":"endpoint","param":"MapsAPI AutoCompleteEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/MAPS/POST_MAPS_AUTO_COMPLETE'
  WHERE endpoint = 'MapsAPI AutoCompleteEndPoint';

-- {"api":"PostMapsGetPlaceDetails","migration":"endpoint","param":"MapsAPI GetPlaceDetailsEndPoints","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/MAPS/POST_MAPS_GET_PLACE_DETAILS'
  WHERE endpoint = 'MapsAPI GetPlaceDetailsEndPoints';

-- {"api":"PostMapsGetPlaceName","migration":"endpoint","param":"MapsAPI GetPlaceNameEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/MAPS/POST_MAPS_GET_PLACE_NAME'
  WHERE endpoint = 'MapsAPI GetPlaceNameEndPoint';
