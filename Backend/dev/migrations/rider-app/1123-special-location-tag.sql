ALTER TABLE atlas_app.estimate
  ADD column special_location_tag text;

ALTER TABLE atlas_app.quote
  ADD column special_location_tag text;

ALTER TABLE atlas_app.booking
  ADD column special_location_tag text;