
update atlas_app.saved_location set place_id=split_part(country, ':', 2);
update atlas_app.saved_location set country=split_part(country, ':', 1);