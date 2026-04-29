CREATE FUNCTION atlas_special_zone.no_overlaps_in_special_zone(id character(36), g geometry(MultiPolygon,4326))
RETURNS boolean AS $$
SELECT NOT EXISTS (
  SELECT 1 FROM atlas_special_zone.special_zone a
  WHERE a.id != $1
    AND a.geom && g
    AND ST_Relate(a.geom, g, '2********'));
$$ LANGUAGE sql;

ALTER FUNCTION atlas_special_zone.no_overlaps_in_special_zone(character(36), geometry) OWNER TO atlas_special_zone_user;

ALTER TABLE atlas_special_zone.special_zone ADD CONSTRAINT no_overlaps CHECK (atlas_special_zone.no_overlaps_in_special_zone(id, geom));

CREATE FUNCTION atlas_special_zone.geojson_to_bin(geojson text)
RETURNS text AS $$
select (ST_SetSRID(ST_GeomFromGeoJSON(geojson), 4326))::text;
$$ LANGUAGE sql;

ALTER FUNCTION atlas_special_zone.geojson_to_bin(text) OWNER TO atlas_special_zone_user;
