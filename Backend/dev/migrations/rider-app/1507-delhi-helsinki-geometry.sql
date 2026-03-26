-- =============================================================================
-- Geometry data for Delhi and Helsinki
-- Simplified city boundary polygons for local testing.
-- =============================================================================

-- Delhi NCR boundary
INSERT INTO atlas_app.geometry (id, region, city, state, geom)
SELECT 'delhi-geometry-0000-000000000001', 'Delhi NCR', 'Delhi', 'Delhi',
  ST_GeomFromText('MULTIPOLYGON(((
    76.8400 28.4000, 77.0000 28.4000, 77.1500 28.4500, 77.3500 28.5000,
    77.4000 28.5500, 77.4000 28.6500, 77.3800 28.7500, 77.3500 28.8500,
    77.2500 28.8800, 77.1000 28.8800, 76.9500 28.8500, 76.8500 28.7500,
    76.8200 28.6500, 76.8200 28.5500, 76.8400 28.4000
  )))')
WHERE NOT EXISTS (SELECT 1 FROM atlas_app.geometry WHERE id = 'delhi-geometry-0000-000000000001');

-- Helsinki metropolitan area boundary
INSERT INTO atlas_app.geometry (id, region, city, state, geom)
SELECT 'helsinki-geometry-000-000000000001', 'Helsinki Metropolitan', 'Helsinki', 'Uusimaa',
  ST_GeomFromText('MULTIPOLYGON(((
    24.5000 60.0500, 24.7000 60.0500, 24.9000 60.0800, 25.1000 60.1000,
    25.2500 60.1500, 25.3000 60.2500, 25.2500 60.3500, 25.1500 60.4000,
    25.0000 60.4200, 24.8000 60.4000, 24.6000 60.3500, 24.5000 60.2500,
    24.4500 60.1500, 24.5000 60.0500
  )))')
WHERE NOT EXISTS (SELECT 1 FROM atlas_app.geometry WHERE id = 'helsinki-geometry-000-000000000001');
