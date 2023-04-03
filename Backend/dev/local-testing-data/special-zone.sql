INSERT INTO atlas_special_zone.special_zone (id, name, category_code, city, state, geom, geo_json, created_at, updated_at)
	VALUES ('9h8016d0-f9cd-4f9f-886f-bc4cbh6a8641', 'Random triangular shape', 'Airport', 'Bangalore', 'Karnataka' , (ST_SetSRID(ST_GeomFromGeoJSON(
'{
  "coordinates": [
    [
      [
        [
          26.74111671238296,
          -1.3083724031948663
        ],
        [
          26.75062111299397,
          -1.6029134155718907
        ],
        [
          27.01991246364392,
          -1.3432125421878993
        ],
        [
          26.74111671238296,
          -1.3083724031948663
        ]
      ]
    ]
  ],
  "type": "MultiPolygon"
}'),4326)),
'ShapeFile (ShapeFileType {_type = "FeatureCollection", features = [LocationFeature {_type = "Feature", properties = SzType {_type = Nothing, area = Nothing, address = Nothing}, geometry = Geometry {_type = MultiPolygon, coordinates = MPolyCoords [[[[26.74111671238296,-1.3083724031948663],[26.75062111299397,-1.6029134155718907],[27.01991246364392,-1.3432125421878993],[26.74111671238296,-1.3083724031948663]]]]}}]})', now(), now());

-- INSERT INTO atlas_bpp_dashboard.merchant (id, short_id, server_name, created_at) VALUES
--     ('sz2db186-39d3-48a4-ad1f-78a0c3f840fd', 'SPECIAL_ZONE', 'SPECIAL_ZONE', now ());

-- INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, created_at, updated_at) VALUES
--     ('2t4eb898-1410-40d3-8c1e-27c1087f4132', 'd5644e83-ffa3-4e0d-ae81-c3155eedb8fd', 'SPECIAL_ZONES', 'USER_FULL_ACCESS', now(), now());

-- INSERT INTO atlas_bpp_dashboard.registration_token (id, token, person_id, created_at, merchant_id) VALUES
--     ('8k666614-c6f1-48a8-ab16-23873b93f452', '141e0a7b-e521-4d6b-a0c4-8713ae345154', 'a77b0507-ae01-42dd-a075-264f59d89049', now (), 'sz2db186-39d3-48a4-ad1f-78a0c3f840fd');

-- INSERT INTO atlas_bpp_dashboard.merchant_access(id, person_id, created_at, merchant_id)
-- 	VALUES ('8k666614-c6f1-48a8-ab16-23267b93f452','a77b0507-ae01-42dd-a075-264f59d89049' , now(), 'sz2db186-39d3-48a4-ad1f-78a0c3f840fd' );
