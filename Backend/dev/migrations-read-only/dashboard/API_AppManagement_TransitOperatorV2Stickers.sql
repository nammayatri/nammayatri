-- {"api":"TransitOperatorV2ListStickerDesigns","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_LIST_STICKER_DESIGNS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2CreateStickerDesign","migration":"capability","param":"transit-operations.master.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_CREATE_STICKER_DESIGN' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2PreviewSticker","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_PREVIEW_STICKER' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2GetStickerSigning","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_GET_STICKER_SIGNING' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2ReadStickerFleetFile","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_READ_STICKER_FLEET_FILE' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2GetStickerDesign","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_GET_STICKER_DESIGN' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2UpdateStickerDesign","migration":"capability","param":"transit-operations.master.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_UPDATE_STICKER_DESIGN' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2DeleteStickerDesign","migration":"capability","param":"transit-operations.master.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_DELETE_STICKER_DESIGN' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2GetStickerBackground","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_GET_STICKER_BACKGROUND' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2DuplicateStickerDesign","migration":"capability","param":"transit-operations.master.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_DUPLICATE_STICKER_DESIGN' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2GenerateStickers","migration":"capability","param":"transit-operations.master.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_GENERATE_STICKERS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2ListStickerBatches","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_LIST_STICKER_BATCHES' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2ListStickerFonts","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_LIST_STICKER_FONTS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2UploadStickerFont","migration":"capability","param":"transit-operations.master.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_UPLOAD_STICKER_FONT' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2DeleteStickerFont","migration":"capability","param":"transit-operations.master.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_DELETE_STICKER_FONT' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2GetStickerFontFile","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_GET_STICKER_FONT_FILE' ) ON CONFLICT DO NOTHING;
