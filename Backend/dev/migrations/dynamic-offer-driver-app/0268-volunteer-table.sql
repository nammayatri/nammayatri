-- Run in Master
INSERT INTO atlas_driver_offer_bpp.volunteer (
    id,
    created_at,
    updated_at,
    place,
    vendor_id,
    is_active
)
VALUES (
    '0ad46579-e881-4576-bc1e-c9a5b38fe785',
    NOW(),
    NOW(),
    'Kolkata Station Chitpur',
    'KOLKATA_STATION_CHITPUR',
    TRUE
)
ON CONFLICT (id) DO NOTHING;

-- Run in Prod
INSERT INTO atlas_driver_offer_bpp.volunteer (
    id, place, vendor_id, created_at, updated_at, is_active
)
VALUES
('b5ae3e17-5fd4-4dce-8c10-dc707b2fb211', 'Kolkata Airport', 'KOLKATA_AIRPORT', NOW(), NOW(), true),
('bf712937-7e8f-4842-8e03-09aa9ca3593d', 'Kolkata Airport', 'KOLKATA_AIRPORT', NOW(), NOW(), true),
('afc69896-4825-467f-b247-6c9d51830c30', 'Kolkata Airport', 'KOLKATA_AIRPORT', NOW(), NOW(), true),
('660088e2-a9bc-400d-a8d4-c04f02a5a741', 'Kolkata Airport', 'KOLKATA_AIRPORT', NOW(), NOW(), true),
('a26c1bce-6982-4143-9e66-46e8c54e5b0d', 'Santragachi', 'SANTRAGACHI', NOW(), NOW(), true),
('edd5b161-17e8-4f09-8a2e-8fb795112778', 'Santragachi', 'SANTRAGACHI', NOW(), NOW(), true),
('4c2a3947-52af-43f4-a63c-90e15de19f9a', 'Santragachi', 'SANTRAGACHI', NOW(), NOW(), true),
('289ad0dc-b344-4eec-ac4d-bc35d0ca5070', 'Santragachi', 'SANTRAGACHI', NOW(), NOW(), true),
('ddef0be1-d0ea-4d62-930f-43dcf6bd5804', 'Kolkata Station Chitpur', 'KOLKATA_STATION_CHITPUR', NOW(), NOW(), true),
('5ddc667a-25f6-4832-b47e-ea5032faa88a', 'Kolkata Station Chitpur', 'KOLKATA_STATION_CHITPUR', NOW(), NOW(), true),
('1e6e1222-2efe-4cb2-8590-2f317814e9ed', 'Kolkata Station Chitpur', 'KOLKATA_STATION_CHITPUR', NOW(), NOW(), true),
('8755ab3c-f904-4876-bda0-d75516d67efd', 'Kolkata Station Chitpur', 'KOLKATA_STATION_CHITPUR', NOW(), NOW(), true),
('d7401c00-bb36-4f12-94e0-111b46f19941', 'Sealdah Station', 'SEALDAH_STATION', NOW(), NOW(), true),
('beb168db-a824-49f8-a957-2c3a473049b0', 'Sealdah Station', 'SEALDAH_STATION', NOW(), NOW(), true),
('6c9909ce-2182-4695-8c52-0a18d3025e5d', 'Sealdah Station', 'SEALDAH_STATION', NOW(), NOW(), true),
('94322270-6547-4766-aa89-17a6231a02b0', 'Sealdah Station', 'SEALDAH_STATION', NOW(), NOW(), true),
('00dbe48f-0b06-4bae-8542-cab118c8c668', 'Howrah Station', 'HOWRAH_STATION', NOW(), NOW(), true),
('af6fefb8-fd41-4a3e-8ced-da0929e26c01', 'Howrah Station', 'HOWRAH_STATION', NOW(), NOW(), true),
('3b6185f4-d76f-4b75-8791-0c27e8d7fddd', 'Howrah Station', 'HOWRAH_STATION', NOW(), NOW(), true),
('8ee620c9-a99d-47ba-97aa-2a300a9986a2', 'Howrah Station', 'HOWRAH_STATION', NOW(), NOW(), true),
('c8d11fc7-923a-4940-b201-06f425c820cc', 'Howrah Station', 'HOWRAH_STATION', NOW(), NOW(), true),
('c86087b5-c79e-4e69-9e2c-bf762524c435', 'Howrah Station', 'HOWRAH_STATION', NOW(), NOW(), true),
('e242e9a8-bbd7-44a3-b41a-033af3c30ef9', 'Howrah Station', 'HOWRAH_STATION', NOW(), NOW(), true),
('4c996631-3c50-4c7d-b12e-8f666fb1c498', 'Howrah Station', 'HOWRAH_STATION', NOW(), NOW(), true),
('753ba278-d760-457e-9c2d-caad2b55b2da', 'Howrah Station', 'HOWRAH_STATION', NOW(), NOW(), true)
ON CONFLICT (id) DO NOTHING;
