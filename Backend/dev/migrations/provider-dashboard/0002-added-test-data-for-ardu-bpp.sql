INSERT INTO atlas_bpp_dashboard.server_access (id, person_id, server_name, created_at)
    SELECT
        md5(random()::text || clock_timestamp()::text)::uuid,
        T1.id,
        'DRIVER_OFFER_BPP',
        '2022-09-12 15:15:42.104639+00'
    FROM atlas_bpp_dashboard.person AS T1;

------------------------

INSERT INTO atlas_bpp_dashboard.registration_token (id, token, person_id, created_at, server_name) VALUES
	('8166bbcb-090d-4825-9614-05eaed970435', '7a207d9a-0fcd-49f1-86dd-2292ab6af5f2', '25e77f37-75e5-4665-8ed0-4be2af35940a', '2022-10-12 18:24:19.296141+00', 'DRIVER_OFFER_BPP'),
	('acfd0038-7b4a-4345-8776-312d9d95d38e', '65913d60-41a8-41f5-a35f-92f9b8af030d', 'cd69ae25-1641-4e6d-b9f4-5bae63e2a537', '2022-10-12 18:26:44.056945+00', 'DRIVER_OFFER_BPP'),
	('ab272593-88f7-4b56-b9b8-9e70cf96f347', 'b0ff7e6b-c101-4234-94fa-32985c4dc0e1', 'a77b0507-ae01-42dd-a075-264f59d89049', '2022-10-12 18:26:50.888013+00', 'DRIVER_OFFER_BPP'),
	('cfa29ea7-6b24-4cfd-bfce-2c9cbe7ed2f7', '1460d294-8c69-44b9-811a-c370949d056e', '3680f4b5-dce4-4d03-aa8c-5405690e87bd', '2022-10-12 18:26:53.856197+00', 'DRIVER_OFFER_BPP'),
	('79abe0b0-ddfd-499b-9886-711f04b27002', '5425f717-3838-476a-a6ed-58a7a71c6c57', '59f5bd8c-8268-4e8e-bcab-c21da7e496d4', '2022-10-12 18:26:59.866588+00', 'DRIVER_OFFER_BPP');

