-----***************************--------
-- THIS WHOLE FILE IS FOR LOCAL TESTING. PLEASE DON'T RUN IT
-----***************************--------


-- ONLY FOR LOCAL TESTING --
DELETE FROM atlas_app.merchant_service_config WHERE service_name = 'MetroPayment_Juspay';

-- ONLY FOR LOCAL TESTING --
DELETE FROM atlas_app.beckn_config WHERE vehicle_category = 'METRO';

-- ONLY FOR LOCAL TESTING --
INSERT INTO
  atlas_app.merchant_service_config (
    merchant_id,
    service_name,
    config_json,
    merchant_operating_city_id
  )
VALUES
  (
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'MetroPayment_Juspay',
    '{"apiKey": "xxxxx", "returnUrl": "www.google.com", "url": "www.google.com", "merchantId": "nammayatri", "username": "nammayatri", "password": "xxxx"}',
    'namma-yatri-0-0000-0000-00000000city'
  );

-- ONLY FOR LOCAL TESTING --
INSERT INTO
  atlas_app.beckn_config(
    bap_ifsc,
    buyer_finder_fee,
    collected_by,
    confirm_buffer_ttl_sec,
    confirm_ttl_sec,
    domain,
    gateway_url,
    id,
    init_ttl_sec,
    payment_params_json,
    registry_url,
    search_ttl_sec,
    select_ttl_sec,
    settlement_type,
    settlement_window,
    static_terms_url,
    subscriber_id,
    subscriber_url,
    vehicle_category,
    merchant_id,
    merchant_operating_city_id,
    created_at,
    updated_at,
    track_ttl_sec,
    status_ttl_sec,
    rating_ttl_sec,
    cancel_ttl_sec,
    unique_key_id
  )
VALUES
  (
    null,
    null,
    'BAP',
    10,
    120,
    'FRFS',
    'http://localhost:8015/v1',
    'dd22a05d-29a3-42c8-9c8d-3de340f93333',
    120,
    '{"bankAccNumber": "xyz@upi","bankCode": "xyz"}',
    'http://localhost:8020',
    120,
    120,
    null,
    null,
    null,
    'localhost:8013/beckn/frfs/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'http://localhost:8013/beckn/frfs/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'METRO',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    now(),
    now(),
    30,
    30,
    120,
    30,
    'localhost/beckn/frfs/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a52'
  );

-- ONLY FOR LOCAL TESTING --

-- MULTIMODAL
INSERT INTO
  atlas_app.integrated_bpp_config(
    id,
    domain,
    feed_key,
    agency_key,
    merchant_id,
    merchant_operating_city_id,
    vehicle_category,
    platform_type,
    config_json,
    created_at,
    updated_at
  )
VALUES
  (
    'abce23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'FRFS',
    'chennai_metro',
    'chennai_metro:CMRL',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    'METRO',
    'MULTIMODAL',
    '{"tag": "CMRL", "contents": {"username": "xxxx", "password": "xxxxx", "networkHostUrl": "www.google.com"}}',
    now(),
    now()
  );

INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('13e531d4-20fc-442c-a9b9-b9f5ddeb1613', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '1104', 'Blue', 'Wimco Nagar Metro - Airport', 13.0817804, 80.273549, 12.981593, 80.1639009, 's_znAum}hNfgAx@BB??xBvCTV??l@`@??f@\??JD??XL??~@d@??RT??Z^??p@pA??RZ??DF??RP??pAjAbp@ncAhsAtdAJHRP??v@H??ZD??xAP??t@H??zC^??~AP??~@J??TB??J@rhA~m@tj@fhA~@nA~@lA??LNLNZX~ArA??~CjCtChB~gAtlAnc@nhA`@L??FDBBDH@B?D??@BA@?BIP??Yr@??`Df@??ZJ^N\NJF??FD??PLLLLN^p@??J^H^BV@`@AR?`@~vAvoCjd@biA', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('93907fe4-5497-4636-b886-dc3e9696811a', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '2214', 'Blue', 'Puratchi Thalaivar Dr.M.G.Ramachandran Central Metro - Airport', 13.035342, 80.2106365, 12.981593, 80.1639009, 'e|pnAacqhNboBz]~rAbX`@L??FDBBDH@B?D??@BA@?BIP??Yr@??`Df@??ZJ^N\NJF??FD??PLLLLN^p@??J^H^BV@`@AR?`@~vAvoCjd@biA', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('485c0323-b87d-4c35-8289-e1d539c7a01c', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '1101', 'Blue', 'Wimco Nagar Metro - Airport', 13.1065733, 80.2807641, 12.981593, 80.1639009, 'c{~nAuy~hNdaAa\pZrCv|@lcAfgAx@BB??xBvCTV??l@`@??f@\??JD??XL??~@d@??RT??Z^??p@pA??RZ??DF??RP??pAjAbp@ncAhsAtdAJHRP??v@H??ZD??xAP??t@H??zC^??~AP??~@J??TB??J@??rAP??dBPxbAzl@tj@fhA~@nA~@lA??LNLNZX~ArA??~CjCtChB~gAtlAnc@nhA`@L??FDBBDH@B?D??@BA@?BIP??Yr@??`Df@??ZJ^N\NJF??FD??PLLLLN^p@??J^H^BV@`@AR?`@~vAvoCjd@biA', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('686e11cc-4058-4093-9d61-f97a7c883f09', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '102146', 'Blue', 'Airport - Wimco Nagar Metro', 13.1426625, 80.2960026, 13.1789252, 80.3071406, 'q|eoAiyaiNA???yBu@??iA]??m@W??ME??i@OsoDgv@GZ??}AY??kB]??]G??eB]??]G??aAO??IAm@M??}AY', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('30a62f9f-1b22-48db-a5a1-27e97d323ae7', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '102103', 'Blue', 'Airport - Wimco Nagar Metro', 13.0878626, 80.2853306, 13.1789252, 80.3071406, 'mf{nA}t_iNa^yDstA``@kw@eVA@??c@tA??w@lB??kAu@??YSQK??SO??cB_A??wBoA??OI??kAo@]Uc`B{n@c@M??m@S??eDcA??yDeAo{Awd@{dCwc@GZ??}AY??kB]??]G??eB]??]G??aAO??IAm@M??}AY??IA??aEu@', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('dddc5527-5712-41a6-ba8b-dd4f34586f3f', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '1108', 'Blue', 'Wimco Nagar Metro - Airport', 13.0459404, 80.2485367, 12.981593, 80.1639009, 'k`snA}oxhNJHRP??v@H??ZD??xAP??t@H??zC^??~AP??~@J??TB??J@??rAP??dBPxbAzl@tj@fhA~@nA~@lA??LNLNZX~ArA??~CjCtChB~gAtlAnc@nhA`@L??FDBBDH@B?D??@BA@?BIP??Yr@??`Df@??ZJ^N\NJF??FD??PLLLLN^p@??J^H^BV@`@AR?`@~vAvoCjd@biA', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('3bd05816-3e5b-472a-9e63-6a32ac933c58', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '24209', 'Green', 'St.Thomas Mount - Puratchi Thalaivar Dr.M.G.Ramachandran Central Metro', 13.0849977, 80.2022092, 13.0817804, 80.273549, 'avznA{nohNb@o^bA_oA@q@??b@???D_C??@m@??@g@??@cA??@Y???g@??@_@??@a@??@]???u@??C]?C??JE??N?f@F??jAJ`q@caAuD{[oL{nAAo@???Y??CcA??@K??Cu@???a@??BaA??@K???Y??@[???S???eA{Pe_C', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('eaea5540-9c27-4e18-ba48-53d214098de4', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '17211', 'Green', 'Puratchi Thalaivar Dr.M.G.Ramachandran Central Metro - St.Thomas Mount', 13.0682418, 80.2041187, 12.9947486, 80.1975543, 'gjwnAazohNL[NSNSNONK??v@]??r@M??~IMzyA_j@jaBxEboBz]~rAbX`@L??FDBBDH@B?D??@BA@?BIP??Yr@??`Df@??ZJ^N\NJF??bAC??^B??Z@??VC??lFD', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('f5cdae25-3c01-4a3f-a0c1-e88ec25dfe89', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '18201', 'Green', 'Puratchi Thalaivar Dr.M.G.Ramachandran Central Metro - St.Thomas Mount', 13.0817804, 80.273549, 12.9947486, 80.1975543, 's_znAum}hNdP~mADh_ANA??AaA???Y??CcA??@K??RA??@f@??@fA??@tA??@fB??FjALvAD\fQ~`BkE|Nqq@`cAc@jgAt@ha@lz@dp@pk@qv@L[NSNSNONK??v@]??r@M??~IMzyA_j@jaBxEboBz]~rAbX`@L??FDBBDH@B?D??@BA@?BIP??Yr@??`Df@??ZJ^N\NJF??bAC??^B??Z@??VC??lFD', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('103f088d-c3b4-48ae-bac1-d230ba04270b', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '109108', 'Blue', 'Airport - Wimco Nagar Metro', 13.0459404, 80.2485367, 13.1789252, 80.3071406, 'o`snAqnxhNerAmeAU[??k@w@??IKY[??IM??_@g@??W]??s@{@??GG??OQ??u@cAcx@ukAohAUye@gfA{AS??SE??c@S??k@Ew@I??iAK??[C??_AKUA??s@G??]C??o@G_bBv]kw@eVA@??c@tA??w@lB??kAu@??YSQK??SO??cB_A??wBoA??OI??kAo@]Uc`B{n@c@M??m@S??eDcA??yDeAo{Awd@{dCwc@GZ??}AY??kB]??]G??eB]??]G??aAO??IAm@M??}AY??IA??aEu@', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('d269622c-138f-498f-878f-d8ab83b8db5d', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '17210', 'Green', 'Puratchi Thalaivar Dr.M.G.Ramachandran Central Metro - St.Thomas Mount', 13.0753312, 80.1950662, 12.9947486, 80.1975543, 'yvxnAobnhNpk@qv@L[NSNSNONK??v@]??r@M??~IMzyA_j@jaBxEboBz]~rAbX`@L??FDBBDH@B?D??@BA@?BIP??Yr@??`Df@??ZJ^N\NJF??bAC??^B??Z@??VC??lFD', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('cac4fa8a-0f5f-477d-a915-46b8de6ca975', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '29117', 'Green', 'Airport - Puratchi Thalaivi Dr.J.Jayalalithaa CMBT Metro', 12.981593, 80.1639009, 13.0682418, 80.2041187, 'knfnAy`hhNkd@ciAokA{nBmLkZw}A{k@wnB__@kaByE_dAaAee@`q@', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('9a0fcb06-0909-4d4b-b897-ef46e40ab48d', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '29217', 'Green', 'St.Thomas Mount - Puratchi Thalaivi Dr.J.Jayalalithaa CMBT Metro', 12.9947486, 80.1975543, 13.0682418, 80.2041187, 'wainAaqnhNK_@Qu@??SA??M?e@G??_@A??UA??_@G??[G??a@A??[A??Aq@?e@??K???W@??[B??yAFc|B}j@wnB__@kaByE_dAaAee@`q@', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('afcd276e-a0cb-4955-8a22-a9e80e778b5e', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '109160', 'Blue', 'Airport - Wimco Nagar Metro', 13.0817804, 80.273549, 13.1789252, 80.3071406, 's_znAum}hNye@gfA{AS??SE??c@S??k@Ew@I??iAK??[C??_AKUA??s@G??]C??o@G??uBUi~Al^kw@eVA@??c@tA??w@lB??kAu@??YSQK??SO??cB_A??wBoA??OI??kAo@]Uc`B{n@c@M??m@S??eDcA??yDeAo{Awd@{dCwc@GZ??}AY??kB]??]G??eB]??]G??aAO??IAm@M??}AY??IA??aEu@', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('eb471656-6a54-4e97-b745-5e9bc95e6210', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '23214', 'Green', 'St.Thomas Mount - Puratchi Thalaivar Dr.M.G.Ramachandran Central Metro', 13.035342, 80.2106365, 13.0817804, 80.273549, 'e|pnAacqhNkaByE_dAaAee@`q@al@jv@g~@kk@b@o^bA_oA@q@??b@???D_C??@m@??@g@??@cA??@Y???g@??@_@??@a@??@]???u@??C]?C??JE??N?f@F??jAJ`q@caAuD{[oL{nAAo@???Y??CcA??@K??Cu@???a@??BaA??@K???Y??@[???S???eA{Pe_C', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('d427b52a-a811-4fea-87e9-8bae05b33ab2', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '23258', 'Green', 'St.Thomas Mount - Puratchi Thalaivar Dr.M.G.Ramachandran Central Metro', 13.0849977, 80.2022092, 13.0817804, 80.273549, 'avznA{nohNb@o^bA_oA@q@??b@???D_C??@m@??@g@??@cA??@Y???g@??@_@??@a@??@]???u@??C]?C??JE??N?f@F??jAJ`q@caAuD{[oL{nAAo@???Y??CcA??@K??Cu@???a@??BaA??@K???Y??@[???S???eA{Pe_C', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('5e4ad317-afcb-48df-ad9e-0f5ce9a8f8bd', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '23117', 'Blue', 'Airport - Puratchi Thalaivar Dr.M.G.Ramachandran Central Metro', 12.981593, 80.1639009, 13.0817804, 80.273549, 'knfnAy`hhNkd@ciAokA{nBmLkZw}A{k@wnB__@kaByE_dAaAee@`q@al@jv@g~@kk@b@o^bA_oA@q@??b@???D_C??@m@??@g@??@cA??@Y???g@??@_@??@a@??@]???u@??C]?C??JE??N?f@F??jAJ`q@caAuD{[oL{nAAo@???Y??CcA??@K??Cu@???a@??BaA??@K???Y??@[???S???eA{Pe_C', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('2701ca53-3f59-4d73-aaa3-1c36848c4d1c', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '2206', 'Blue', 'Puratchi Thalaivar Dr.M.G.Ramachandran Central Metro - Airport', 13.0797145, 80.2246325, 12.981593, 80.1639009, 'yrynAezshNwKrSkSpnAt@ha@lz@dp@pk@qv@L[NSNSNONK??v@]??r@M??~IMzyA_j@jaBxEboBz]~rAbX`@L??FDBBDH@B?D??@BA@?BIP??Yr@??`Df@??ZJ^N\NJF??FD??PLLLLN^p@??J^H^BV@`@AR?`@~vAvoCjd@biA', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('c751dc2f-a499-48ff-aa00-620aa2560937', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '141142', 'Blue', 'Wimco Nagar Metro - Airport', 13.1789252, 80.3071406, 12.981593, 80.1639009, 'e`moAg_diNDW??j@H??`@H??lM`C??z@N??jB\txCfi@lDjA??TH??LD??l@N??xAd@??pCz@??z@V??p@R??~@X??h@RhrAha@J@R?V???Fc@??HBJBRF??hD~@??t@T??~@R??dAX??XJ??`@N??nCr@f}Adl@@???c@tA??hA`@\Jd@P??j@L??p@P??b@L??lAZdAXrmBwLpZrCv|@lcAfgAx@BB??xBvCTV??l@`@??f@\??JD??XL??~@d@??RT??Z^??p@pA??RZ??DF??RP??pAjAbp@ncAhsAtdAJHRP??v@H??ZD??xAP??t@H??zC^??~AP??~@J??TB??J@??rAP??dBPxbAzl@tj@fhA~@nA~@lA??LNLNZX~ArA??~CjCtChB~gAtlAnc@nhA`@L??FDBBDH@B?D??@BA@?BIP??Yr@??`Df@??ZJ^N\NJF??FD??PLLLLN^p@??J^H^BV@`@AR?`@~vAvoCjd@biA', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('5e91a508-25ce-4d28-8782-daebf8320546', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '1114', 'Blue', 'Wimco Nagar Metro - Airport', 13.003722, 80.2015436, 12.981593, 80.1639009, 'axjnAakohN`@L??FDBBDH@B?D??@BA@?BIP??Yr@??`Df@??ZJ^N\NJF??FD??PLLLLN^p@??J^H^BV@`@AR?`@~vAvoCjd@biA', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('7f026d20-ed58-4653-9b15-7a176e6ee3f1', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '1211', 'Blue', 'Puratchi Thalaivar Dr.M.G.Ramachandran Central Metro - Airport', 13.0682418, 80.2041187, 12.981593, 80.1639009, 'gjwnAazohNL[NSNSNONK??v@]??r@M??~IMzyA_j@jaBxEboBz]~rAbX`@L??FDBBDH@B?D??@BA@?BIP??Yr@??`Df@??ZJ^N\NJF??FD??PLLLLN^p@??J^H^BV@`@AR?`@~vAvoCjd@biA', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('89810da4-af8f-42d0-8017-9f863e56bbc1', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '2201', 'Blue', 'Puratchi Thalaivar Dr.M.G.Ramachandran Central Metro - Airport', 13.0817804, 80.273549, 12.981593, 80.1639009, 's_znAum}hNdP~mADh_ANA??AaA???Y??CcA??@K??RA??@f@??@fA??@tA??@fB??FjALvAD\??Hn@|Pn_BkE|Nqq@`cAc@jgAt@ha@lz@dp@pk@qv@L[NSNSNONK??v@]??r@M??~IMzyA_j@jaBxEboBz]~rAbX`@L??FDBBDH@B?D??@BA@?BIP??Yr@??`Df@??ZJ^N\NJF??FD??PLLLLN^p@??J^H^BV@`@AR?`@~vAvoCjd@biA', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('0ac6a3f1-2ada-4ad9-82b1-11042e392725', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '17201', 'Green', 'Puratchi Thalaivar Dr.M.G.Ramachandran Central Metro - St.Thomas Mount', 13.0817804, 80.273549, 12.9947486, 80.1975543, 's_znAum}hNdP~mADh_ANA??AaA???Y??CcA??@K??RA??@f@??@fA??@tA??@fB??FjALvAD\??Hn@|Pn_BkE|Nqq@`cAc@jgAt@ha@lz@dp@pk@qv@L[NSNSNONK??v@]??r@M??~IMzyA_j@jaBxEboBz]~rAbX`@L??FDBBDH@B?D??@BA@?BIP??Yr@??`Df@??ZJ^N\NJF??bAC??^B??Z@??VC??lFD', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('644c4d7d-98a4-4880-a9a8-9d7f17245fbd', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '109117', 'Blue', 'Airport - Wimco Nagar Metro', 12.981593, 80.1639009, 13.1789252, 80.3071406, 'knfnAy`hhNkd@ciAokA{nBmLkZmn@e}As^mbAe@i@??u@cA??OCECc@]??a@_@??ACIIIIIGIGKGKE??OK??QK??Bc@??@IAKAIEM??OW??EE??OO_Ay@]]SWk@m@iyAkuA}A_E??[y@??EI??Ym@??Yi@??Sa@??MU??Wg@??QWQ[??QS??[e@ioAa]erAmeAU[??k@w@??IKY[??IM??_@g@??W]??s@{@??GG??OQ??u@cA??[a@gw@sjAohAUye@gfA{AS??SE??c@S??k@Ew@I??iAK??[C??_AKUA??s@G??]C??o@G??uBUi~Al^kw@eVA@??c@tA??w@lB??kAu@??YSQK??SO??cB_A??wBoA??OI??kAo@]Uc`B{n@c@M??m@S??eDcA??yDeAo{Awd@{dCwc@GZ??}AY??kB]??]G??eB]??]G??aAO??IAm@M??}AY??IA??aEu@', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('3fa16e5c-ff64-4442-9c53-c77e5a78d7d5', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '109114', 'Blue', 'Airport - Wimco Nagar Metro', 13.003722, 80.2015436, 13.1789252, 80.3071406, 'axjnAakohNad@ihAs^mbAe@i@??u@cA??OCECc@]??a@_@??ACIIIIIGIGKGKE??OK??QK??Bc@??@IAKAIEM??OW??EE??OO_Ay@]]SWk@m@iyAkuA}A_E??[y@??EI??Ym@??Yi@??Sa@??MU??Wg@??QWQ[??QS??[e@??EG??QW??OS??Y_@gmAmZerAmeAU[??k@w@??IKY[??IM??_@g@??W]??s@{@??GG??OQ??u@cAcx@ukAohAUye@gfA{AS??SE??c@S??k@Ew@I??iAK??[C??_AKUA??FaB??@}@??o@E??mAKmbBtb@kw@eVA@??c@tA??w@lB??kAu@??YSQK??SO??cB_A??wBoA??OI??kAo@]Uc`B{n@c@M??m@S??eDcA??yDeAo{Awd@{dCwc@GZ??}AY??kB]??]G??eB]??]G??aAO??IAm@M??}AY??IA??aEu@', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('12813c1c-f2cd-43c6-818b-ad61346fbc5b', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '102101', 'Blue', 'Airport - Wimco Nagar Metro', 13.1065733, 80.2807641, 13.1789252, 80.3071406, 'c{~nAuy~hNkw@eVA@??c@tA??w@lB??kAu@??YSQK??SO??cB_A??wBoA??OI??kAo@]Uc`B{n@c@M??m@S??eDcA??yDeAo{Awd@{dCwc@GZ??}AY??kB]??]G??eB]??]G??aAO??IAm@M??}AY', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('8eb46df8-8d6b-484f-b222-b7f071315257', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '23203', 'Green', 'St.Thomas Mount - Puratchi Thalaivar Dr.M.G.Ramachandran Central Metro', 13.0789291, 80.2508746, 13.0817804, 80.273549, 'wmynA__yhNAo@???Y??CcA??@K??Cu@???a@??BaA??@K???Y??@[???S???eA{Pe_C', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());
INSERT INTO atlas_app.route (id, integrated_bpp_config_id, code, short_name, long_name, start_lat, start_lon, end_lat, end_lon, polyline, time_bounds, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
        VALUES ('42c9c27d-1818-4b11-b558-e212581728cd', 'abce23a5-3ce6-4c37-8b9b-41377c3c1a52', '24217', 'Green', 'St.Thomas Mount - Puratchi Thalaivar Dr.M.G.Ramachandran Central Metro', 12.9947486, 80.1975543, 13.0817804, 80.273549, 'wainAaqnhNK_@Qu@??SA??M?e@G??_@A??UA??_@G??[G??a@A??[A??Aq@?e@??K???W@??[B??yAFc|B}j@wnB__@kaByE_dAaAee@`q@al@jv@g~@kk@b@o^bA_oA@q@??b@???D_C??@m@??@g@??@cA??@Y???g@??@_@??@a@??@]???u@??C]?C??JE??N?f@F??jAJ`q@caAuD{[oL{nAAo@???Y??CcA??@K??Cu@???a@??BaA??@K???Y??@[???S???eA{Pe_C', 'Unbounded', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', now(), now());

-- ONLY FOR LOCAL TESTING --
delete from atlas_app.station WHERE vehicle_type = 'METRO';
insert into atlas_app.station (integrated_bpp_config_id,address,code,id,lat,lon,name,vehicle_type,merchant_id,merchant_operating_city_id,created_at,updated_at,time_bounds,possible_types)
values
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SGM','719fca8c-dab8-4a2c-91ed-9cb034a2e2ab',13.044682,80.248052,'AG-DMS','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SAP','f723fd25-aeed-4bc8-af80-69ef9c39871e',12.980826,80.1642,'Chennai International Airport','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SGE','f1af3af8-acd5-4cff-ba67-443c39aa043a',13.069557,80.272842,'Government Estate','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SGU','b3455347-fdc0-4e45-9e7b-69bf675fbc8c',13.00924,80.213199,'Guindy','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SHC','30896c2b-b138-4622-bcf4-950047593cd2',13.087369,80.285021,'High Court','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SKP','f8fd5b6c-f687-4cf4-978e-92b3291b84a1',13.151,80.299,'Kaladipet Metro','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SLI','73c28eaa-1854-4ab0-ab81-7a5097772bfa',13.064511,80.266065,'LIC','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SLM','b2194993-cf37-4edd-b3c8-d0d51b8a9694',13.014712,80.223993,'Little Mount','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SMA','84fb33fd-b73d-4f7f-975a-7fbb6496d956',13.095177,80.286164,'Mannadi','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SME','aea13170-cc8c-4958-aba9-f465c9df8352',12.987656,80.176505,'Meenambakkam','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SCR','2e8fc614-6f94-41c3-baeb-99b7c0d27bb1',13.03139,80.239969,'Nandanam','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SOT','758be4e2-6324-41a2-91ac-82df5bcea03a',12.999933,80.193985,'Nanganallur Road','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SNW','af82359e-38d1-4117-9798-dba4901c81d9',13.107064,80.280528,'New Washermenpet Metro','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SSA','03a5b176-cbeb-43ab-ba40-b5db6dc98ae1',13.023717,80.228208,'Saidapet','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'STE','38de1336-7355-473f-ad70-a7ebb02f3b7d',13.037904,80.247029,'Teynampet','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'STC','911e5fb8-d60e-4d6c-bc27-756212f5da51',13.116,80.284,'Thiagaraya Organization Metro','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'STV','b39464d4-73f8-404a-9528-65979c4a2b54',13.172,80.305,'Thiruvotriyur Metro','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'STT','a4d0fe8c-1b2b-48af-be78-cce4a2178098',13.159773,80.302449,'Thiruvotriyur Theradi Metro','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'STL','7160f60f-28a4-4e7c-8809-ac4480aec8a1',13.058198,80.258056,'Thousand Lights','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'STG','f515efb3-6122-478d-bf32-41d00135a03b',13.143,80.296,'Tollgate Metro','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'STR','2ba1e24b-6b51-4750-9186-fff179fae542',13.124,80.289,'Tondiarpet Metro','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SWA','8c9232aa-2733-449f-ac7f-c5826b3a8342',13.107064,80.280528,'Washermanpet','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SWD','8c75dad6-ab10-4e4e-a32b-518544331f25',13.184299,80.309093,'Wimco Nagar Depot','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SWN','90ec1de8-f088-4bc3-aca7-bab7c282100f',13.18304,80.309036,'Wimco Nagar Metro','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SAE','d0675bc1-6d9d-4cec-b811-d3bba72718e4',13.084794,80.21866,'Anna Nagar East','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SAT','effd95d1-805c-4fdc-bcf4-5cbd57f7202f',13.084975,80.208727,'Anna Nagar Tower','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SAL','b764c82b-c3bb-4a04-a444-343f05d9a35d',13.004713,80.20145,'Arignar Anna Alandur ','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SAR','c9a739b8-dc27-4d31-8d33-8592285ecb02',13.062058,80.211581,'Arumbakkam','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SAN','7b23d184-ee76-4eb6-830d-5e74e094db01',13.035534,80.21114,'Ashok Nagar','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SEG','4dabdae1-d625-4d92-b073-ed9ef3e80d45',13.079059,80.261098,'Egmore','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SSI','571a3f6b-c1c3-468b-87f7-ff2b0c064c2d',13.017044,80.20594,'Ekkattuthangal','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SKM','282c79e1-a405-472c-a650-3aa4c7b2a573',13.077508,80.242867,'Kilpauk','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SKO','06c8f6aa-b9ae-4006-aec0-9e54051f32a6',13.073708,80.194869,'Koyambedu','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SNP','887fc0c8-36bc-4074-9347-10599e7eaebb',13.078625,80.250855,'Nehru Park','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SPC','cb598781-a44c-4342-9660-1c932a7680d8',13.07557,80.232347,'Pachaiyappas Organization','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SCC','073bda1a-cc69-41c4-9e98-87b02bb71135',13.081426,80.272887,'Puratchi Thalaivar Dr. M.G. Ramachandran Central','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SCM','95afb8f4-4d81-4e0d-a2d5-8b12b2b92883',13.068568,80.203882,'Puratchi Thalaivi Dr. J. Jayalalithaa CMBT','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SSN','d3e89a9a-6bf0-46df-93e3-3700d4698c3e',13.078697,80.225133,'Shenoy Nagar','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SMM','93f84dcd-1e93-4b91-a4d6-d433a8c85e80',12.995128,80.19864,'St. Thomas Mount','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'STI','9b8a19fa-0a31-4f57-a4ad-113d828afb04',13.085259,80.201575,'Thirumangalam','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a52',null,'SVA','93afef11-2fc6-461e-aa29-824f11beaab6',13.050825,80.212242,'Vadapalani','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null);

-- APPLICATION
INSERT INTO
  atlas_app.integrated_bpp_config(
    id,
    domain,
    feed_key,
    agency_key,
    merchant_id,
    merchant_operating_city_id,
    vehicle_category,
    platform_type,
    config_json,
    created_at,
    updated_at
  )
VALUES
  (
    'abce23a5-3ce6-4c37-8b9b-41377c3c1a53',
    'FRFS',
    'chennai_metro',
    'chennai_metro:CMRL',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    'METRO',
    'APPLICATION',
    '{"tag": "ONDC", "contents": {"networkHostUrl": "www.google.com"}}',
    now(),
    now()
  );

-- ONLY FOR LOCAL TESTING --
delete from atlas_app.station WHERE vehicle_type = 'METRO';
insert into atlas_app.station (integrated_bpp_config_id,address,code,id,lat,lon,name,vehicle_type,merchant_id,merchant_operating_city_id,created_at,updated_at,time_bounds,possible_types)
values
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SGM','719fca8c-dab8-4a2c-91ed-9cb034a2e2ab',13.044682,80.248052,'AG-DMS','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SAP','f723fd25-aeed-4bc8-af80-69ef9c39871e',12.980826,80.1642,'Chennai International Airport','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SGE','f1af3af8-acd5-4cff-ba67-443c39aa043a',13.069557,80.272842,'Government Estate','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SGU','b3455347-fdc0-4e45-9e7b-69bf675fbc8c',13.00924,80.213199,'Guindy','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SHC','30896c2b-b138-4622-bcf4-950047593cd2',13.087369,80.285021,'High Court','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SKP','f8fd5b6c-f687-4cf4-978e-92b3291b84a1',13.151,80.299,'Kaladipet Metro','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SLI','73c28eaa-1854-4ab0-ab81-7a5097772bfa',13.064511,80.266065,'LIC','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SLM','b2194993-cf37-4edd-b3c8-d0d51b8a9694',13.014712,80.223993,'Little Mount','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SMA','84fb33fd-b73d-4f7f-975a-7fbb6496d956',13.095177,80.286164,'Mannadi','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SME','aea13170-cc8c-4958-aba9-f465c9df8352',12.987656,80.176505,'Meenambakkam','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SCR','2e8fc614-6f94-41c3-baeb-99b7c0d27bb1',13.03139,80.239969,'Nandanam','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SOT','758be4e2-6324-41a2-91ac-82df5bcea03a',12.999933,80.193985,'Nanganallur Road','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SNW','af82359e-38d1-4117-9798-dba4901c81d9',13.107064,80.280528,'New Washermenpet Metro','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SSA','03a5b176-cbeb-43ab-ba40-b5db6dc98ae1',13.023717,80.228208,'Saidapet','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'STE','38de1336-7355-473f-ad70-a7ebb02f3b7d',13.037904,80.247029,'Teynampet','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'STC','911e5fb8-d60e-4d6c-bc27-756212f5da51',13.116,80.284,'Thiagaraya Organization Metro','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'STV','b39464d4-73f8-404a-9528-65979c4a2b54',13.172,80.305,'Thiruvotriyur Metro','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'STT','a4d0fe8c-1b2b-48af-be78-cce4a2178098',13.159773,80.302449,'Thiruvotriyur Theradi Metro','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'STL','7160f60f-28a4-4e7c-8809-ac4480aec8a1',13.058198,80.258056,'Thousand Lights','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'STG','f515efb3-6122-478d-bf32-41d00135a03b',13.143,80.296,'Tollgate Metro','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'STR','2ba1e24b-6b51-4750-9186-fff179fae542',13.124,80.289,'Tondiarpet Metro','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SWA','8c9232aa-2733-449f-ac7f-c5826b3a8342',13.107064,80.280528,'Washermanpet','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SWD','8c75dad6-ab10-4e4e-a32b-518544331f25',13.184299,80.309093,'Wimco Nagar Depot','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SWN','90ec1de8-f088-4bc3-aca7-bab7c282100f',13.18304,80.309036,'Wimco Nagar Metro','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SAE','d0675bc1-6d9d-4cec-b811-d3bba72718e4',13.084794,80.21866,'Anna Nagar East','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SAT','effd95d1-805c-4fdc-bcf4-5cbd57f7202f',13.084975,80.208727,'Anna Nagar Tower','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SAL','b764c82b-c3bb-4a04-a444-343f05d9a35d',13.004713,80.20145,'Arignar Anna Alandur ','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SAR','c9a739b8-dc27-4d31-8d33-8592285ecb02',13.062058,80.211581,'Arumbakkam','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SAN','7b23d184-ee76-4eb6-830d-5e74e094db01',13.035534,80.21114,'Ashok Nagar','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SEG','4dabdae1-d625-4d92-b073-ed9ef3e80d45',13.079059,80.261098,'Egmore','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SSI','571a3f6b-c1c3-468b-87f7-ff2b0c064c2d',13.017044,80.20594,'Ekkattuthangal','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SKM','282c79e1-a405-472c-a650-3aa4c7b2a573',13.077508,80.242867,'Kilpauk','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SKO','06c8f6aa-b9ae-4006-aec0-9e54051f32a6',13.073708,80.194869,'Koyambedu','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SNP','887fc0c8-36bc-4074-9347-10599e7eaebb',13.078625,80.250855,'Nehru Park','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SPC','cb598781-a44c-4342-9660-1c932a7680d8',13.07557,80.232347,'Pachaiyappas Organization','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SCC','073bda1a-cc69-41c4-9e98-87b02bb71135',13.081426,80.272887,'Puratchi Thalaivar Dr. M.G. Ramachandran Central','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SCM','95afb8f4-4d81-4e0d-a2d5-8b12b2b92883',13.068568,80.203882,'Puratchi Thalaivi Dr. J. Jayalalithaa CMBT','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SSN','d3e89a9a-6bf0-46df-93e3-3700d4698c3e',13.078697,80.225133,'Shenoy Nagar','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SMM','93f84dcd-1e93-4b91-a4d6-d433a8c85e80',12.995128,80.19864,'St. Thomas Mount','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'STI','9b8a19fa-0a31-4f57-a4ad-113d828afb04',13.085259,80.201575,'Thirumangalam','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a53',null,'SVA','93afef11-2fc6-461e-aa29-824f11beaab6',13.050825,80.212242,'Vadapalani','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null);

-- PARTNERORG
INSERT INTO
  atlas_app.integrated_bpp_config(
    id,
    domain,
    feed_key,
    agency_key,
    merchant_id,
    merchant_operating_city_id,
    vehicle_category,
    platform_type,
    config_json,
    created_at,
    updated_at
  )
VALUES
  (
    'abce23a5-3ce6-4c37-8b9b-41377c3c1a54',
    'FRFS',
    'chennai_metro',
    'chennai_metro:CMRL',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    'METRO',
    'PARTNERORG',
    '{"tag": "ONDC", "contents": {"networkHostUrl": "www.google.com"}}',
    now(),
    now()
  );

-- ONLY FOR LOCAL TESTING --
delete from atlas_app.station WHERE vehicle_type = 'METRO';
insert into atlas_app.station (integrated_bpp_config_id,address,code,id,lat,lon,name,vehicle_type,merchant_id,merchant_operating_city_id,created_at,updated_at,time_bounds,possible_types)
values
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SGM','719fca8c-dab8-4a2c-91ed-9cb034a2e2ab',13.044682,80.248052,'AG-DMS','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SAP','f723fd25-aeed-4bc8-af80-69ef9c39871e',12.980826,80.1642,'Chennai International Airport','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SGE','f1af3af8-acd5-4cff-ba67-443c39aa043a',13.069557,80.272842,'Government Estate','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SGU','b3455347-fdc0-4e45-9e7b-69bf675fbc8c',13.00924,80.213199,'Guindy','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SHC','30896c2b-b138-4622-bcf4-950047593cd2',13.087369,80.285021,'High Court','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SKP','f8fd5b6c-f687-4cf4-978e-92b3291b84a1',13.151,80.299,'Kaladipet Metro','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SLI','73c28eaa-1854-4ab0-ab81-7a5097772bfa',13.064511,80.266065,'LIC','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SLM','b2194993-cf37-4edd-b3c8-d0d51b8a9694',13.014712,80.223993,'Little Mount','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SMA','84fb33fd-b73d-4f7f-975a-7fbb6496d956',13.095177,80.286164,'Mannadi','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SME','aea13170-cc8c-4958-aba9-f465c9df8352',12.987656,80.176505,'Meenambakkam','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SCR','2e8fc614-6f94-41c3-baeb-99b7c0d27bb1',13.03139,80.239969,'Nandanam','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SOT','758be4e2-6324-41a2-91ac-82df5bcea03a',12.999933,80.193985,'Nanganallur Road','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SNW','af82359e-38d1-4117-9798-dba4901c81d9',13.107064,80.280528,'New Washermenpet Metro','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SSA','03a5b176-cbeb-43ab-ba40-b5db6dc98ae1',13.023717,80.228208,'Saidapet','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'STE','38de1336-7355-473f-ad70-a7ebb02f3b7d',13.037904,80.247029,'Teynampet','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'STC','911e5fb8-d60e-4d6c-bc27-756212f5da51',13.116,80.284,'Thiagaraya Organization Metro','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'STV','b39464d4-73f8-404a-9528-65979c4a2b54',13.172,80.305,'Thiruvotriyur Metro','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'STT','a4d0fe8c-1b2b-48af-be78-cce4a2178098',13.159773,80.302449,'Thiruvotriyur Theradi Metro','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'STL','7160f60f-28a4-4e7c-8809-ac4480aec8a1',13.058198,80.258056,'Thousand Lights','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'STG','f515efb3-6122-478d-bf32-41d00135a03b',13.143,80.296,'Tollgate Metro','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'STR','2ba1e24b-6b51-4750-9186-fff179fae542',13.124,80.289,'Tondiarpet Metro','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SWA','8c9232aa-2733-449f-ac7f-c5826b3a8342',13.107064,80.280528,'Washermanpet','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SWD','8c75dad6-ab10-4e4e-a32b-518544331f25',13.184299,80.309093,'Wimco Nagar Depot','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SWN','90ec1de8-f088-4bc3-aca7-bab7c282100f',13.18304,80.309036,'Wimco Nagar Metro','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SAE','d0675bc1-6d9d-4cec-b811-d3bba72718e4',13.084794,80.21866,'Anna Nagar East','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SAT','effd95d1-805c-4fdc-bcf4-5cbd57f7202f',13.084975,80.208727,'Anna Nagar Tower','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SAL','b764c82b-c3bb-4a04-a444-343f05d9a35d',13.004713,80.20145,'Arignar Anna Alandur ','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SAR','c9a739b8-dc27-4d31-8d33-8592285ecb02',13.062058,80.211581,'Arumbakkam','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SAN','7b23d184-ee76-4eb6-830d-5e74e094db01',13.035534,80.21114,'Ashok Nagar','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SEG','4dabdae1-d625-4d92-b073-ed9ef3e80d45',13.079059,80.261098,'Egmore','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SSI','571a3f6b-c1c3-468b-87f7-ff2b0c064c2d',13.017044,80.20594,'Ekkattuthangal','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SKM','282c79e1-a405-472c-a650-3aa4c7b2a573',13.077508,80.242867,'Kilpauk','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SKO','06c8f6aa-b9ae-4006-aec0-9e54051f32a6',13.073708,80.194869,'Koyambedu','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SNP','887fc0c8-36bc-4074-9347-10599e7eaebb',13.078625,80.250855,'Nehru Park','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SPC','cb598781-a44c-4342-9660-1c932a7680d8',13.07557,80.232347,'Pachaiyappas Organization','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SCC','073bda1a-cc69-41c4-9e98-87b02bb71135',13.081426,80.272887,'Puratchi Thalaivar Dr. M.G. Ramachandran Central','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SCM','95afb8f4-4d81-4e0d-a2d5-8b12b2b92883',13.068568,80.203882,'Puratchi Thalaivi Dr. J. Jayalalithaa CMBT','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SSN','d3e89a9a-6bf0-46df-93e3-3700d4698c3e',13.078697,80.225133,'Shenoy Nagar','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SMM','93f84dcd-1e93-4b91-a4d6-d433a8c85e80',12.995128,80.19864,'St. Thomas Mount','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'STI','9b8a19fa-0a31-4f57-a4ad-113d828afb04',13.085259,80.201575,'Thirumangalam','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null),
('abce23a5-3ce6-4c37-8b9b-41377c3c1a54',null,'SVA','93afef11-2fc6-461e-aa29-824f11beaab6',13.050825,80.212242,'Vadapalani','METRO','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52','namma-yatri-0-0000-0000-00000000city',now(),now(),'Unbounded',null);