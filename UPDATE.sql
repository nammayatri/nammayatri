UPDATE
  atlas_app.merchant_operating_city
SET
  city = 'Delhi',
  state = 'NationalCapitalTerritory'
WHERE
  city = 'Kochi';

UPDATE
  atlas_app.integrated_bpp_config
SET
  vehicle_category = 'BUS',
  config_json = '{ "tag": "ONDC", "contents": {} }'
WHERE
  platform_type = 'MULTIMODAL';

UPDATE
  atlas_app.geometry
SET
  geom = '0106000000010000000103000000010000004F000000C087A3F36984534096305DAF63662B40B413CAAEF38953406A66C8E057552B40B2689CBCDB8C5340F9646AE7DE442B40DFAAD0D6F48B5340FC5524FA28332B40E51FE8BE858C5340F8A02EABF5202B40FED0C5CE628E5340FC0356CE2F342B4029D75ADACD905340595A71368C2C2B4071F8A3A84D965340166EA8DEC42F2B40E323A046EA9953405B1D77D7232D2B408A6CF70C609853401020B7BD43132B40BF34D1211298534045A0AF23DBF12A409897B1F7BD975340C07BB0D9D8C42A40DAB8464C969853407822DC3D9FAB2A40C008108A049C5340F6B90AC43BA42A40BFE8A342D49E53406D89D324ADA12A400C506DF8CBA15340DDE3FDC7A6882A40920F542B22A25340246A5D8518922A4076D1125258A353406125178B5E942A40F131B9994BA453405D70CD131DA12A40D52A43AABDA55340A62484F4DF912A40E9DFA07FCEA4534050E752C3476F2A408A1E924C63A55340A27D1B2273582A407DB6B40A57A1534094024BAF28442A4014386F6197A25340830C715D25382A405F8460C7DFA053407F9EF238B22F2A40A45329A8D6A0534043A1509114222A40DDDBB0D94B9E53406634309DBA1C2A40C1B6B49E669D53405BC2E5C4D90E2A407ECD991B739E534099A7CF3B33F72940B452258CC29C5340A516BD19FEF7294033B364C2D39A53402A24E8C41AE72940F05A3BD5309E5340769E2454C6C22940307D0DE9C7965340553F4A5F77DE2940457A10936C94534070276A9779B92940658168752B905340850CF82F4CB82940FBFE754A0F8F534007F35D81A9882940BBEA4236DF8953400D7D4D45578D2940AC4250C60A875340F8405061C69D2940980BE02EA3855340277CF98724AD29405CCF96B4118253405F9E21EABDB829406D09C151F27F5340F88E01ACB7A12940D3D17A56EA7C5340832A7D9555B929402DCACB4D887C5340E6FC5151C8A82940A7E7E8F4BA7A534050AFE299B7AC294046AA19C6817C53406088C76ABAC7294044A69E815279534020480E8829C42940D952222785745340979E2FE8EEB929405695A8D56173534002F2BA24E1A62940F70B176F6A725340533B867940802940E955BCC8D870534005964B53496C294004C5EEF1396F5340FDCA1BC3A25929408DF2ED3FED6B5340901C1752325029402DF123240C6B534022FBFC3F555C2940EDB734830F6653400CE7AFB821542940317DB2FB6966534093288629C63F29407C8C50FA2C655340E96AEACF3322294048DCCD4C4B675340758E60F0B30A294038A34CB779655340361E44DEB80029402ED4C5140966534087A7057A48F428401D79F78A6F685340E905455101FE28405B6BDAC2F46753401B6C49A53DC528404E36586262655340375852F5EAAA2840029AB56EDA6153405170C29E9491284004A6BC59A25E534031D746AD3E8828404B0817B5815E5340EEBEFF0925682840AE11CDF50D6E5340C22E591F575E2840C11E4119DF6F5340DBE7A70B0852284098CEF73F727053402E1BB41E092F2840D3EB2AEF546B534099E42CF371E727408D831C121360534084C1251E29E82740FD8B76501B5F5340E3B8FFC446C72740025AC38E905D53408273613571D12740CD8E835C045E5340E96C3F36AFB32740F9CA6074315B53404390E3F46D8527407ADD16A63658534028CF073C2C96274056A882691D555340B54E3200128F2740D7368B317F5253407BFC26BECF9F27404F663FA4752D5340E399D0F58F342A40C087A3F36984534096305DAF63662B40',
  city = 'Delhi',
  state = 'NationalCapitalTerritory';

UPDATE
  atlas_app.rider_config
SET
  multimodal_testing = true,
  metro_booking_allowed = true;

update
  atlas_app.merchant
set
  city = 'Delhi';

-- External Merchant Service Configs
DELETE FROM
  atlas_app.merchant_service_config
WHERE
  service_name = 'MultiModalStaticData_OTPTransit';

INSERT INTO
  atlas_app.merchant_service_config (
    merchant_id,
    service_name,
    config_json,
    merchant_operating_city_id
  )
SELECT
  m.merchant_id,
  'MultiModalStaticData_OTPTransit',
  '{"baseUrl": "https://api.sandbox.moving.tech/gtfs-inmemory", "queryType": "MULTI_SEARCH", "numItineraries": 20, "weightedSortCfg": {"duration": 0.47, "transfers": 0.05, "arrivalTime": 0.48}}',
  m.id
FROM
  atlas_app.merchant_operating_city m;

DELETE FROM
  atlas_app.merchant_service_config
WHERE
  service_name = 'MultiModal_OTPTransit';

INSERT INTO
  atlas_app.merchant_service_config (
    merchant_id,
    service_name,
    config_json,
    merchant_operating_city_id
  )
SELECT
  m.merchant_id,
  'MultiModal_OTPTransit',
  '{"baseUrl": "https://api.sandbox.moving.tech/nandi/otp/gtfs/v1", "queryType": "MULTI_SEARCH", "numItineraries": 20, "weightedSortCfg": {"duration": 0.47, "transfers": 0.05, "arrivalTime": 0.48}}',
  m.id
FROM
  atlas_app.merchant_operating_city m;

DELETE FROM
  atlas_app.merchant_service_config
WHERE
  service_name = 'BusPayment_Juspay';

INSERT INTO
  atlas_app.merchant_service_config (
    merchant_id,
    service_name,
    config_json,
    merchant_operating_city_id
  )
SELECT
  m.merchant_id,
  'BusPayment_Juspay',
  '{ "apiKey": "0.1.0|2|gBsRFgxKWcGpZYLP83E8Odv/5dfkM0KFd20UDvQTq26sjFlyAubhQ1p7s5qJHVFCpPPCkcwEl67HFG6kSxEuWwwaUEg3L+vdiuh1wJszJWr7","returnUrl": "https://app.integ.moving.tech/wv/ticketBookingStatus","url": "https://api.juspay.in/","merchantId": "nammayatri","username": "nammayatri","password": "0.1.0|1|Huy/LADFDdnBFPXqXEPgi/85N6JGa0HuUSZMZYf8FyNnGGKWMMDVncNxySjFvUg39WnpEidXvf0mRZkmqA==" }',
  m.id
FROM
  atlas_app.merchant_operating_city m;

DELETE FROM
  atlas_app.merchant_service_config
WHERE
  service_name = 'Maps_Google';

WITH MerchantMapsServiceConfigs AS (
  SELECT
    T1.merchant_id,
    T1.id,
    'Maps_Google',
    CAST (
      '{
  "googleKey": "0.1.0|2|S34+Lq69uC/hNeYSXr4YSjwwmaTS0jO/1ZGlAAwl72hBhgD9AAZhgI4o/6x3oi99KyJkQdt5UvgjlHyeEOuf1Z3xzOBqWBYVQM/RBggZ7NggTyIsDgiG5b3p",
  "useNewPlaces": true,
  "googleMapsUrl": "https://maps.googleapis.com/maps/api/",
  "googleRoadsUrl": "https://roads.googleapis.com/",
  "googlePlaceNewUrl": "https://places.googleapis.com/v1/",
  "googleRouteConfig": { "url": "https://routes.googleapis.com/",
  "routePreference": "TRAFFIC_AWARE_OPTIMAL",
  "computeAlternativeRoutes": false },
  "useAdvancedDirections": false,
  "googleAutocompleteParams": ["in"]
  }' AS json
    )
  FROM
    atlas_app.merchant_operating_city AS T1
) -- ONDC Metro Provider Feed and Config
INSERT INTO
  atlas_app.merchant_service_config (
    merchant_id,
    merchant_operating_city_id,
    service_name,
    config_json
  ) (
    SELECT
      *
    FROM
      MerchantMapsServiceConfigs
  );

WITH config_updates (
  city,
  vehicle_category,
  merchant_short_id,
  feed_key,
  agency_key,
  platform_type
) AS (
  VALUES
    (
      'Delhi',
      'BUS',
      'NAMMA_YATRI',
      'delhi_bus_nammayatri_mock',
      'delhi_bus_nammayatri_mock',
      'MULTIMODAL'
    )
)
UPDATE
  atlas_app.integrated_bpp_config AS ic
SET
  feed_key = cu.feed_key,
  agency_key = cu.agency_key
FROM
  config_updates cu
  JOIN atlas_app.merchant_operating_city moc ON cu.city = moc.city
  AND cu.merchant_short_id = moc.merchant_short_id
WHERE
  ic.merchant_operating_city_id = moc.id
  AND ic.vehicle_category = cu.vehicle_category
  AND ic.platform_type = cu.platform_type;

UPDATE
  atlas_app.beckn_config
SET
  gateway_url = 'https://pre-prod-ondc-ticketing-api-delhi.transportstack.in/',
  registry_url = 'https://preprod.registry.ondc.org',
  subscriber_id = 'api.sandbox.moving.tech/dev/bap/frfs/4b17bd06-ae7e-48e9-85bf-282fb310209c',
  subscriber_url = 'https://ee1f17dfb318.ngrok-free.app/beckn/frfs/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
  unique_key_id = '007',
  settlement_type = 'NEFT'
WHERE
  domain = 'FRFS'
  and merchant_id = 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52';

UPDATE
  atlas_app.merchant
SET
  gateway_and_registry_priority_list = '{ONDC}';

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN bpp_payment_id text;


INSERT INTO atlas_app.issue_category (
    id, category, logo_url, priority, merchant_id, category_type,
    created_at, updated_at, is_ride_required, max_allowed_ride_age,
    is_active, merchant_operating_city_id, label, igm_category, allowed_ride_statuses
) VALUES (
    'c3e78976-0b1d-4c5e-ac89-234e6afc0c6a', 'Metro', 'logoUrl', 15,
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'Category',
    '2024-09-23T12:52:26.59061Z', '2024-09-27T10:43:23.704051Z',
    FALSE, 86400, FALSE,
    'namma-yatri-0-0000-0000-00000000city', 'METRO_RELATED', 'ORDER', NULL
);

INSERT INTO atlas_app.issue_option (
    id, issue_category_id, issue_message_id, option, label, priority,
    merchant_id, restricted_variants, is_active, created_at, updated_at,
    merchant_operating_city_id, show_only_when_user_blocked,
    igm_sub_category, restricted_ride_statuses, mandatory_uploads
) VALUES (
    '9dzrbtwg-zu2p-zmyj-584m-uk4a1vems75a',
    'c3e78976-0b1d-4c5e-ac89-234e6afc0c6a',
    'hkpdpoe5-5lny-3xkh-1oi0-7d7hkwrxabsx',
    'Ticket not generated post payment completion',
    NULL, 1,
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    '{}', TRUE,
    '2024-07-09T14:01:56.528501Z', '2024-07-09T14:01:56.53006Z',
    'namma-yatri-0-0000-0000-00000000city',
    FALSE, 'ORD101', '{}', NULL
);