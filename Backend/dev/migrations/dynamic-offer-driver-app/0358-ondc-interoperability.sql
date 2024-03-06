-- DON'T RUN IN MASTER & PROD
INSERT INTO atlas_driver_offer_bpp.white_list_org (id, subscriber_id, domain) VALUES
(md5(random()::TEXT || clock_timestamp()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT)::UUID,
'beta.beckn.uat.juspay.net/dev/bap/beckn/da4e23a5-3ce6-4c37-8b9b-41377c3c1a51', 'MOBILITY');
