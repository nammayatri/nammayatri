DO $$
DECLARE
    merchant_row RECORD;
BEGIN

    FOR merchant_row IN SELECT merchant_id, id FROM atlas_app.merchant_operating_city LOOP

        INSERT INTO atlas_app.merchant_service_config (merchant_id, service_name, config_JSON, merchant_operating_city_id, created_at, updated_at)
        VALUES (merchant_row.merchant_id, 'Tokenization_JourneyMonitoring', '{ "url":"http://13.201.220.83:8080","username":"INSERT_USER_NAME","password":"INSERT_PASSWORD" }', merchant_row.id, NOW(), NOW());
    END LOOP;
END $$;

DO $$
DECLARE
    merchant_row RECORD;
BEGIN

    FOR merchant_row IN SELECT merchant_id, id FROM atlas_app.merchant_operating_city LOOP

        INSERT INTO atlas_app.merchant_service_config (merchant_id, service_name, config_JSON, merchant_operating_city_id, created_at, updated_at)
        VALUES (merchant_row.merchant_id, 'IncidentReport_ERSS', '{ "url":"http://13.201.220.83:8080", "userName": "INSERT_USER_NAME" }', merchant_row.id, NOW(), NOW());
    END LOOP;
END $$;
