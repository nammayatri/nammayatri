DO $$
DECLARE
    merchant_row RECORD;
BEGIN

    FOR merchant_row IN SELECT merchant_id, id FROM atlas_driver_offer_bpp.merchant_operating_city LOOP

        INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, service_name, config_JSON, merchant_operating_city_id, created_at, updated_at)
        VALUES (merchant_row.merchant_id, 'Verification_HyperVerge', '{ "url":"https://dummyUrl","appId":"dummy","appKey":"0.1.0|8|xgWOO3UTpKVvav8nc93+KVrEneQHUXMlXy1vpcsaoC/RKjHC3YTGcePB00hiZSsgENimo1Gy938="  }', merchant_row.id, NOW(), NOW()); --This password is master passetto encrypted. Please re-encrypt before running in prod.
    END LOOP;
END $$;