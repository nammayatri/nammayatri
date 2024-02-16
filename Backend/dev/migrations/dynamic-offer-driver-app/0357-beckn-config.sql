INSERT INTO atlas_driver_offer_bpp.beckn_config (id, domain, registry_url, merchant_id, created_at, updated_at)
    SELECT atlas_driver_offer_bpp.uuid_generate_v4(), 'MOBILITY', T1.registry_url, T1.id, now(), now()
    FROM atlas_driver_offer_bpp.merchant AS T1;
