-- Align internal_api_key with the local-dev provider-dashboard token.
-- Runs automatically after ANY Config Sync (master or prod) so AUTH_BLOCKED
-- errors never occur locally, regardless of which value the sync brings in.
-- MSIL_PARTNER is excluded as it uses a dedicated key outside the standard dashboard flow.

UPDATE atlas_driver_offer_bpp.merchant
SET internal_api_key = 'some-secret-dashboard-token-for-driver-offer-bpp'
WHERE short_id != 'MSIL_PARTNER';

UPDATE atlas_app.merchant
SET driver_offer_api_key = 'some-secret-dashboard-token-for-driver-offer-bpp';
