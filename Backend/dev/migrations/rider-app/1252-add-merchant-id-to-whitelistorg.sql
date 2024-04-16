
-- whitelist merchant level
-- NOTE: RUN IT AFTER SUCCESSFULL PROD RELEASE
WITH MerchantIds AS (
    SELECT id as merchant_id FROM atlas_app.merchant
)
INSERT INTO atlas_app.white_list_org (id, subscriber_id, domain, merchant_id)
SELECT
    md5(random()::TEXT || clock_timestamp()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT)::UUID,
    subscriber_id,
    domain,
    MerchantIds.merchant_id as merchant_id
FROM atlas_app.white_list_org CROSS JOIN MerchantIds;
