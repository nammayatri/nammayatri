-- ============================================================================
-- Helsinki cancellation-fee + invoice extras (supersedes hardcoded bits in 0005/0003)
-- ============================================================================
-- Adds the four pieces that were missing for full Helsinki cancellation + invoice
-- support (per Dhruv's integ-env recipe shared 2026-05-17). Uses DYNAMIC city
-- lookup so it works regardless of whether config-sync rotates Helsinki's UUID.
-- Idempotent (ON CONFLICT DO NOTHING / DO UPDATE).
--
-- 1. CUSTOMER_NO_SHOW cancellation_reason row — required for the cancellation
--    handler at CancelRide/Internal.hs:361,509,515 to accept that reasonCode
--    and apply the no-show charge logic.
-- 2. access_matrix grant: FLEET role → FINANCE_INVOICE_PDF — required for fleet
--    owner to download their invoice PDFs from the dashboard.
-- 3. transporter_config.invoice_config (BPP) — adds logoUrl + emitLedgerEntries
--    so driver-side invoices render with the Lynx Taxi Finland logo and ledger
--    entries flow to finance_ledger_entry.
-- 4. rider_config.invoice_config (BAP) — adds supplier name/address/VAT number
--    so customer-side invoices show the correct Finnish business info.
-- ============================================================================

-- ---------------------------------------------------------------------------
-- Resolve Helsinki city UUIDs dynamically — no hardcoded values
-- ---------------------------------------------------------------------------

-- DO NOT RUN ANY OF THESE IN MASTER/PROD
DO $$
DECLARE
    bpp_city_id TEXT;
    bap_city_id TEXT;
BEGIN
    SELECT id INTO bpp_city_id
    FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE city = 'Helsinki' AND merchant_short_id = 'BRIDGE_FINLAND_PARTNER'
    LIMIT 1;

    SELECT id INTO bap_city_id
    FROM atlas_app.merchant_operating_city
    WHERE city = 'Helsinki' AND merchant_short_id = 'BRIDGE_FINLAND'
    LIMIT 1;

    RAISE NOTICE 'Helsinki BPP city: %, BAP city: %', bpp_city_id, bap_city_id;

    IF bpp_city_id IS NULL THEN
        RAISE NOTICE 'Skipping BPP-side updates: Helsinki not in atlas_driver_offer_bpp.merchant_operating_city. Run config-sync first.';
        RETURN;
    END IF;

    ------------------------------------------------------------------------
    -- 1. cancellation_reason: CUSTOMER_NO_SHOW (used by CancelRide handler)
    ------------------------------------------------------------------------
    INSERT INTO atlas_driver_offer_bpp.cancellation_reason
        (reason_code, description, enabled, priority, created_at, updated_at)
    VALUES
        ('CUSTOMER_NO_SHOW', 'Customer did not show up', true, 8, now(), now())
    ON CONFLICT (reason_code) DO UPDATE
      SET enabled = true,
          priority = EXCLUDED.priority,
          description = EXCLUDED.description,
          updated_at = now();

    ------------------------------------------------------------------------
    -- 2. transporter_config.invoice_config (BPP) — logo + ledger entries
    ------------------------------------------------------------------------
    UPDATE atlas_driver_offer_bpp.transporter_config
    SET invoice_config = '{
        "driverInvoiceLineItemsVatInclusive": false,
        "emitLedgerEntries": true,
        "logoUrl": "https://raw.githubusercontent.com/witcher-shailesh/github-asset-store/main/uploads/img-lynx-app-2-1778256229295.svg"
    }'::jsonb,
    -- 0005 may have failed to flip this if its hardcoded UUID didn't match — reaffirm here
    can_add_cancellation_fee = true
    WHERE merchant_operating_city_id = bpp_city_id;

    ------------------------------------------------------------------------
    -- 3. rider_config.invoice_config (BAP) — supplier + logo
    ------------------------------------------------------------------------
    IF bap_city_id IS NOT NULL THEN
        UPDATE atlas_app.rider_config
        SET invoice_config = '{
            "ledgerEmitEnabled": true,
            "logoUrl": "https://raw.githubusercontent.com/witcher-shailesh/github-asset-store/main/uploads/img-lynx-app-2-1778256229295.svg",
            "supplierName": "LynxTaxi Finland",
            "supplierAddress": "123, MB Road, Kochi, Helsinki - 682001, Finland",
            "supplierVatNumber": "FI12345678"
        }'::jsonb
        WHERE merchant_operating_city_id = bap_city_id;
    END IF;
END $$;

-- ---------------------------------------------------------------------------
-- 4. access_matrix grant: FLEET role → DSL / FINANCE_INVOICE_PDF
--    (separate DO block — atlas_bpp_dashboard.role uses different schema/key)
-- ---------------------------------------------------------------------------
INSERT INTO atlas_bpp_dashboard.access_matrix
    (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at)
SELECT
    gen_random_uuid()::text,
    r.id,
    'DSL',
    'USER_FULL_ACCESS',
    'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_INVOICE_PDF',
    now(),
    now()
FROM atlas_bpp_dashboard.role r
WHERE r.dashboard_access_type IN ('FLEET_OWNER', 'FLEET_BUSINESS')
  AND NOT EXISTS (
    SELECT 1 FROM atlas_bpp_dashboard.access_matrix am
    WHERE am.role_id = r.id
      AND am.user_action_type = 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_INVOICE_PDF'
  );

-- ---------------------------------------------------------------------------
-- Summary
-- ---------------------------------------------------------------------------
SELECT 'Helsinki cancellation + invoice extras applied' AS status;

SELECT 'cancellation_reason CUSTOMER_NO_SHOW: ' ||
       CASE WHEN exists(SELECT 1 FROM atlas_driver_offer_bpp.cancellation_reason WHERE reason_code = 'CUSTOMER_NO_SHOW')
            THEN '✓ present' ELSE '✗ missing' END AS reason_check;

SELECT 'BPP invoice_config has logoUrl: ' ||
       CASE WHEN invoice_config::text LIKE '%logoUrl%' THEN '✓' ELSE '✗' END AS bpp_invoice_check
FROM atlas_driver_offer_bpp.transporter_config tc
JOIN atlas_driver_offer_bpp.merchant_operating_city moc ON tc.merchant_operating_city_id = moc.id
WHERE moc.city = 'Helsinki' AND moc.merchant_short_id = 'BRIDGE_FINLAND_PARTNER'
LIMIT 1;

SELECT 'BAP invoice_config has supplierVatNumber: ' ||
       CASE WHEN invoice_config::text LIKE '%supplierVatNumber%' THEN '✓' ELSE '✗' END AS bap_invoice_check
FROM atlas_app.rider_config rc
JOIN atlas_app.merchant_operating_city moc ON rc.merchant_operating_city_id = moc.id
WHERE moc.city = 'Helsinki' AND moc.merchant_short_id = 'BRIDGE_FINLAND'
LIMIT 1;

SELECT 'FLEET role has FINANCE_INVOICE_PDF access: ' ||
       count(*)::text || ' grants' AS access_check
FROM atlas_bpp_dashboard.access_matrix am
JOIN atlas_bpp_dashboard.role r ON r.id = am.role_id
WHERE am.user_action_type = 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_INVOICE_PDF'
  AND r.dashboard_access_type IN ('FLEET_OWNER', 'FLEET_BUSINESS');
