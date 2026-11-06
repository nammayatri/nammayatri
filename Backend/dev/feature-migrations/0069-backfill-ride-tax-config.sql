CREATE OR REPLACE FUNCTION pg_temp.vat_tax(p_pct NUMERIC) RETURNS JSONB AS $$
  SELECT CASE WHEN p_pct IS NULL OR p_pct <= 0 THEN NULL ELSE jsonb_build_object('tax', p_pct) END;
$$ LANGUAGE sql IMMUTABLE;

CREATE OR REPLACE FUNCTION pg_temp.gst_split_tax(p_pct NUMERIC, p_gst JSONB) RETURNS JSONB AS $$
DECLARE
  v_cgst NUMERIC := COALESCE((p_gst ->> 'cgstPercentage')::NUMERIC, 0);
  v_sgst NUMERIC := COALESCE((p_gst ->> 'sgstPercentage')::NUMERIC, 0);
  v_igst NUMERIC := COALESCE((p_gst ->> 'igstPercentage')::NUMERIC, 0);
  v_total NUMERIC := v_cgst + v_sgst + v_igst;
BEGIN
  IF p_pct IS NULL OR p_pct <= 0 THEN
    RETURN NULL;
  END IF;
  IF v_total <= 0 THEN
    RETURN jsonb_build_object('tax', p_pct);
  END IF;
  RETURN jsonb_strip_nulls(jsonb_build_object(
    'tax', p_pct,
    'cgst', CASE WHEN v_cgst > 0 THEN round(p_pct * v_cgst / v_total, 6) END,
    'sgst', CASE WHEN v_sgst > 0 THEN round(p_pct * v_sgst / v_total, 6) END,
    'igst', CASE WHEN v_igst > 0 THEN round(p_pct * v_igst / v_total, 6) END));
END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE OR REPLACE FUNCTION pg_temp.charge_pct(p_charge_config TEXT) RETURNS NUMERIC AS $$
  SELECT CASE WHEN (p_charge_config::jsonb ->> 'value') LIKE '%\%' THEN replace(p_charge_config::jsonb ->> 'value', '%', '')::NUMERIC END;
$$ LANGUAGE sql IMMUTABLE;

DROP TABLE IF EXISTS city_tax;

CREATE TEMP TABLE city_tax AS
SELECT tc.merchant_operating_city_id AS city_id,
       COALESCE(tc.tax_config::jsonb, '{}'::jsonb) AS tax_config,
       COALESCE(tc.tax_config::jsonb ->> 'taxRegime',
         CASE WHEN (tc.tax_config::jsonb ->> 'serviceVatPercentage') IS NOT NULL
                OR EXISTS (
                     SELECT 1
                       FROM atlas_driver_offer_bpp.fare_product fp
                       JOIN atlas_driver_offer_bpp.fare_policy p ON p.id = fp.fare_policy_id
                      WHERE fp.merchant_operating_city_id = tc.merchant_operating_city_id
                        AND p.vat_charge_config IS NOT NULL)
              THEN 'VAT' ELSE 'GST' END) AS regime
  FROM atlas_driver_offer_bpp.transporter_config tc;

UPDATE atlas_driver_offer_bpp.transporter_config tc
   SET tax_config = jsonb_set(COALESCE(tc.tax_config::jsonb, '{}'::jsonb), '{taxRegime}', to_jsonb(ct.regime))::json
  FROM city_tax ct
 WHERE tc.merchant_operating_city_id = ct.city_id
   AND (tc.tax_config IS NULL OR NOT (tc.tax_config::jsonb ? 'taxRegime'));

WITH policy_city AS (
  SELECT DISTINCT ON (p.id)
         p.id,
         p.vat_charge_config,
         p.toll_tax_charge_config,
         ct.regime,
         ct.tax_config
    FROM atlas_driver_offer_bpp.fare_policy p
    JOIN atlas_driver_offer_bpp.fare_product fp ON fp.fare_policy_id = p.id
    JOIN city_tax ct ON ct.city_id = fp.merchant_operating_city_id
   ORDER BY p.id
)
UPDATE atlas_driver_offer_bpp.fare_policy p
   SET ride_tax_config = COALESCE(p.ride_tax_config,
         CASE WHEN pc.regime = 'VAT' THEN pg_temp.vat_tax(pg_temp.charge_pct(pc.vat_charge_config)) END::text),
       toll_tax_config = COALESCE(p.toll_tax_config,
         CASE WHEN pc.regime = 'VAT' THEN pg_temp.vat_tax(pg_temp.charge_pct(pc.toll_tax_charge_config)) END::text),
       parking_tax_config = COALESCE(p.parking_tax_config,
         CASE WHEN pc.regime = 'VAT' AND (pc.vat_charge_config::jsonb -> 'appliesOn') ? 'ParkingChargeComponent'
              THEN pg_temp.vat_tax(pg_temp.charge_pct(pc.vat_charge_config))
         END::text),
       commission_tax_config = COALESCE(p.commission_tax_config,
         CASE WHEN pc.regime = 'VAT' THEN pg_temp.vat_tax((pc.tax_config ->> 'commissionVatPercentage')::NUMERIC)
              ELSE pg_temp.gst_split_tax((pc.tax_config ->> 'commissionVatPercentage')::NUMERIC, pc.tax_config -> 'rideGst')
         END::text)
  FROM policy_city pc
 WHERE pc.id = p.id;

UPDATE atlas_driver_offer_bpp.cancellation_consequence_matrix m
   SET customer_commission_and_tax = (
         m.customer_commission_and_tax::jsonb
         || jsonb_strip_nulls(jsonb_build_object('commissionTax',
              CASE WHEN ct.regime = 'VAT' THEN pg_temp.vat_tax((ct.tax_config ->> 'commissionVatPercentage')::NUMERIC)
                   ELSE pg_temp.gst_split_tax((ct.tax_config ->> 'commissionVatPercentage')::NUMERIC, ct.tax_config -> 'rideGst')
              END))
       )::text
  FROM city_tax ct
 WHERE m.merchant_operating_city_id = ct.city_id
   AND m.customer_commission_and_tax IS NOT NULL
   AND NOT (m.customer_commission_and_tax::jsonb ? 'commissionTax');

DROP TABLE IF EXISTS city_tax;
