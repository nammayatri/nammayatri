-- Seed IGM categories, sub-categories, and messages for OffUs mobility (TRV10) — MSIL only
-- Required for /issue API to create issue_report + Kapture ticket + driver UI display

-- Step 1: Insert IGM categories into issue_category

INSERT INTO atlas_driver_offer_bpp.issue_category
  (id, category, logo_url, priority, igm_category, merchant_id, category_type, created_at, updated_at,
   is_ride_required, is_active, merchant_operating_city_id, label, is_ticket_required)
SELECT
  md5('igm-fulfillment-' || moc.id)::uuid,
  'Ride Issues (IGM)', 'https://assets.moving.tech/beckn/common/common/images/ic_ride_related.png', 20, 'FULFILLMENT',
  moc.merchant_id, 'Category', now(), now(), true, true, moc.id, 'IGM_FULFILLMENT', true
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE moc.merchant_short_id = 'MSIL_PARTNER'
  AND NOT EXISTS (
    SELECT 1 FROM atlas_driver_offer_bpp.issue_category
    WHERE igm_category = 'FULFILLMENT' AND merchant_operating_city_id = moc.id
  );

INSERT INTO atlas_driver_offer_bpp.issue_category
  (id, category, logo_url, priority, igm_category, merchant_id, category_type, created_at, updated_at,
   is_ride_required, is_active, merchant_operating_city_id, label, is_ticket_required)
SELECT
  md5('igm-order-' || moc.id)::uuid,
  'Order Issues (IGM)', 'https://assets.moving.tech/beckn/common/common/images/ic_ride_related.png', 21, 'ORDER',
  moc.merchant_id, 'Category', now(), now(), true, true, moc.id, 'IGM_ORDER', true
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE moc.merchant_short_id = 'MSIL_PARTNER'
  AND NOT EXISTS (
    SELECT 1 FROM atlas_driver_offer_bpp.issue_category
    WHERE igm_category = 'ORDER' AND merchant_operating_city_id = moc.id
  );

INSERT INTO atlas_driver_offer_bpp.issue_category
  (id, category, logo_url, priority, igm_category, merchant_id, category_type, created_at, updated_at,
   is_ride_required, is_active, merchant_operating_city_id, label, is_ticket_required)
SELECT
  md5('igm-payment-' || moc.id)::uuid,
  'Payment Issues (IGM)', 'https://assets.moving.tech/beckn/common/common/images/ic_fare_related.png', 22, 'PAYMENT',
  moc.merchant_id, 'Category', now(), now(), false, true, moc.id, 'IGM_PAYMENT', true
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE moc.merchant_short_id = 'MSIL_PARTNER'
  AND NOT EXISTS (
    SELECT 1 FROM atlas_driver_offer_bpp.issue_category
    WHERE igm_category = 'PAYMENT' AND merchant_operating_city_id = moc.id
  );

-- Step 2: Insert issue_message for each sub-category
-- FULFILLMENT messages

INSERT INTO atlas_driver_offer_bpp.issue_message
  (id, category_id, option_id, merchant_operating_city_id, message, priority, label, merchant_id,
   reference_category_id, reference_option_id, media_files, message_title, message_action,
   message_type, is_active, created_at, updated_at)
SELECT
  md5('igm-msg-' || sub.igm_code || '-' || ic.merchant_operating_city_id)::uuid,
  ic.id, NULL, ic.merchant_operating_city_id, sub.message_text, sub.priority, NULL, ic.merchant_id,
  NULL, NULL, '{}', sub.message_title, NULL, 'Terminal', true, now(), now()
FROM atlas_driver_offer_bpp.issue_category ic,
  (VALUES
    ('FLM111', 'Driver unable to end trip', 'Fare Policy', 1),
    ('FLM112', 'Driver took a circuitous route/longer route / Driver took a different route', 'Fare Policy', 2),
    ('FLM113', 'Trip OTP is not available in the app', 'OTP', 3),
    ('FLM114', 'Trip OTP not working when paired with the Driver app', 'OTP', 4),
    ('FLM115', 'Vehicle broke down while on trip', 'Vehicle', 5)
  ) AS sub(igm_code, message_text, message_title, priority)
WHERE ic.igm_category = 'FULFILLMENT'
  AND ic.merchant_operating_city_id IN (SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city WHERE merchant_short_id = 'MSIL_PARTNER')
  AND NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.issue_message WHERE id = md5('igm-msg-' || sub.igm_code || '-' || ic.merchant_operating_city_id)::uuid);

-- ORDER messages

INSERT INTO atlas_driver_offer_bpp.issue_message
  (id, category_id, option_id, merchant_operating_city_id, message, priority, label, merchant_id,
   reference_category_id, reference_option_id, media_files, message_title, message_action,
   message_type, is_active, created_at, updated_at)
SELECT
  md5('igm-msg-ORD111-' || ic.merchant_operating_city_id)::uuid,
  ic.id, NULL, ic.merchant_operating_city_id, 'Report lost item to support', 1, NULL, ic.merchant_id,
  NULL, NULL, '{}', 'Vehicle', NULL, 'Terminal', true, now(), now()
FROM atlas_driver_offer_bpp.issue_category ic
WHERE ic.igm_category = 'ORDER'
  AND ic.merchant_operating_city_id IN (SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city WHERE merchant_short_id = 'MSIL_PARTNER')
  AND NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.issue_message WHERE id = md5('igm-msg-ORD111-' || ic.merchant_operating_city_id)::uuid);

-- PAYMENT messages

INSERT INTO atlas_driver_offer_bpp.issue_message
  (id, category_id, option_id, merchant_operating_city_id, message, priority, label, merchant_id,
   reference_category_id, reference_option_id, media_files, message_title, message_action,
   message_type, is_active, created_at, updated_at)
SELECT
  md5('igm-msg-' || sub.igm_code || '-' || ic.merchant_operating_city_id)::uuid,
  ic.id, NULL, ic.merchant_operating_city_id, sub.message_text, sub.priority, NULL, ic.merchant_id,
  NULL, NULL, '{}', sub.message_title, NULL, 'Terminal', true, now(), now()
FROM atlas_driver_offer_bpp.issue_category ic,
  (VALUES
    ('PMT111', 'Driver asked for extra fare / demanded more fare', 'Fare Policy', 1),
    ('PMT112', 'Mismatch between fare shown at the beginning and at the end of the trip', 'Fare Policy', 2),
    ('PMT113', 'Amount paid extra by mistake', 'Fare Policy', 3),
    ('PMT114', 'Driver asked me to pay for airport pickup charges/railway station pickup charges', 'Fare Policy', 4),
    ('PMT115', 'Customer refused to pay for parking fee during rental ride', 'Fare Policy', 5),
    ('PMT116', 'Customer refused to pay revised fare (for extra km and time)', 'Fare Policy', 6),
    ('PMT117', 'Driver asked to pay for toll separately while it was already computed in the initial fare', 'Toll', 7),
    ('PMT118', 'Driver refused to take the toll road despite the toll fee already being paid', 'Toll', 8),
    ('PMT119', 'Delayed payment by collector entity and concerns on delayed interest', 'Delayed payment (RSF)', 9)
  ) AS sub(igm_code, message_text, message_title, priority)
WHERE ic.igm_category = 'PAYMENT'
  AND ic.merchant_operating_city_id IN (SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city WHERE merchant_short_id = 'MSIL_PARTNER')
  AND NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.issue_message WHERE id = md5('igm-msg-' || sub.igm_code || '-' || ic.merchant_operating_city_id)::uuid);

-- Step 3: Insert issue_option for each sub-category, linked to its message
-- FULFILLMENT options

INSERT INTO atlas_driver_offer_bpp.issue_option
  (id, issue_category_id, option, priority, igm_sub_category, merchant_id, is_active, created_at, updated_at,
   merchant_operating_city_id, show_only_when_user_blocked, restricted_variants, restricted_ride_statuses,
   issue_message_id)
SELECT
  md5('igm-opt-' || sub.igm_code || '-' || ic.merchant_operating_city_id)::uuid,
  ic.id, sub.option_text, sub.priority, sub.igm_code,
  ic.merchant_id, true, now(), now(), ic.merchant_operating_city_id, false, '{}', '{}',
  md5('igm-msg-' || sub.igm_code || '-' || ic.merchant_operating_city_id)::uuid
FROM atlas_driver_offer_bpp.issue_category ic,
  (VALUES
    ('FLM111', 'Driver unable to end trip', 1),
    ('FLM112', 'Driver took a circuitous route/longer route / Driver took a different route', 2),
    ('FLM113', 'Trip OTP is not available in the app', 3),
    ('FLM114', 'Trip OTP not working when paired with the Driver app', 4),
    ('FLM115', 'Vehicle broke down while on trip', 5)
  ) AS sub(igm_code, option_text, priority)
WHERE ic.igm_category = 'FULFILLMENT'
  AND ic.merchant_operating_city_id IN (SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city WHERE merchant_short_id = 'MSIL_PARTNER')
  AND NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.issue_option WHERE id = md5('igm-opt-' || sub.igm_code || '-' || ic.merchant_operating_city_id)::uuid);

-- ORDER options

INSERT INTO atlas_driver_offer_bpp.issue_option
  (id, issue_category_id, option, priority, igm_sub_category, merchant_id, is_active, created_at, updated_at,
   merchant_operating_city_id, show_only_when_user_blocked, restricted_variants, restricted_ride_statuses,
   issue_message_id)
SELECT
  md5('igm-opt-ORD111-' || ic.merchant_operating_city_id)::uuid,
  ic.id, 'Report lost item to support', 1, 'ORD111',
  ic.merchant_id, true, now(), now(), ic.merchant_operating_city_id, false, '{}', '{}',
  md5('igm-msg-ORD111-' || ic.merchant_operating_city_id)::uuid
FROM atlas_driver_offer_bpp.issue_category ic
WHERE ic.igm_category = 'ORDER'
  AND ic.merchant_operating_city_id IN (SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city WHERE merchant_short_id = 'MSIL_PARTNER')
  AND NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.issue_option WHERE id = md5('igm-opt-ORD111-' || ic.merchant_operating_city_id)::uuid);

-- PAYMENT options

INSERT INTO atlas_driver_offer_bpp.issue_option
  (id, issue_category_id, option, priority, igm_sub_category, merchant_id, is_active, created_at, updated_at,
   merchant_operating_city_id, show_only_when_user_blocked, restricted_variants, restricted_ride_statuses,
   issue_message_id)
SELECT
  md5('igm-opt-' || sub.igm_code || '-' || ic.merchant_operating_city_id)::uuid,
  ic.id, sub.option_text, sub.priority, sub.igm_code,
  ic.merchant_id, true, now(), now(), ic.merchant_operating_city_id, false, '{}', '{}',
  md5('igm-msg-' || sub.igm_code || '-' || ic.merchant_operating_city_id)::uuid
FROM atlas_driver_offer_bpp.issue_category ic,
  (VALUES
    ('PMT111', 'Driver asked for extra fare / demanded more fare', 1),
    ('PMT112', 'Mismatch between fare shown at the beginning and at the end of the trip', 2),
    ('PMT113', 'Amount paid extra by mistake', 3),
    ('PMT114', 'Driver asked me to pay for airport pickup charges/railway station pickup charges', 4),
    ('PMT115', 'Customer refused to pay for parking fee during rental ride', 5),
    ('PMT116', 'Customer refused to pay revised fare (for extra km and time)', 6),
    ('PMT117', 'Driver asked to pay for toll separately while it was already computed in the initial fare', 7),
    ('PMT118', 'Driver refused to take the toll road despite the toll fee already being paid', 8),
    ('PMT119', 'Delayed payment by collector entity and concerns on delayed interest', 9)
  ) AS sub(igm_code, option_text, priority)
WHERE ic.igm_category = 'PAYMENT'
  AND ic.merchant_operating_city_id IN (SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city WHERE merchant_short_id = 'MSIL_PARTNER')
  AND NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.issue_option WHERE id = md5('igm-opt-' || sub.igm_code || '-' || ic.merchant_operating_city_id)::uuid);

-- Step 4: Add new columns to igm_config and insert for MSIL

ALTER TABLE atlas_driver_offer_bpp.igm_config ADD COLUMN IF NOT EXISTS respondent_name text;
ALTER TABLE atlas_driver_offer_bpp.igm_config ADD COLUMN IF NOT EXISTS respondent_phone text;
ALTER TABLE atlas_driver_offer_bpp.igm_config ADD COLUMN IF NOT EXISTS respondent_email text;
ALTER TABLE atlas_driver_offer_bpp.igm_config ADD COLUMN IF NOT EXISTS resolution_provider_name text;
ALTER TABLE atlas_driver_offer_bpp.igm_config ADD COLUMN IF NOT EXISTS resolution_provider_phone text;
ALTER TABLE atlas_driver_offer_bpp.igm_config ADD COLUMN IF NOT EXISTS resolution_provider_email text;

-- Insert igm_config for MSIL merchant (one per merchant, not per city)
-- Required by /issue and /issue_status handlers — throws "IGMConfig not found" without it
-- Contact details should be updated with actual MSIL info before production deployment

INSERT INTO atlas_driver_offer_bpp.igm_config
  (id, gro_name, gro_phone, gro_email,
   respondent_name, respondent_phone, respondent_email,
   resolution_provider_name, resolution_provider_phone, resolution_provider_email,
   merchant_id, expected_response_time, expected_resolution_time, created_at, updated_at)
SELECT
  md5('igm-config-' || m.id)::uuid,
  'MSIL GRO', '1800000000', 'gro@msil.in',
  'MSIL Respondent', '9450394140', 'respondent@msil.in',
  'MSIL Resolution Provider', '9059304940', 'resolution@msil.in',
  m.id, 3600, 86400, now(), now()
FROM atlas_driver_offer_bpp.merchant m
WHERE m.short_id = 'MSIL_PARTNER'
  AND NOT EXISTS (
    SELECT 1 FROM atlas_driver_offer_bpp.igm_config WHERE merchant_id = m.id
  );
