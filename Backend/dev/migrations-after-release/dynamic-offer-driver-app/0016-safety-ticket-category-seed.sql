-- Seed SAFETY category + options (Police, Ambulance, Safety Team) for direct ticket creation from driver app
-- Used by POST /ticketKapture/createTicket API during safety flow
-- Lookup: category by igm_category='SAFETY', options by label='POLICE'/'AMBULANCE'/'SAFETY_TEAM'

-- Step 1: Insert SAFETY category

INSERT INTO atlas_driver_offer_bpp.issue_category
  (id, category, logo_url, priority, igm_category, merchant_id, category_type, created_at, updated_at,
   is_ride_required, is_active, merchant_operating_city_id, label, is_ticket_required)
SELECT
  md5('safety-cat-' || moc.id)::uuid::text,
  'Safety', 'https://assets.moving.tech/beckn/common/common/images/ic_ride_related.png', 30, 'SAFETY',
  moc.merchant_id, 'Category', now(), now(), false, true, moc.id, 'SAFETY', true
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE moc.merchant_short_id = 'MSIL_PARTNER'
  AND NOT EXISTS (
    SELECT 1 FROM atlas_driver_offer_bpp.issue_category
    WHERE igm_category = 'SAFETY' AND merchant_operating_city_id = moc.id
  );

-- Step 2: Insert messages for each safety option

INSERT INTO atlas_driver_offer_bpp.issue_message
  (id, category_id, option_id, merchant_operating_city_id, message, priority, label, merchant_id,
   reference_category_id, reference_option_id, media_files, message_title, message_action,
   message_type, is_active, created_at, updated_at)
SELECT
  md5('safety-msg-' || sub.code || '-' || ic.merchant_operating_city_id)::uuid::text,
  ic.id, NULL, ic.merchant_operating_city_id, sub.message_text, sub.priority, NULL, ic.merchant_id,
  NULL, NULL, '{}', sub.message_title, NULL, 'Terminal', true, now(), now()
FROM atlas_driver_offer_bpp.issue_category ic,
  (VALUES
    ('POLICE', 'Driver triggered safety alert - Police assistance requested', 'Police', 1),
    ('AMBULANCE', 'Driver triggered safety alert - Medical emergency', 'Ambulance', 2),
    ('SAFETY_TEAM', 'Driver triggered safety alert - Safety team notified', 'Safety Team', 3)
  ) AS sub(code, message_text, message_title, priority)
WHERE ic.igm_category = 'SAFETY'
  AND ic.merchant_operating_city_id IN (SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city WHERE merchant_short_id = 'MSIL_PARTNER')
  AND NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.issue_message WHERE id = md5('safety-msg-' || sub.code || '-' || ic.merchant_operating_city_id)::uuid::text);

-- Step 3: Insert options with label as lookup key (igm_sub_category left NULL since these aren't IGM protocol enums)

INSERT INTO atlas_driver_offer_bpp.issue_option
  (id, issue_category_id, option, priority, igm_sub_category, label, merchant_id, is_active, created_at, updated_at,
   merchant_operating_city_id, show_only_when_user_blocked, restricted_variants, restricted_ride_statuses,
   issue_message_id)
SELECT
  md5('safety-opt-' || sub.code || '-' || ic.merchant_operating_city_id)::uuid::text,
  ic.id, sub.option_text, sub.priority, NULL, sub.code,
  ic.merchant_id, true, now(), now(), ic.merchant_operating_city_id, false, '{}', '{}',
  md5('safety-msg-' || sub.code || '-' || ic.merchant_operating_city_id)::uuid::text
FROM atlas_driver_offer_bpp.issue_category ic,
  (VALUES
    ('POLICE', 'Police assistance requested', 1),
    ('AMBULANCE', 'Medical emergency - Ambulance requested', 2),
    ('SAFETY_TEAM', 'Safety team notified', 3)
  ) AS sub(code, option_text, priority)
WHERE ic.igm_category = 'SAFETY'
  AND ic.merchant_operating_city_id IN (SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city WHERE merchant_short_id = 'MSIL_PARTNER')
  AND NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.issue_option WHERE id = md5('safety-opt-' || sub.code || '-' || ic.merchant_operating_city_id)::uuid::text);
