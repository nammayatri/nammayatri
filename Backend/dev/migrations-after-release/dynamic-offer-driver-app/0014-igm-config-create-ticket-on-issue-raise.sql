ALTER TABLE atlas_driver_offer_bpp.igm_config ADD COLUMN IF NOT EXISTS create_ticket_on_issue_raise boolean DEFAULT true;

UPDATE atlas_driver_offer_bpp.igm_config SET create_ticket_on_issue_raise = false
WHERE merchant_id IN (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER');
