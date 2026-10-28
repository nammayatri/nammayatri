UPDATE atlas_driver_offer_bpp.igm_config SET create_ticket_on_issue_raise = true
WHERE merchant_id IN (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER');
