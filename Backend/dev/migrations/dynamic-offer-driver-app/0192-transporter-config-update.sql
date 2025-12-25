-- set true for YATRI SATHI in master and prod env
UPDATE atlas_driver_offer_bpp.transporter_config SET subscription = true where merchant_id = '';
