INSERT INTO atlas_driver_offer_bpp.bap_metadata (id, name, logo_url, domain)
SELECT w.subscriber_id, 'NammaYatri', 'https://bitbucket.juspay.net/projects/PICAF/repos/assetstore/browse/beckn/nammayatri/nammayatricommon/images/ny_ic_launcher.png', w.domain
FROM atlas_driver_offer_bpp.white_list_org w
WHERE NOT EXISTS (
    SELECT 1 FROM atlas_driver_offer_bpp.bap_metadata b WHERE b.id = w.subscriber_id
);