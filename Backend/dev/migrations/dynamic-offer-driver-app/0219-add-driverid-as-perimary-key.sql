DELETE FROM atlas_driver_offer_bpp.aadhaar_verification
WHERE ctid NOT IN (
    SELECT MIN(ctid) AS min_ctid
    FROM atlas_driver_offer_bpp.aadhaar_verification
    GROUP BY driver_id
);

ALTER TABLE atlas_driver_offer_bpp.aadhaar_verification DROP column id;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_verification ADD PRIMARY KEY (driver_id);

