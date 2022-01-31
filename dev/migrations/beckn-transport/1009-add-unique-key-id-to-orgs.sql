ALTER TABLE atlas_transporter.organization ADD COLUMN unique_key_id varchar(255) NOT NULL DEFAULT 'FIXME';

UPDATE atlas_transporter.organization SET unique_key_id = 'juspay-mobility-bpp-1-key'
  WHERE short_id = 'JUSPAY.MOBILITY.PROVIDER.UAT.1';

UPDATE atlas_transporter.organization SET unique_key_id = 'juspay-mobility-bpp-1-key'
  WHERE short_id = 'another-test-cabs';

