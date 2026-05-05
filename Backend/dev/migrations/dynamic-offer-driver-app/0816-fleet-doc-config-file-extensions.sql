-- Add supported_file_extensions and is_approval_supported columns to fleet_owner_document_verification_config

-- Set default allowed extensions for existing document types
UPDATE atlas_driver_offer_bpp.fleet_owner_document_verification_config
SET supported_file_extensions = '{JPG,JPEG,PNG,PDF,WEBP}'
WHERE document_type IN ('UDYAMCertificate', 'TANCertificate', 'LDCCertificate', 'PanCard', 'GSTCertificate');
