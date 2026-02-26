-- Restrict DrivingSchoolCertificate uploads to OPERATOR and ADMIN only.
UPDATE atlas_driver_offer_bpp.document_verification_config
SET roles_allowed_to_upload_document_text = ARRAY['OPERATOR', 'ADMIN']::text[]
WHERE document_type = 'DrivingSchoolCertificate';
