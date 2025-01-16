ALTER TABLE atlas_app.issue_category
ADD COLUMN allowed_ride_statuses text[];

ALTER TABLE atlas_app.issue_option
ADD COLUMN restricted_ride_statuses text[] DEFAULT '{}';

UPDATE atlas_app.issue_config ic
SET message_transformation_config = JSONB_SET(
    ic.message_transformation_config::jsonb,
    '{merchantNameWTranslations}',
    COALESCE(
        CASE m.short_id
            WHEN 'NAMMA_YATRI' THEN
                JSONB_BUILD_ARRAY(
                    JSONB_BUILD_OBJECT('language', 'ENGLISH', 'translation', 'Namma Yatri'),
                    JSONB_BUILD_OBJECT('language', 'HINDI', 'translation', 'नम्मा यात्री'),
                    JSONB_BUILD_OBJECT('language', 'KANNADA', 'translation', 'ನಮ್ಮ ಯಾತ್ರೆ'),
                    JSONB_BUILD_OBJECT('language', 'TAMIL', 'translation', 'நம்ம யாத்ரி'),
                    JSONB_BUILD_OBJECT('language', 'MALAYALAM', 'translation', 'നമ്മ യാത്രി'),
                    JSONB_BUILD_OBJECT('language', 'BENGALI', 'translation', 'নামা যাত্রি'),
                    JSONB_BUILD_OBJECT('language', 'FRENCH', 'translation', 'Namma Yatri'),
                    JSONB_BUILD_OBJECT('language', 'TELUGU', 'translation', 'నమ్మ యాత్రి')
                )
            WHEN 'JATRI_SAATHI' THEN
                JSONB_BUILD_ARRAY(
                    JSONB_BUILD_OBJECT('language', 'ENGLISH', 'translation', 'Jatri Saathi'),
                    JSONB_BUILD_OBJECT('language', 'HINDI', 'translation', 'जात्रि साथी'),
                    JSONB_BUILD_OBJECT('language', 'KANNADA', 'translation', 'ಜಾತ್ರಿ ಸಾಥಿ'),
                    JSONB_BUILD_OBJECT('language', 'TAMIL', 'translation', 'ஜாத்ரி சாத்தி'),
                    JSONB_BUILD_OBJECT('language', 'MALAYALAM', 'translation', 'ജാത്രി സാഥി'),
                    JSONB_BUILD_OBJECT('language', 'BENGALI', 'translation', 'যাত্রি সাথী'),
                    JSONB_BUILD_OBJECT('language', 'FRENCH', 'translation', 'Jatri Saathi'),
                    JSONB_BUILD_OBJECT('language', 'TELUGU', 'translation', 'జాత్రి సాథి')
                )
            WHEN 'BRIDGE_CABS' THEN
                JSONB_BUILD_ARRAY(
                    JSONB_BUILD_OBJECT('language', 'ENGLISH', 'translation', 'Bridge Cabs'),
                    JSONB_BUILD_OBJECT('language', 'HINDI', 'translation', 'ब्रिज कैब्स'),
                    JSONB_BUILD_OBJECT('language', 'KANNADA', 'translation', 'ಬ್ರಿಜ್ ಕ್ಯಾಬ್ಸ್'),
                    JSONB_BUILD_OBJECT('language', 'TAMIL', 'translation', 'பிரிட்ஜ் காப்ஸ்'),
                    JSONB_BUILD_OBJECT('language', 'MALAYALAM', 'translation', 'ബ്രിഡ്ജ് കാബ്സ്'),
                    JSONB_BUILD_OBJECT('language', 'BENGALI', 'translation', 'ব্রিজ ক্যাবস'),
                    JSONB_BUILD_OBJECT('language', 'FRENCH', 'translation', 'Bridge Cabs'),
                    JSONB_BUILD_OBJECT('language', 'TELUGU', 'translation', 'బ్రిడ్జ్ క్యాబ్స్')
                )
            ELSE
                JSONB_BUILD_ARRAY(
                    JSONB_BUILD_OBJECT('language', 'ENGLISH', 'translation', 'Namma Yatri'),
                    JSONB_BUILD_OBJECT('language', 'HINDI', 'translation', 'नम्मा यात्री'),
                    JSONB_BUILD_OBJECT('language', 'KANNADA', 'translation', 'ನಮ್ಮ ಯಾತ್ರೆ'),
                    JSONB_BUILD_OBJECT('language', 'TAMIL', 'translation', 'நம்ம யாத்ரி'),
                    JSONB_BUILD_OBJECT('language', 'MALAYALAM', 'translation', 'നമ്മ യാത്രി'),
                    JSONB_BUILD_OBJECT('language', 'BENGALI', 'translation', 'নামা যাত্রি'),
                    JSONB_BUILD_OBJECT('language', 'FRENCH', 'translation', 'Namma Yatri'),
                    JSONB_BUILD_OBJECT('language', 'TELUGU', 'translation', 'నమ్మ యాత్రి')
                )
        END,
        '[]'::jsonb
    )
)
FROM atlas_app.merchant m
WHERE ic.merchant_id = m.id;

UPDATE atlas_app.issue_config ic
SET message_transformation_config = JSONB_SET(
    ic.message_transformation_config::jsonb,
    '{merchantName}',
    '"Namma Yatri"',
    true
)
FROM atlas_app.merchant m
WHERE ic.merchant_id = m.id
  AND m.short_id IN ('MOBILITY_PAYTM', 'MOBILITY_PASSCULTURE', 'MOBILITY_REDBUS');