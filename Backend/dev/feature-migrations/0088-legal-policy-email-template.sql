INSERT INTO atlas_app.email_template (
    id,
    merchant_id,
    merchant_operating_city_id,
    template_key,
    language,
    subject_template,
    html_template,
    config,
    created_at,
    updated_at
)
SELECT
    gen_random_uuid()::text,
    moc.merchant_id,
    moc.id,
    'LEGAL_POLICY_UPDATE',
    'ENGLISH',
    'Update to our {#policyName#} (v{#version#})',
    '<!DOCTYPE html><html><body style="font-family:Arial,sans-serif;max-width:600px;margin:auto;color:#111;">'
    || '<h2 style="color:#2563eb;">Policy update</h2><p>Hi,</p>'
    || '<p>We have updated our <strong>{#policyName#}</strong>.</p>'
    || '<p>Version: <strong>{#version#}</strong></p>'
    || '<p style="background:#fef3c7;padding:10px;border-left:4px solid #f59e0b;{#mandatoryStyle#}">This update is mandatory. Continuing to use our services implies acceptance.</p>'
    || '<p style="{#optionalStyle#}">Please review when convenient.</p>'
    || '<p><a href="{#url#}" style="display:inline-block;padding:10px 20px;background:#2563eb;color:#fff;text-decoration:none;border-radius:4px;">View the full document</a></p>'
    || '<p style="color:#666;font-size:13px;">Or copy this link: <a href="{#url#}">{#url#}</a></p>'
    || '<hr style="border:none;border-top:1px solid #e5e7eb;margin:24px 0;"/><p style="color:#666;font-size:12px;">Namma Yatri</p></body></html>',
    '{"fromEmail": "noreply@moving.tech", "policyTypeLabels": {"TERMS_OF_SERVICE": "Terms of Service", "PRIVACY_POLICY": "Privacy Policy", "COOKIE_POLICY": "Cookie Policy", "DRIVER_AGREEMENT": "Driver Agreement", "CONSENT_FORM": "Consent Form"}}'::json,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
ON CONFLICT (language, merchant_operating_city_id, template_key) DO NOTHING;
