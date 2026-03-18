-- AI Voice Update feature: Voice notification templates
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.voice_template (
    id                     VARCHAR(36) PRIMARY KEY,
    template_type          VARCHAR(30) NOT NULL,
    language               VARCHAR(10) NOT NULL,
    template_text          TEXT NOT NULL,
    tts_provider           VARCHAR(20) NOT NULL DEFAULT 'GOOGLE',
    voice_name             VARCHAR(50),
    speech_rate            DOUBLE PRECISION DEFAULT 1.0,
    is_active              BOOLEAN NOT NULL DEFAULT TRUE,
    merchant_id            VARCHAR(36) NOT NULL,
    merchant_operating_city_id VARCHAR(36),
    created_at             TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    updated_at             TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_voice_template_type_lang
    ON atlas_driver_offer_bpp.voice_template(template_type, language, is_active);

-- Seed some default templates
INSERT INTO atlas_driver_offer_bpp.voice_template (id, template_type, language, template_text, tts_provider, merchant_id)
VALUES
    ('vt-ride-req-en', 'RIDE_REQUEST', 'en', 'New ride request from {{pickup}} to {{drop}}. Estimated fare {{fare}} rupees.', 'GOOGLE', 'default-merchant'),
    ('vt-ride-req-hi', 'RIDE_REQUEST', 'hi', '{{pickup}} se {{drop}} tak nayi sawari ki request. Anumani kiraya {{fare}} rupaye.', 'GOOGLE', 'default-merchant'),
    ('vt-ride-req-kn', 'RIDE_REQUEST', 'kn', '{{pickup}} inda {{drop}} ge hosa savari viniyu. Andaaji bhada {{fare}} rupaayi.', 'GOOGLE', 'default-merchant'),
    ('vt-earn-sum-en', 'EARNINGS_SUMMARY', 'en', 'Today you completed {{rides}} rides and earned {{earnings}} rupees. {{message}}', 'GOOGLE', 'default-merchant'),
    ('vt-earn-sum-hi', 'EARNINGS_SUMMARY', 'hi', 'Aaj aapne {{rides}} rides karke {{earnings}} rupaye kamaye. {{message}}', 'GOOGLE', 'default-merchant'),
    ('vt-safety-en', 'SAFETY_ALERT', 'en', 'Safety reminder: {{message}}. Please drive carefully.', 'GOOGLE', 'default-merchant'),
    ('vt-safety-hi', 'SAFETY_ALERT', 'hi', 'Suraksha suchana: {{message}}. Kripya savdhaani se chalayein.', 'GOOGLE', 'default-merchant');
